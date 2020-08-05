module fsparse

use iso_c_binding
use iso_fortran_env
implicit none

type sparse_graph_32
   integer(int32) :: n=0
   integer(int64) :: nnz=0
   integer(int32),allocatable :: xadj(:)
   integer(int32),allocatable :: adjncy(:)
end type sparse_graph_32

! MTMETIS 0.7.2
#if defined(USE_MTMETIS)
interface
   ! NOTE: configured with all 32bit integer
   integer(c_int) function MTMETIS_NodeND(nvtxs,xadj,adjncy,vwgt,options,perm,iperm) bind(c,name="MTMETIS_NodeND")
     use iso_c_binding
     integer(c_int32_t), intent(in) :: nvtxs
     integer(c_int32_t), intent(in) :: xadj(*)
     integer(c_int32_t), intent(in) :: adjncy(*)
     integer(c_int32_t), intent(in) :: vwgt(*)
     real(c_double), intent(in) :: options(*)
     integer(c_int32_t), intent(inout) :: perm(*)
     integer(c_int32_t), intent(inout) :: iperm(*)
   end function MTMETIS_NodeND
end interface
#endif

! METIS 5
#if defined(USE_METIS)
interface
   integer(c_int32_t) function METIS_SetDefaultOptions(opts) bind(C,name="METIS_SetDefaultOptions")
     use,intrinsic :: iso_c_binding
     integer(c_int32_t) :: opts(0:40)
   end function METIS_SetDefaultOptions
   integer(c_int32_t) function METIS_NodeND(nvtxs,xadj,adjncy,vwgt,opts,perm,iperm) bind(C,name="METIS_NodeND")
     use,intrinsic::iso_c_binding
     implicit none
     integer(c_int32_t) :: nvtxs
     integer(c_int32_t) :: xadj(*),adjncy(*),perm(*),iperm(*)
     type(c_ptr),value :: vwgt
     integer(c_int32_t) :: opts(0:40)
   end function METIS_NodeND
end interface
#endif

contains

! MT METIS 0.7.2 interface
#if defined(USE_MTMETIS)
subroutine ordering_mtmetis(graph,nthr,perm,iperm)
   type(sparse_graph_32),intent(inout) :: graph
   integer(int32),intent(in) :: nthr
   integer(int32),intent(inout) :: perm(:),iperm(:)

   real(real64),parameter :: opt_default=-huge(opt_default)
   real(real64) :: options(64)
   integer(int32) :: ret
   integer(int32),allocatable :: vwgt(:)

   allocate(vwgt(1:graph%n))
   vwgt(:) = 1
   graph%xadj(:) = graph%xadj(:) - 1
   graph%adjncy(:) = graph%adjncy(:) - 1
   options(:) = opt_default
   options(2) = max(1,nthr)
   ret = MTMETIS_NodeND(graph%n,graph%xadj,graph%adjncy,vwgt,options,perm,iperm)
   perm(:) = perm(:) + 1
   iperm(:) = iperm(:) + 1
   graph%xadj(:) = graph%xadj(:) + 1
   graph%adjncy(:) = graph%adjncy(:) + 1
end subroutine ordering_mtmetis
#endif

! METIS 5 interface
#if defined(USE_METIS)
subroutine ordering_metis(graph,nthr,perm,iperm)
   type(sparse_graph_32),intent(inout) :: graph
   integer(int32),intent(in) :: nthr
   integer(int32),intent(inout) :: perm(:),iperm(:)

   integer(int32) :: options(0:40)
   integer(int32) :: ret

   ret = METIS_SetDefaultOptions(options)
   !options(5) = 3   ! debug level
   options(17) = 1   ! Fortran index
   ret = METIS_NodeND(graph%n, graph%xadj, graph%adjncy, c_null_ptr, options, perm, iperm)
end subroutine ordering_metis
#endif

! graph tools
subroutine init_sparse_graph(graph)
   type(sparse_graph_32),intent(inout) :: graph
   graph%n = 0
   graph%nnz = 0
   if(allocated(graph%xadj)) deallocate(graph%xadj)
   if(allocated(graph%adjncy)) deallocate(graph%adjncy)
end subroutine init_sparse_graph

subroutine make_sparse_graph(n,ia,ja,graph)
   integer(int32),intent(in) :: n,ia(n+1),ja(:)
   type(sparse_graph_32),intent(inout) :: graph
   integer(int32) :: io
   call init_sparse_graph(graph)
   graph%n = n
   graph%nnz = get_graph_size(n, ia, ja)
   allocate(graph%xadj(1:graph%n+1),graph%adjncy(1:graph%nnz))
   call get_graph(n, ia, ja, graph%xadj, graph%adjncy, io)
   if(io/=0) call error_stop('get_graph failed')
end subroutine make_sparse_graph

function get_graph_size(n, ia, ja) result(gsz)
   integer(int32),intent(in) :: n,ia(n+1),ja(:)
   integer(int32) :: i,j,k,kfst,klst,ne
   integer(int64) :: gsz
   gsz = 0
   do i=1,n
      kfst = ia(i)
      klst = ia(i+1)-1
      ! number of nonzeros within a column
      ne = klst - kfst + 1
      ! count
      do k=kfst,klst
         j = ja(k)
         if(i /= j) then
            ! upper and lower elements
            gsz = gsz + 2
         end if
      end do
   end do
end function get_graph_size

subroutine get_graph(n, ia, ja, xadj, adjncy, info)
   integer(int32),intent(in)  :: n,ia(n+1),ja(:)
   integer(int32),intent(out) :: xadj(n+1)
   integer(int32),intent(out) :: adjncy(:)
   integer(int32),intent(out),optional :: info
   integer(int32) :: i,j,k,io
   integer(int32),allocatable :: cnt(:)
   ! initializations
   if(present(info)) info=0
   xadj   = 0
   adjncy = 0

   ! allocations
   allocate(cnt(1:n),stat=io)
   if(io /= 0) then
      if(present(info)) info=io
      return
   end if

   ! count up the elements
   cnt(1:n) = 0
   do i=1,n
      do k=ia(i),ia(i+1)-1
         j = ja(k)
         if(i/=j .and. j>0 .and. j<=n) then
            cnt(i) = cnt(i)+1
            cnt(j) = cnt(j)+1
         end if
      end do
   end do

   ! set xadj and clear cnt(:)
   xadj(1) = 1
   do i=1,n
      xadj(i+1) = xadj(i) + cnt(i)
      cnt(i) = 0
   end do
   ! set xadj and clear cnt(:)
   xadj(1) = 1
   do i=1,n
      xadj(i+1) = xadj(i) + cnt(i)
      cnt(i) = 0
   end do

   ! set adjncy
   do i=1,n
      do k=ia(i),ia(i+1)-1
         j = ja(k)
         if(i/=j .and. j>0 .and. j<=n) then
            adjncy(xadj(i)+cnt(i)) = j
            adjncy(xadj(j)+cnt(j)) = i
            cnt(i) = cnt(i)+1
            cnt(j) = cnt(j)+1
         end if
      end do
   end do
   deallocate(cnt)
end subroutine get_graph


! utility
subroutine get_sparse_file_info(file,bytetype,n,nnz,rev)
   character(len=*),intent(in) :: file
   integer(int32),intent(inout) :: bytetype,n,nnz,rev
   integer(int32) :: unit,io
   open(newunit=unit,file=file,form='unformatted',iostat=io)
   if(io/=0) call error_stop('file open error')
   read(unit,iostat=io) bytetype
   if(io/=0) call error_stop('invalid sparse file: bytetype')
   read(unit,iostat=io) n,nnz,rev
   if(io/=0) call error_stop('invalid sparse file: n,nnz,rev')
   close(unit)
end subroutine get_sparse_file_info

subroutine save_sym_sparse_matrix(file,n,ia,ja,a)
   character(len=*),intent(in) :: file
   integer(int32),intent(in) :: n,ia(:),ja(:)
   real(real64),intent(in) :: a(:)
   integer(int32),parameter :: bytetype=32
   integer(int32) :: i,unit,rev,io
   integer(int32) :: nnz
   rev = 0
   nnz = size(ja)
   open(newunit=unit,file=file,form='unformatted',iostat=io)
   if(io/=0) call error_stop('file open error')
   write(unit) bytetype
   write(unit) n,nnz,rev
   write(unit) ia(1:n+1)
   write(unit) ja(1:nnz)
   write(unit) a(1:nnz)
   close(unit)
end subroutine save_sym_sparse_matrix

subroutine read_sym_sparse_matrix(file,n,ia,ja,a)
   character(len=*),intent(in) :: file
   integer(int32),intent(inout) :: n,ia(:),ja(:)
   real(real64),intent(inout) :: a(:)
   integer(int32) :: bytetype
   integer(int32) :: i,unit,rev,io
   integer(int32) :: nnz
   rev = 0
   nnz = size(ja)
   open(newunit=unit,file=file,form='unformatted',iostat=io)
   if(io/=0) call error_stop('file open error')
   read(unit,iostat=io) bytetype
   if(io/=0)        call error_stop('invalid sparse file: bytetype')
   if(bytetype/=32) call error_stop('invalid bytetype /= 32')
   read(unit,iostat=io) n,nnz,rev
   if(io/=0) call error_stop('invalid sparse file: n,nnz,rev')
   read(unit,iostat=io) ia(1:n+1)
   if(io/=0) call error_stop('invalid sparse file: ia')
   read(unit,iostat=io) ja(1:nnz)
   if(io/=0) call error_stop('invalid sparse file: ja')
   read(unit,iostat=io) a(1:nnz)
   if(io/=0) call error_stop('invalid sparse file: a')
   close(unit)
end subroutine read_sym_sparse_matrix

subroutine check_permutation(n,perm,iperm)
   integer(int32),intent(in) :: n,perm(:),iperm(:)
   integer(int32) :: i
   do i=1,n
      if(perm(i)<1 .or. perm(i)>n .or. iperm(i)<1 .or. iperm(i)>n) then
         call error_stop('perm/iperm out of range')
      end if
   end do
   do i=1,n
      if(iperm(perm(i))/=i) then
         call error_stop('incorrect perm/iperm')
      end if
   end do
end subroutine check_permutation

subroutine error_stop(msg)
   character(len=*),intent(in) :: msg
   print '(A)',msg
   stop
end subroutine error_stop

end module fsparse
