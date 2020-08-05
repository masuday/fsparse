program test_ordering
   use iso_fortran_env
   use fsparse
   implicit none
   integer(int32) :: x=123456789, y=362436069, z=521288629, w=88675123
   integer(int32) :: i,j,neq,imax32,thr,info
   integer(int32) :: k
   integer(int64) :: n,nnz,cnnz,nfull
   real(real64) :: ratio,btime,etime
   integer(int32),allocatable :: ia(:)
   integer(int32),allocatable :: ja(:)
   real(real64),allocatable :: a(:)
   integer(int32) :: ordalg

   type(sparse_graph_32) :: graph
   integer(int32) :: gsz,nthr
   integer(int64) :: t1,t2,trate
   real(real64) :: options(0:128),dble_null
   integer(int32),allocatable :: perm(:),iperm(:)
   
   ! parameters
   imax32 = huge(imax32)
!   neq = 1000000
!   n = neq
!   nnz = 2247483647_int64
!   nfull = n*(n+1)/2
!   ratio = dble(nnz)/dble(nfull)
!   thr = int(imax32*ratio*1.05)

   call get_param(neq,nnz,nthr)
   n = neq
   nfull = n*(n+1)/2
   ratio = dble(nnz)/dble(nfull-neq)
   thr = int(imax32*ratio*1.5)

   print '(A,I14)'  ,'neq          =',n
   print '(A,I14)'  ,'nnz          =',nnz
   print '(A,I14)'  ,'full elements=',nfull
   print '(A,F14.6)','sparse ratio =',ratio
   print '(A,I14)'  ,'rng threshold=',thr
   print '(A,I14)'  ,'nthr         =',nthr

   ! generate a matrix
   print *,'making a sparse matrix'
   allocate(ia(n+1),ja(nnz),a(nnz))
   ia(1) = 1
   k = 0
   call cpu_time(btime)
   do i=1,n
      ! diagonal
      cnnz = 1
      k = k + 1
      ja(k) = i
      a(k) = dble(n)
      if(k<nnz-n) then
         do j=i+1,n
            ! off diagonal
            if(genrand31()<thr) then
               cnnz = cnnz + 1
               k = k + 1
               ja(k) = j
               a(k) = -1.0d0
               if(k>=nnz-n) then
                  exit
               end if
            end if
         end do
      end if
      ia(i+1) = ia(i)+cnnz
      call cpu_time(etime)
      if(etime-btime>10.0) then
         print '(A,I8,2(A,I12))','column',i,'; nnz=',k,'; total nnz=',nnz
         btime = etime
      end if
   end do
   print '(A,I8,2(A,I12))','column',n,'; nnz=',k,'; total nnz=',nnz

   ! start ordering
   allocate(perm(neq),iperm(neq))
   call system_clock(t1)
   call make_sparse_graph(neq,ia,ja,graph)
   call system_clock(t2,trate)
   print *,'graph elapsed = ',dble(t2-t1)/trate

   ! MTMETIS
   !print '(A)','MT METIS'
   !call system_clock(t1)
   !call ordering_mtmetis(graph,nthr,perm,iperm)
   !call system_clock(t2,trate)
   !print *,'ordering elapsed = ',dble(t2-t1)/trate

   ! METIS 5
   print '(A)','METIS 5'
   call system_clock(t1)
   call ordering_metis(graph,nthr,perm,iperm)
   call system_clock(t2,trate)
   print *,'ordering elapsed = ',dble(t2-t1)/trate

   ! checks
   call check_permutation(neq,perm,iperm)

   print *,'done'
   
contains

! xorshift
subroutine setseed(p,q,r,s)
   integer,intent(in) :: p,q,r,s
   x = p
   y = q
   z = r
   w = s
   if(p+q+r+s <= 0) then
      print '(A)','ERROR: x+y+z+w must be >0.'
      stop
   end if
end subroutine setseed
function genrand31() result(r)
   integer :: r,t
   t = ieor(x,ishft(x,-11))
   x = y
   y = z
   z = w
   w = ieor( ieor(w,ishft(w,19)), ieor(t,ishft(t,8)) )
   r = abs(w)
end function genrand31

subroutine get_param(neq,nnz,nthr)
   integer(int32),intent(inout) :: neq
   integer(int64),intent(inout) :: nnz
   integer(int32),intent(inout) :: nthr
   integer :: argc,io
   character(len=32) :: argv
   neq = 1000000
   nnz = 1500000_int64
   nthr = 1 !ORD_METIS
   argc = command_argument_count()
   if(argc<=1) then
      print '(A)','usage: test_big_matrix2 neq nnz [nthr]'
      stop
   end if
   call get_command_argument(1,argv)
   read(argv,*,iostat=io) neq
   if(io/=0) then
      print *,'Invalid neq'
      stop
   end if
   call get_command_argument(2,argv)
   read(argv,*,iostat=io) nnz
   if(io/=0) then
      print *,'Invalid nnz'
      stop
   end if
   if(argc>2) then
      call get_command_argument(3,argv)
      read(argv,*,iostat=io) nthr
      if(io/=0) then
         print *,'Invalid nthr'
         stop
      end if
   end if
end subroutine get_param

end program test_ordering
