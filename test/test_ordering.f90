program test_ordering
   use iso_fortran_env
   use fsparse
   implicit none
   integer(int32) :: x=123456789, y=362436069, z=521288629, w=88675123
   integer(int32) :: i,j,imax32,thr,seed,info
   integer(int32) :: k,n
   integer(int64) :: nnz,cnnz,nfull
   real(real64) :: ratio,btime,etime
   integer(int32),allocatable :: ia(:)
   integer(int32),allocatable :: ja(:)
   real(real64),allocatable :: a(:)
   integer(int32) :: ordalg

   integer(int32) :: gsz,nthr
   integer(int64) :: t1,t2,trate
   real(real64) :: options(0:128),dble_null

#if defined(USE_METIS_64) || defined(USE_MTMETIS_64)
   type(sparse_graph_64) :: graph
   integer(int64) :: neq
   integer(int64),allocatable :: perm(:),iperm(:)
#else
   type(sparse_graph_32) :: graph
   integer(int32) :: neq
   integer(int32),allocatable :: perm(:),iperm(:)
#endif

   ! parameters
   call get_param(n,nnz,nthr,seed)
   neq = n
   nfull = n*(n+1)/2
   ratio = dble(nnz)/dble(nfull-neq)
   imax32 = huge(imax32)
   thr = int(imax32*ratio*1.5)
   x = x + seed
   y = y + seed
   z = z + seed
   w = w + seed

   print '(A,I14)'  ,'neq          =',n
   print '(A,I14)'  ,'nnz          =',nnz
   print '(A,I14)'  ,'full elements=',nfull
   print '(A,F14.6)','sparse ratio =',ratio
   print '(A,I14)'  ,'rng threshold=',thr
   print '(A,I14)'  ,'nthr         =',nthr
   print '(A,4I14)' ,'seed         =',x,y,z,w

   ! generate a matrix
   print *,'making a sparse matrix'
   allocate(ia(n+1),ja(nnz),a(nnz))
   ia = 0
   ja = 0
   a = 0.0
   ia(1) = 1
   k = 0
   call cpu_time(btime)
   call generate_sparse_matrix(n,nnz,ia,ja,a)
   print '(A,I8,2(A,I12))','column',n,'; nnz=',ia(n+1)-1,'; total nnz=',nnz

   ! start ordering
   allocate(perm(neq),iperm(neq))
   call system_clock(t1)
   call make_sparse_graph(n,ia,ja,graph)
   call system_clock(t2,trate)
   print *,'adjncy allocated = ',size(graph%adjncy), 'filled = ',count(graph%adjncy>0)
   print *,'graph elapsed = ',dble(t2-t1)/trate

   ! AMD
   print '(A,X,I0)','AMD',kind(perm)
   call system_clock(t1)
   call ordering_amd(graph,perm,iperm)
   call system_clock(t2,trate)
   print *,'ordering elapsed = ',dble(t2-t1)/trate
   print *,'checking perm and iperm'
   call check_permutation(neq,perm,iperm)

   ! AMDBAR
   print '(A,X,I0)','AMD BAR',kind(perm)
   call system_clock(t1)
   call ordering_amd(graph,perm,iperm, use_amdbar=.true.)
   call system_clock(t2,trate)
   print *,'ordering elapsed = ',dble(t2-t1)/trate
   print *,'checking perm and iperm'
   call check_permutation(neq,perm,iperm)

   ! METIS 5
#if defined(USE_METIS_32) || defined(USE_METIS_64)
   print '(A,X,I0)','METIS5',kind(perm)
   call system_clock(t1)
   call ordering_metis(graph,perm,iperm)
   call system_clock(t2,trate)
   print *,'ordering elapsed = ',dble(t2-t1)/trate
   print *,'checking perm and iperm'
   call check_permutation(neq,perm,iperm)
#endif

   ! MTMETIS
#if defined(USE_MTMETIS_32) || defined(USE_MTMETIS_64)
   print '(A,X,I0)','MT-METIS',kind(perm)
   call system_clock(t1)
   call ordering_mtmetis(graph,nthr,perm,iperm)
   call system_clock(t2,trate)
   print *,'ordering elapsed = ',dble(t2-t1)/trate
   print *,'checking perm and iperm'
   call check_permutation(neq,perm,iperm)
#endif

   print *,'done'
   
contains

! generate sparse matrix
subroutine generate_sparse_matrix(n,nnz,ia,ja,a)
   integer,intent(in) :: n
   integer(int64),intent(in) :: nnz
   integer,intent(inout) :: ia(:),ja(:)
   real(real64),intent(inout) :: a(:)
   integer :: k,r1,r2,i,j,kk,cnt,nnz4
   integer,allocatable :: row(:),col(:)
   allocate(row(nnz),col(nnz))
   ! reserved for diagonals
   do k=1,n
      row(k)=k
      col(k)=k
   end do
   do k=n+1,nnz
      ! off-diagonals
      ! upper A[i,j] (i<j)
      r1 = mod(genrand31(),n)+1
      r2 = mod(genrand31(),n)+1
      row(k) = min(r1,r2)
      col(k) = max(r1,r2)
   end do
   nnz4 = nnz
   call iqsort(1,nnz4,row,col)
   ! make ija
   ia(1) = 1
   cnt = 1
   do i=1,n
      kk = 0
      j = 0
      do while(cnt<=nnz)
         if(row(cnt)==i) then
            if(col(cnt)>j) then
               j = col(cnt)
               ja(ia(i)+kk) = j
               kk = kk + 1
            end if
            cnt = cnt + 1
         else
            ia(i+1) = ia(i)+kk
            exit
         end if
      end do
   end do
   ia(n+1) = ia(n)+kk
   do i=1,n
      do k=ia(i),ia(i+1)-1
         j = ja(k)
         if(i==j) then
            a(k) = dble(n)
         else
            a(k) = -1.0
         end if
      end do
   end do
   deallocate(row,col)
end subroutine generate_sparse_matrix

subroutine iqsort(top,bottom,key,val)
   integer,intent(in)    :: top,bottom
   integer,intent(inout) :: key(:),val(:)
   integer,parameter :: thr=10, ss=64
   integer :: i,j,left,right,p, x,t
   integer :: lstack(1:ss),rstack(1:ss)
   left=top
   right=bottom
   p=1
   do
      if(right-left <= thr) then
         if(p==1) exit
         p=p-1
         left =lstack(p)
         right=rstack(p)
      end if
      x=key((left+right)/2)
      i=left
      j=right
      do
         do while(key(i) < x)
            i=i+1
         end do
         do while(x < key(j))
            j=j-1
         end do
         if(i >= j) exit
         ! swap
         t=key(i); key(i)=key(j); key(j)=t
         t=val(i); val(i)=val(j); val(j)=t
         ! next
         i=i+1;    j=j-1
      end do
      if(i-left > right-j) then
         if(i-left > thr) then
            lstack(p)=left
            rstack(p)=i-1
            p=p+1
         end if
         left=j+1
      else
         if(right-j > thr) then
            lstack(p)=j+1
            rstack(p)=right
            p=p+1
         end if
         right=i-1
      end if
   end do
   call iinssort(top,bottom,key,val)
end subroutine iqsort

subroutine iinssort(top,bottom,key,val)
   integer,intent(in)    :: top,bottom
   integer,intent(inout) :: key(:),val(:)
   integer :: i,j,lb,ub,x,y
   lb=top
   ub=bottom
   do i=lb+1,ub
      x=key(i)
      y=val(i)
      j=i-1
      do while(j >= lb)
         if(key(j) > x) then
            key(j+1)=key(j)
            val(j+1)=val(j)
         else
            exit
         end if
         j=j-1
      end do
      key(j+1)=x
      val(j+1)=y
   end do
end subroutine iinssort
 
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

subroutine get_param(n,nnz,nthr,seed)
   integer(int32),intent(inout) :: n
   integer(int64),intent(inout) :: nnz
   integer(int32),intent(inout) :: nthr
   integer(int32),intent(inout) :: seed
   integer :: argc,io
   character(len=32) :: argv
   n = 1000000
   nnz = 1500000_int64
   nthr = 1
   seed = 0
   argc = command_argument_count()
   if(argc<=1) then
      print '(A)','usage: test_ordering neq nnz [nthr] [seed]'
      stop
   end if
   call get_command_argument(1,argv)
   read(argv,*,iostat=io) n
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
   if(n>nnz) then
      print *,'nnz smaller than n'
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
   if(argc>3) then
      call get_command_argument(4,argv)
      read(argv,*,iostat=io) seed
      if(io/=0) then
         print *,'Invalid seed'
         stop
      end if
   end if
end subroutine get_param

end program test_ordering
