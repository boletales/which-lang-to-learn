program eratosthenes
    implicit none
    
    integer,parameter :: pmax = 100000000
    integer :: i
    integer :: j
    integer :: pcount = 1
    logical,allocatable,dimension(:) :: sieve
    integer,allocatable,dimension(:) :: primes
    allocate(sieve(pmax))
    sieve=.true.
    allocate(primes(pmax))
    primes=0
   
    print '(a)', "start"
    do i=2 , int(sqrt(real(pmax)))
        if(sieve(i))then
            !$omp parallel do
            do j = i , pmax/i
                sieve(j*i) = .false.
            end do
            !$omp end parallel do
        end if
    end do

    do i=2 , pmax
        if(sieve(i))then
            primes(pcount) = i
            pcount = pcount + 1
        end if
    end do

    print '(a,i0,a)', "found " , (pcount-1), " primes"
    print '(i0)', primes(pcount-1)

    print '(a)', "end"
end program eratosthenes
