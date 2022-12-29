program eratosthenes
    implicit none
    
    integer,parameter :: pmax = 100000000
    integer :: l
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
    do l=1 , int(log(sqrt(real(pmax)))/log(2.0)+1)
        !$omp parallel do
        do i=lshift(1, l) , min(lshift(2, l), int(sqrt(real(pmax))))
            if(sieve(i))then
                do j = i , pmax/i
                    sieve(j*i) = .false.
                end do
            end if
        end do
        !$omp end parallel do
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
