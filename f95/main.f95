program eratosthenes
    implicit none
    integer,parameter :: pmax = 100000000
    integer :: i
    integer :: j

    logical :: sieve(pmax) = .true.
    integer :: primes(pmax) = 0
    integer :: pcount = 1
    
    print *, "start"
    do i=2 , int(sqrt(real(pmax)))
        if(sieve(i))then
            do j = i*i , pmax , i
                sieve(j) = .false.
            end do
        end if
    end do

    do i=2 , pmax
        if(sieve(i))then
            primes(pcount) = i
            pcount = pcount + 1
        end if
    end do

    print *, primes(pcount-1)

    print *, "end"
end program eratosthenes
