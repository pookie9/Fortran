!  F90 (Fortran 90) written to fulfill Part 1 for Programming Languages
!  Does Parts A,B,C,D and G

program bottles
  implicit none
  integer::i,numBottles
  integer,dimension(:),allocatable::factors
  character (len=*),parameter::line1='() line of text on the screen, () line of text.', & !& is used to continue the same statement on next line
  beg1end2=' lines of text on the screen, ',end1=' lines of text.',beg2='Print it out, stand up and shout, '
  100 format (7x,'Name:', 7x, 'Id:', 1x, 'Weight:')
  read (*,*)numBottles
  print 100
  if (numBottles == 1) then
     write(*,*)line1
  endif
  do i = 1,numBottles-1
     if (i == 1) then
        write(*,*),line1
     else
        call primeFactors(i)
        write(*,"(a30)",advance="no"),beg1end2
        call primeFactors(i)
        write(*,*),end1
     end if
     write(*,"(a35)",advance="no"),beg2
     call primeFactors(i+1)
     write(*,*),beg1end2
  end do
end program bottles

!Writes out the factors of n
subroutine primeFactors(n)
  implicit none
  integer::n,i,m !Note that you have to declare the type of the argument after it has already been passed in. Wacky!
  m=n
  i=2
  write(*,"(a1)",advance="no"),'('
  do while (m>=i)
     !does not support % for modular division, have to use mod(a,b) instead to get the equivalent of a%b
     if (mod(m, i)==0) then ! i is a factor of n, now begins the wacky formatting to make i go pretty
        write(*,"(i5)",advance="no"), i
        m=m/i
     else
        i=i+1
     end if
  end do
  write(*,"(a2)",advance="no")' )'
end subroutine primeFactors
