!  F90 (Fortran 90) written by King and Groves to fulfill Part 1 for Programming Languages
!  Does Parts A,B,C,D and G, only does pretty printing for 8 digit or less numbers

program bottles
  implicit none !Used to say that implicit type casting is now allowed

  !Note that all variable declarations must be done at the top of the program
  integer::i,numBottles
  !The (len=*) means to calculate the length of the string from its literal value
  character (len=*),parameter::line1='() line of text on the screen, () line of text.',& ! & is used to have statements wrap around
  beg1=' lines of text on the screen, ',end1=' lines of text.',beg2='Print it out, stand up and shout, ', &
  end2=' lines of text on the screen. '
  30 format(a30)! format for beg1end2, 29 characters long text
  34 format(a34)!format for beg2
    !in writes and reads the first * means to write to stdout/read from stdin, the second means to use auto-formatting
  write(*,*)"Enter number of lines to print: "
  read (*,*)numBottles
  if (numBottles == 1) then
     write(*,*)line1
  endif
  do i = 1,numBottles-1
     if (i == 1) then
        write(*,*),line1
     else
        call primeFactors(i)!This will print out all of the prime factors on the same line with the parenthesis around them
        !The (a30) is formatting a means string, 30 means it will print out a string of length 30, truncating or adding spaces
        !as need be in order to make the length 30, advance="no" means to not print out a newline character
        write(*,30,advance="no"),beg1 
        call primeFactors(i)!This will print out all of the prime factors on the same line with the parenthesis around them
        write(*,*),end1 ! Note the lack of advance="no" so it will print out a newline character
     endif
     write(*,34,advance="no"),beg2
     call primeFactors(i+1)
     write(*,30),end2 
  enddo
endprogram bottles

!Writes out the factors of n
subroutine primeFactors(n)
  implicit none
  integer::n,i,m !Note that you have to declare the type of the argument after it has already been passed in. Wacky!

  1 format(i1) ! Format for 1 digit number
  2 format(i2) ! Format for 2 digit number
  3 format(i3) ! Format for 3 digit number
  4 format(i4) ! Format for 4 digit number
  5 format(i5) ! Format for 5 digit number
  6 format(i6) ! Format for 6 digit number
  7 format(i7) ! Format for 7 digit number
  8 format(i8) ! Format for 8 digit number

  m=n
  i=2
  write(*,"(a1)",advance="no"),'('
  do while (m>=i)
     !does not support % for modular division, have to use mod(a,b) instead to get the equivalent of a%b
     if (mod(m, i)==0) then ! i is a factor of n, now begins the wacky formatting to make i print pretty
        !These next statements check how many digits i is and assigns the approriate formatting
        if (i<10) then
           write(*,1, advance="no"),i
        elseif (i<100) then
           write(*,2,advance="no"), i
        elseif (i<1000) then
           write(*,3,advance="no"), i
        elseif (i<10000) then
           write(*,4,advance="no"), i
        elseif (i<100000) then
           write(*,5,advance="no"), i
        elseif (i<1000000) then
           write(*,6,advance="no"), i
        elseif (i<10000000) then
           write(*,7,advance="no"), i
        elseif (i<100000000) then!Only support numbers up to 100,000,000 to print prettily
           write(*,8,advance="no"), i
        else!Really long digit, just give it 20 spaces, fails on all 21+ digit numbers, fair assumption I'd say
           write(*,"(i20)",advance="no"),i        
        endif
        m=m/i
        if (m>=i) then!not the last factor so print a *
           write(*,"(a1)",advance="no"), '*'
        endif
     else
        i=i+1
     end if
  end do
  write(*,"(a1)",advance="no")')'
end subroutine primeFactors
