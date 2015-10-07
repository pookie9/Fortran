!  F90 (Fortran 90) written by King and Groves to fulfill Part 1 for Programming Languages
!  Does Parts A,B,C,D and G, sorry the formatting is ugly, you literally cannot format well in Fortran, I spent three hours trying to

program bottles
  implicit none !Used to say that implicit type casting is now allowed

  !Note that all variable declarations must be done at the top of the program
  integer::i,numBottles
  !The (len=*) means to calculate the length of the string from its literal value
  character (len=*),parameter::line1='() line of text on the screen, () line of text.',& ! & is used to have statements wrap around
 beg1end2=' lines of text on the screen, ',end1=' lines of text.',beg2='Print it out, stand up and shout, '

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
        write(*,"(a30)",advance="no"),beg1end2 
        call primeFactors(i)!This will print out all of the prime factors on the same line with the parenthesis around them
        write(*,*),end1
     endif
     write(*,"(a35)",advance="no"),beg2
     call primeFactors(i+1)
     write(*,*),beg1end2 ! Note the lack of advance="no" so it will print out a newline character
  enddo
endprogram bottles

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
        !the i means to print an integer, the 5 means 5 digits, if it goes over 5 then it will just print **, 
        !otherwise it will pad with spaces, so if you give it a number with a factor > 5 digits, this won't work
        !But it still looks stupid this way because if often has 4 extra spaces, there is just no way to do pretty formatting
        !in Fortran, it is literally not possible
        write(*,"(i5)",advance="no"),i
        write(*,"(a1)",advance="no"), '*'
        m=m/i
     else
        i=i+1
     end if
  end do
  write(*,"(a2)",advance="no")' )'
end subroutine primeFactors
