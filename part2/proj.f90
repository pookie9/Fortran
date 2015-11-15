!  F90 (Fortran 90) written by King and Groves to fulfill Part 1 for Programming Languages
!  Does Parts A,B,C,D and G, only does pretty printing for 8 digit or less numbers

program bottles
  implicit none !Used to say that implicit type casting is now allowed

  !Note that all variable declarations must be done at the top of the program
  integer::i,j,numWrong=0,numRight=0,size,randNum,dicLength=64117,start
  character(20)::word
  character(1)::guess
  character(12)::command
  character,dimension(26)::alphabet
  character, dimension(40)::guessed!Where the user's guesses and blanks will be
  alphabet=(/'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z' /)
  call srand(time())
  call srand(floor(time()*rand(floor(rand()*time()))))!Random seeding, the srand isn't very good so this produces better randomness
  randNum=floor(rand()*dicLength)+1!Which line in the dictionary is the word
  open(1, file='american-english.txt', status='old') 
  do i=1,randNum!Reading a randNum of lines
     read(1,*) word
  end do
  word=trim(adjustl(word))
  size=verify(word,'abcdefghijklmnopqrstuvwxyz')-1!Gets the actual size of the word
  do i=1,size*2,2
     guessed(i)='_'
     guessed(i+1)=' '
  end do
  do i=size*2+1,40
     guessed(i)=' '
  end do
  do while(numWrong<7)!Only allow 7 wrong guesses before game is over
     write(command,"(A11,I1)") "cat hangman",numWrong!Which ascii art hangman to display
     call system(command)!Exectuing the cat command
     write (*,*) alphabet
     write(*,*) guessed
     do while(1==1)
        write(*,*) "Enter guess:"
        read(*,*) guess!If the user enters more than one letter at a time it will just use the first letter
        if (alphabet(1+ichar(guess)-ichar('a'))==' ')then
           write(*,*)"Already guessed that."
        else
           exit
        endif
     end do
     alphabet(1+ichar(guess)-ichar('a'))=' ' !Removing the guess from the alphabet
     if (index(word,guess)==0) then!Not found
        numWrong=numWrong+1
        write(*,*) "Incorrect Guess."
     else
        do j=0,size!Reveals each occurrence of guess
           if (word(j:j) .eq. guess) then
              guessed(j*2-1)=guess
              numRight=numRight+1
           endif
        end do
     endif
     if (numRight==size) then
        exit
     end if
  end do
  if (numWrong==7) then
     call system("cat hangman7")
     write(*,*) "You lost. Word is: ", word
  else
     write(*,*) "You won! Word is: ",word
  end if

endprogram bottles
