!  F90 (Fortran 90) written by King and Groves to fulfill Part 2 for Programming Languages
!  Does Matrix Addition, Subtraction, and Multiplication of integer matrices

!  module defines a function
module readMatrix
  implicit none !Used to say that implicit type casting is not allowed
contains
  !  function returns a matrix of size determined by user's input
  function fillMatrix(rows, cols)
    implicit none
    !  reads in matrix dimensions passed by function call
    integer,intent(in)::rows,cols
    integer::fillMatrix(rows,cols)
    integer::i,j

    !  writes user input to appropriate matrix (double array) location)
    do i=1, rows
      do j=1, cols
        write(*,*) 'Enter matrix element ',i,j
        read *,fillMatrix(i,j)
      enddo
    enddo
  end function
end module

!  program runs as main when file is executed
program matrix
  
  implicit none
  !  declare variables at top of the program
  integer::func,x1,x2,y1,y2

  !  prompt and read in the desired function's flag from user
  write(*,*)"Which function do you wish to perform on matrices? (quit(0), addition(1), subtraction(2), multiplication(3)): "
  read(*,*)func

  !  perform desired requests from the user
  do while(func /= 0)
  
    if(func == 1) then !  if flag is 1, then call add_sub subroutine with a positive opcode
      call add_sub(1)
    elseif (func == 2) then !  if flag is 2, then call add_sub subroutine with a positive opcode
      call add_sub(-1)
    elseif (func == 3) then !  if flag is 3, then call multiplication subroutine
      call multiplication()
    else !  if flag is invalid, then terminate the program through error checking
      write(*,*)"invalid function. Program Terminated"
      func = 0
    endif

    !  prompt and read in the desired function's flag from user
    write(*,*)""  
    write(*,*)"Which function do you wish to perform on matrices? (quit(0), addition(1), subtraction(2), multiplication(3)): "
    read(*,*)func

  enddo

endprogram matrix

!  subroutine performs matrix operations of the same matrix format (two matrices of identical dimensions)
subroutine add_sub(op)
  
  use readMatrix !  include function module
  implicit none
  integer,allocatable :: matrix1(:,:), matrix2(:,:), matrix3(:,:) !  make matrix variables that are not allocated yet
  integer::i,j,x,y,stat
  integer, intent(in)::op !  read in opcode to determine whether to add or subtract the matrices
 
  !  prompt and read in dimensions
  write(*,*)"Enter the row dimension of your matrices: "
  read(*,*)x
  write(*,*)"Enter the column dimension of your matrices: "
  read(*,*)y

  !  print out dimensions in a readable format
  write(*,*)""
  write(*,*)"Dimensions: ",x,"x",y,"+",x,"x",y
  
  !  allocate double arrays to hold matrices and include error checking
  allocate( matrix1(x,y), stat=stat)
  if (stat/=0) stop 'Cannot Allocate Memory'

  allocate( matrix2(x,y), stat=stat)
  if (stat/=0) stop 'Cannot Allocate Memory'

  allocate( matrix3(x,y), stat=stat)
  if (stat/=0) stop 'Cannot Allocate Memory'

  !  prompt and build the first matrix
  write(*,*)""
  write(*,*)"Enter the first matrix: "
  write(*,*)""
  matrix1 = fillMatrix(x, y)

  !  prompt and build the second matrix
  write(*,*)""
  write(*,*)"Enter the second matrix: "
  write(*,*)""
  matrix2 = fillMatrix(x, y)
  
  !  perform either addition or subtraction depending on the opcode
  do i=1, x
    do j=1, y
      if (op < 0) then
        matrix3(i,j) = matrix1(i,j) - matrix2(i,j)
      elseif (op > 0 ) then
        matrix3(i,j) = matrix1(i,j) + matrix2(i,j)
      endif
    enddo
  enddo
  
  !  print out the matrices and their result in a human readable format
  write(*,*)""
  
  call writeMatrix(matrix1, x, y) !  call writeMatrix subroutine

  write(*,*)""
  
  !  print appropriate symbol for performed operation
  if (op > 0) then
    write(*,*)"+"
  elseif (op < 0) then
    write(*,*)"-" 
  endif
 
  write(*,*)""
  

  call writeMatrix(matrix2, x, y) !  call writeMatrix subroutine

  write(*,*)""
  write(*,*)"="
  write(*,*)""

  call writeMatrix(matrix3, x, y) ! call writeMatrix subroutine

  !  free allocated memory
  deallocate(matrix1)
  deallocate(matrix2)
  deallocate(matrix3)

end subroutine add_sub

!  subroutine performs matrix operations on matrices who share a "middle" dimension 
!  (making for valid matrix multiplication)
subroutine multiplication

  use readMatrix !  include readMatrix module
  implicit none
  integer,allocatable :: matrix1(:,:), matrix2(:,:), matrix3(:,:) !  make matrix variables that are not allocated yet
  integer::i,j,k,x,y,xy,temp,stat

  !  prompt and read in dimensions
  write(*,*)"Enter the row dimension of your first matrix: "
  read(*,*)x
  write(*,*)"Enter the shared value of first matrix column and second matrix row: "
  read(*,*)xy
  write(*,*)"Enter the column dimension of your second matrix: "
  read(*,*)y

  !  print out dimensions in a readable format
  write(*,*)"Dimensions: ",x,"x",xy,"*",xy,"x",y

  !  allocate double arrays to hold matrices and include error checking
  allocate( matrix1(x,xy), stat=stat)
  if (stat/=0) stop 'Cannot Allocate Memory'

  allocate( matrix2(xy,y), stat=stat)
  if (stat/=0) stop 'Cannot Allocate Memory'

  allocate( matrix3(x,y), stat=stat)
  if (stat/=0) stop 'Cannot Allocate Memory'
 
  !  prompt and build the first matrix
  write(*,*)""
  write(*,*)"Enter the first matrix: "
  write(*,*)""
  matrix1 = fillMatrix(x, xy)
  
  !  prompt and build the second matrix
  write(*,*)""
  write(*,*)"Enter the second matrix: "
  write(*,*)""
  matrix2 = fillMatrix(xy, y)
  
  !  perform multiplication
  do i=1, x
    do j=1, y
      temp = 0
      do k=1, xy
        temp = temp + matrix1(i, k) * matrix2(k, j)
      enddo
      matrix3(i,j) = temp
    enddo
  enddo

  !  print out the matrices and their result in a human readable format
  write(*,*)""
  
  call writeMatrix(matrix1, x, xy) !  call writeMatrix subroutine
  
  write(*,*)""
  write(*,*)"*"
  write(*,*)""
  
  call writeMatrix(matrix2, xy, y) ! call writeMatrix subroutine

  write(*,*)""
  write(*,*)"="
  write(*,*)""

  call writeMatrix(matrix3, x, y) !  call writeMatrix subroutine
  
  !  free allocated memory
  deallocate(matrix1)
  deallocate(matrix2)
  deallocate(matrix3)

end subroutine multiplication

!  subroutine prints out a matrix in a "pretty" human readable format
subroutine writeMatrix(matrix, x, y)
   
    implicit none
    integer,intent(in)::x,y !  read in matrix dimensions
    integer,intent(in)::matrix(x,y) !  read in matrix with specified dimension
    integer::j

    !  write out each row of the double array taking advantage of the way Fortran prints single arrays
    do j=1, x
        write(*,*)matrix(j,1:y)
    enddo

end subroutine writeMatrix
