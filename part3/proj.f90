module readMatrix
  implicit none
contains
  function fillMatrix(rows, cols)
    implicit none
    integer,intent(in)::rows,cols
    integer::fillmatrix(rows,cols)
    integer::i,j

    do i=1, rows
      do j=1, cols
        write(*,*) 'Enter matrix element ',i,j
        read *,fillMatrix(i,j)
      enddo
    enddo
  end function
end module


program matrix
  
  implicit none
  integer::func,x1,x2,y1,y2

  write(*,*)"Which function do you wish to perform on matrices? (addition(1), subtraction(2), multiplication(3)): "

  read(*,*)func

  if(func == 1) then
    call addition()
  elseif (func == 2) then
    call subtraction()
  elseif (func == 3) then
    call multiplication()
  else
    write(*,*)"invalid function. Program Terminated"
  endif

endprogram matrix

subroutine addition
  
  use readMatrix
  implicit none
  integer,allocatable :: matrix1(:,:), matrix2(:,:)
  integer::x,y,stat

  write(*,*)"Enter the row dimension of your matrices: "
  read(*,*)x
  write(*,*)"Enter the column dimension of your matrices: "
  read(*,*)y

  write(*,*)x
  write(*,*)y
  
  allocate( matrix1(x,y), stat=stat)
  if (stat/=0) stop 'Cannot Allocate Memory'

  allocate( matrix2(x,y), stat=stat)
  if (stat/=0) stop 'Cannot Allocate Memory'
  
  matrix1 = fillMatrix(x, y)
  matrix2 = fillMatrix(x, y)

  call writeMatrix(matrix1, x, y)
  call writeMatrix(matrix2, x, y)

  deallocate(matrix1)
  deallocate(matrix2)

end subroutine addition

subroutine subtraction
  
  use readMatrix
  implicit none
  integer,allocatable :: matrix1(:,:), matrix2(:,:), matrix3(:,:)
  integer::i,j,x,y,stat
 
  write(*,*)"Enter the row dimension of your matrices: "
  read(*,*)x
  write(*,*)"Enter the column dimension of your matrices: "
  read(*,*)y

  write(*,*)x
  write(*,*)y
  
  allocate( matrix1(x,y), stat=stat)
  if (stat/=0) stop 'Cannot Allocate Memory'

  allocate( matrix2(x,y), stat=stat)
  if (stat/=0) stop 'Cannot Allocate Memory'

  allocate( matrix3(x,y), stat=stat)
  if (stat/=0) stop 'Cannot Allocate Memory'
  
  write(*,*)""
  write(*,*)"Enter the first matrix: "
  matrix1 = fillMatrix(x, y)

  write(*,*)""
  write(*,*)"Enter the second matrix: "
  matrix2 = fillMatrix(x, y)
  
  do i=1, x
    do j=1, y
      matrix3(i,j) = matrix1(i,j) - matrix2(i,j)
    enddo
  enddo
  
  write(*,*)""
  call writeMatrix(matrix1, x, y)

  write(*,*)""
  write(*,*)"-"
  write(*,*)""

  call writeMatrix(matrix2, x, y)

  write(*,*)""
  write(*,*)"="
  write(*,*)""

  call writeMatrix(matrix3, x, y)

  deallocate(matrix1)
  deallocate(matrix2)
  deallocate(matrix3)

end subroutine subtraction

subroutine multiplication

  use readMatrix
  implicit none
  integer,allocatable :: matrix1(:,:), matrix2(:,:)
  integer::x,y,xy,stat

  write(*,*)"Enter the row dimension of your first matrix: "
  read(*,*)x
  write(*,*)"Enter the shared value of first matrix column and second matrix row: "
  read(*,*)xy
  write(*,*)"Enter the column dimension of your second matrix: "
  read(*,*)y

  write(*,*)x
  write(*,*)xy
  write(*,*)y

  allocate( matrix1(x,xy), stat=stat)
  if (stat/=0) stop 'Cannot Allocate Memory'

  allocate( matrix2(xy,y), stat=stat)
  if (stat/=0) stop 'Cannot Allocate Memory'

  matrix1 = fillMatrix(x, xy)
  matrix2 = fillMatrix(xy, y)

  call writeMatrix(matrix1, x, xy)
  call writeMatrix(matrix2, xy, y)

  deallocate(matrix1)
  deallocate(matrix2)

end subroutine multiplication

subroutine writeMatrix(matrix, x, y)
   
    implicit none
    integer,intent(in)::x,y
    integer,intent(in)::matrix(x,y)
    integer::j

    do j=1, x
        write(*,*)matrix(j,1:y)
    enddo

end subroutine writeMatrix
