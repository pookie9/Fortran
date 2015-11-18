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

  write(*,*)"Which function do you wish to perform on matrices? (quit(0), addition(1), subtraction(2), multiplication(3)): "
  read(*,*)func

  do while(func /= 0)
  
    if(func == 1) then
      call add_sub(1)
    elseif (func == 2) then
      call add_sub(-1)
    elseif (func == 3) then
      call multiplication()
    else
      write(*,*)"invalid function. Program Terminated"
      func = 0
    endif

    write(*,*)""  
    write(*,*)"Which function do you wish to perform on matrices? (quit(0), addition(1), subtraction(2), multiplication(3)): "
    read(*,*)func

  enddo

endprogram matrix

subroutine add_sub(op)
  
  use readMatrix
  implicit none
  integer,allocatable :: matrix1(:,:), matrix2(:,:), matrix3(:,:)
  integer::i,j,x,y,stat
  integer, intent(in)::op
 
  write(*,*)"Enter the row dimension of your matrices: "
  read(*,*)x
  write(*,*)"Enter the column dimension of your matrices: "
  read(*,*)y

  write(*,*)""
  write(*,*)"Dimensions: ",x,"x",y,"+",x,"x",y
  
  allocate( matrix1(x,y), stat=stat)
  if (stat/=0) stop 'Cannot Allocate Memory'

  allocate( matrix2(x,y), stat=stat)
  if (stat/=0) stop 'Cannot Allocate Memory'

  allocate( matrix3(x,y), stat=stat)
  if (stat/=0) stop 'Cannot Allocate Memory'

  write(*,*)""
  write(*,*)"Enter the first matrix: "
  write(*,*)""
  matrix1 = fillMatrix(x, y)

  write(*,*)""
  write(*,*)"Enter the second matrix: "
  write(*,*)""
  matrix2 = fillMatrix(x, y)
  
  do i=1, x
    do j=1, y
      if (op < 0) then
        matrix3(i,j) = matrix1(i,j) - matrix2(i,j)
      elseif (op > 0 ) then
        matrix3(i,j) = matrix1(i,j) + matrix2(i,j)
      endif
    enddo
  enddo
  
  write(*,*)""
  
  call writeMatrix(matrix1, x, y)

  write(*,*)""
  
  if (op > 0) then
    write(*,*)"+"
  elseif (op < 0) then
    write(*,*)"-" 
  endif
 
  write(*,*)""
  

  call writeMatrix(matrix2, x, y)

  write(*,*)""
  write(*,*)"="
  write(*,*)""
  call writeMatrix(matrix3, x, y)

  deallocate(matrix1)
  deallocate(matrix2)
  deallocate(matrix3)

end subroutine add_sub

subroutine multiplication

  use readMatrix
  implicit none
  integer,allocatable :: matrix1(:,:), matrix2(:,:), matrix3(:,:)
  integer::i,j,k,x,y,xy,temp,stat

  write(*,*)"Enter the row dimension of your first matrix: "
  read(*,*)x
  write(*,*)"Enter the shared value of first matrix column and second matrix row: "
  read(*,*)xy
  write(*,*)"Enter the column dimension of your second matrix: "
  read(*,*)y

  write(*,*)"Dimensions: ",x,"x",xy,"*",xy,"x",y

  allocate( matrix1(x,xy), stat=stat)
  if (stat/=0) stop 'Cannot Allocate Memory'

  allocate( matrix2(xy,y), stat=stat)
  if (stat/=0) stop 'Cannot Allocate Memory'

  allocate( matrix3(x,y), stat=stat)
  if (stat/=0) stop 'Cannot Allocate Memory'
 
  write(*,*)""
  write(*,*)"Enter the first matrix: "
  write(*,*)""
  matrix1 = fillMatrix(x, xy)
  
  write(*,*)""
  write(*,*)"Enter the second matrix: "
  write(*,*)""
  matrix2 = fillMatrix(xy, y)
  
  do i=1, x
    do j=1, y
      temp = 0
      do k=1, xy
        temp = temp + matrix1(i, k) * matrix2(k, j)
      enddo
      matrix3(i,j) = temp
    enddo
  enddo

  write(*,*)""
  
  call writeMatrix(matrix1, x, xy)
  
  write(*,*)""
  write(*,*)"*"
  write(*,*)""
  
  call writeMatrix(matrix2, xy, y)

  write(*,*)""
  write(*,*)"="
  write(*,*)""

  call writeMatrix(matrix3, x, y)
  
  deallocate(matrix1)
  deallocate(matrix2)
  deallocate(matrix3)

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
