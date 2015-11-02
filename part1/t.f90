program productDetails 
implicit none 

   character (len=15) :: name
   integer :: id 
   real :: weight
   name = 'Ardupilot'
   id = 1
   weight = 0.08
   
   print *,' The product details are' 
   100 format (7x,'Name:', 7x, 'Id:', 1x, 'Weight:')   
   print 100

   
   print 200, name, id, weight 
   200 format(1x, a, 2x, i3, 2x, f5.2) 
   
end program productDetails
