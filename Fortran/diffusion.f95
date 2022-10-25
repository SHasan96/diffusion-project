program diffusion
    implicit none
    integer, parameter :: maxsize = 10
    double precision, dimension(maxsize,maxsize,maxsize) :: cube
    double precision :: diffusion_coefficient = 0.175,  & 
                        room_dimension = 5,             &
                        speed_of_gas_molecules = 250.0, &
                        timestep,                       &
                        distance_between_blocks,        &
                        DTerm,                          &
                        time = 0.0,                     &
                        ratio = 0.0,                    &
                        change,                         &
                        sumval = 0.0,                   & 
                        minimumval, maximumval
    integer :: i, j, k, l, m, n 
    
    timestep = (room_dimension / speed_of_gas_molecules) / maxsize
    distance_between_blocks = room_dimension / maxsize
    DTerm = diffusion_coefficient * timestep / (distance_between_blocks*distance_between_blocks)
    
    cube = 0.0 ! Zero the cube
    
    cube(1,1,1) = 1.0e21 ! Initialize the first cell 

    do while (ratio <= 0.99)
        do i = 1, maxsize
            do j = 1, maxsize
                do k = 1, maxsize
                    do l = 1, maxsize
                        do m = 1, maxsize
                            do n = 1, maxsize
                                if ( ( ( i == l ) .and. ( j == m ) .and. ( k == n+1) )  .or. & 
                                     ( ( i == l ) .and. ( j == m ) .and. ( k == n-1) )  .or. & 
                                     ( ( i == l ) .and. ( j == m+1 ) .and. ( k == n) )  .or. & 
                                     ( ( i == l ) .and. ( j == m-1 ) .and. ( k == n) )  .or. & 
                                     ( ( i == l+1 ) .and. ( j == m ) .and. ( k == n) )  .or. & 
                                     ( ( i == l-1 ) .and. ( j == m ) .and. ( k == n) ) ) then
                                          change = ( cube(i,j,k) - cube(l,m,n) ) * DTerm
                                          cube(i,j,k) = cube(i,j,k) - change                               
                                          cube(l,m,n) = cube(l,m,n) + change
                                end if
                            end do
                        end do
                    end do
                end do
            end do
        end do
    
        time = time + timestep
        sumval = 0.0
        minimumval = cube(1,1,1)
        maximumval = cube(1,1,1)
    
        do i = 1, maxsize
            do j = 1, maxsize
                do k = 1, maxsize
                    maximumval = max(cube(i,j,k),maximumval)
                    minimumval = min(cube(i,j,k),minimumval)
                    sumval = sumval + cube(i,j,k)       
                end do
            end do
        end do
    
        ratio = minimumval/maximumval
      
        print *, time, cube(1,1,1), cube(maxsize-1,1,1), &
                 cube(maxsize-1, maxsize-1,1),           &
                 cube(maxsize-1, maxsize-1, maxsize-1), sumval   
    end do
    
    print *, "Box equilibrated in ", time, " seconds of simulated time."

end program diffusion             
