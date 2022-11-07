program diffusion
    implicit none 
    double precision, dimension(:,:,:), allocatable :: cube
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
    integer :: maxsize, i, j, k, l, m, n, px, py, ierr
    character :: flag
    logical :: partition = .false. ! no partition by default 
    
    write(*, "(a)", advance="no") "M_size?: "
    read(*,*) maxsize
    write(*, "(a)", advance="no") "Add partition? (y/n): "
    read(*,*) flag
    
    if (flag == 'y') then
        partition = .true.
    end if
    
    allocate(cube(0:maxsize-1,0:maxsize-1,0:maxsize-1), stat=ierr)
    if (ierr /= 0) then
        print *, "Could not allocate memory - halting run."
        stop
    endif
    
    timestep = (room_dimension / speed_of_gas_molecules) / maxsize
    distance_between_blocks = room_dimension / maxsize
    DTerm = diffusion_coefficient * timestep / (distance_between_blocks*distance_between_blocks)
    
    cube = 0.0 ! Zero the cube
    
    cube(0,0,0) = 1.0e21 ! Initialize the first cell 
    
    if (partition) then
        ! Adding the partition
        px = ceiling(maxsize * 0.5) - 1  ! use lower median for even Msize
        py = ceiling(maxsize * (1-0.75)) - 1 ! partition height (1-percent height) 

        do j = py, maxsize-1
            do k = 0, maxsize-1
                cube(px,j,k) = -1 ! Mark partition cubes with -1 
            end do
        end do
   end if

    do while (ratio <= 0.99)
        do i = 0, maxsize-1
            do j = 0, maxsize-1
                do k = 0, maxsize-1
                    do l = 0, maxsize-1
                        do m = 0, maxsize-1
                            do n = 0, maxsize-1
                                if (cube(k,j,i) == -1 .or. cube(n,m,l) == -1) then
                                    cycle ! skip iteration
                                end if
                                if ( ( ( i == l ) .and. ( j == m ) .and. ( k == n+1) )  .or. & 
                                     ( ( i == l ) .and. ( j == m ) .and. ( k == n-1) )  .or. & 
                                     ( ( i == l ) .and. ( j == m+1 ) .and. ( k == n) )  .or. & 
                                     ( ( i == l ) .and. ( j == m-1 ) .and. ( k == n) )  .or. & 
                                     ( ( i == l+1 ) .and. ( j == m ) .and. ( k == n) )  .or. & 
                                     ( ( i == l-1 ) .and. ( j == m ) .and. ( k == n) ) ) then
                                          change = ( cube(k,j,i) - cube(n,m,l) ) * DTerm
                                          cube(k,j,i) = cube(k,j,i) - change                               
                                          cube(n,m,l) = cube(n,m,l) + change
                                end if
                            end do
                        end do
                    end do
                end do
            end do
        end do
    
        time = time + timestep
        sumval = 0.0
        minimumval = cube(0,0,0)
        maximumval = cube(0,0,0)
    
        do i = 0, maxsize-1
            do j = 0, maxsize-1
                do k = 0, maxsize-1
                    if (cube(k,j,i) == -1) then
                        cycle
                    end if
                    maximumval = max(cube(k,j,i),maximumval)
                    minimumval = min(cube(k,j,i),minimumval)
                    sumval = sumval + cube(k,j,i)       
                end do
            end do
        end do
    
        ratio = minimumval/maximumval
      
       !print *, ratio, time, cube(0,0,0), cube(maxsize-1,0,0), &
       !         cube(maxsize-1, maxsize-1,0),                  &
       !         cube(maxsize-1, maxsize-1, maxsize-1), sumval   
    end do
    deallocate(cube) ! Free array    
    print *, "Box equilibrated in ", time, " seconds of simulated time."

end program diffusion             
