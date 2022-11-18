!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    CSC 330
!    SIMPLIFIED 3D DIFFUSION MODEL
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program diffusion
    implicit none
    character(len=5) :: msize 
    double precision, dimension(:,:,:), allocatable :: cube 
    double precision, parameter :: diffusion_coefficient = 0.175,  & 
                                   room_dimension = 5,             &
                                   speed_of_gas_molecules = 250.0
    double precision :: timestep,                       &
                        distance_between_blocks,        &
                        DTerm,                          &
                        time = 0.0,                     &
                        ratio = 0.0,                    &
                        change,                         &
                        sumval = 0.0,                   & 
                        minimumval, maximumval         
    integer :: maxsize, i, j, k, l, m, n, px, py, ierr
    character(len=1) :: flag
    logical :: partition = .false. ! no partition by default 
    
    ! Read the two command line arguments 
    call get_command_argument(1, msize)
    call get_command_argument(2, flag)
    ! Convert them into the required data types
    read(msize,*)maxsize

    ! Set the partition ass true if user enters only 'y'
    if (flag == 'y') then
        partition = .true.
    end if
    
    ! Allocate the array based on input maxsize
    ! Array indexing is set as 0 to maxsize-1 (to mimic arrays in C++)
    ! (By default fortran arrays are indexed starting at 1)
    allocate(cube(0:maxsize-1,0:maxsize-1,0:maxsize-1), stat=ierr)
    if (ierr /= 0) then
        print *, "Could not allocate memory - halting run."
        stop
    endif
    
    ! Set values to variables based on maxsize
    timestep = (room_dimension / speed_of_gas_molecules) / maxsize
    distance_between_blocks = room_dimension / maxsize
    DTerm = diffusion_coefficient * timestep / (distance_between_blocks*distance_between_blocks)
    
    cube = 0.0 ! Zero the cube
    
    cube(0,0,0) = 1.0e21 ! Initialize the first cell 
    
    ! Adding the partition
    if (partition) then
        px = ceiling(maxsize * 0.5) - 1  ! Use lower median for even Msize
        py = ceiling(maxsize * (1-0.75)) - 1 ! Partition height (1-percent height) 
        do j = py, maxsize-1
            do k = 0, maxsize-1
                cube(px,j,k) = -1 ! Mark partition blocks with -1 
            end do
        end do
    end if
    
    ! Loop through all blocks
    do while (ratio <= 0.99)
        do i = 0, maxsize-1
            do j = 0, maxsize-1
                do k = 0, maxsize-1
                    do l = 0, maxsize-1
                        do m = 0, maxsize-1
                            do n = 0, maxsize-1
                                ! No change if partition (a value of -1) encountered    
                                if (cube(k,j,i) == -1 .or. cube(n,m,l) == -1) then
                                    cycle ! Skip iteration 
                                end if
                                ! Change occurs between adjacent blocks 
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
        
        ! Variables to keep track of minimums, maximums and total
        sumval = 0.0
        minimumval = cube(0,0,0)
        maximumval = cube(0,0,0)
        
        ! Find the minimum and the maximum value blocks to calculate ratio
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
