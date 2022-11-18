#!/usr2/local/julia-1.8.2/bin/julia

###### CSC 330
###### SIMPLIFIED 3D DIFFUSION MODEL

let
   # Read command line arguments
   maxsize = parse(Int32, ARGS[1])
   #print("Msize?: ")
   #maxsize = parse(Int32, readline())
   #print("Add partition? (y/n): ")
   #p_flag = read(stdin, Char)
   p_flag = ARGS[2]
   partition = false
   if p_flag == "y"
      partition = true
   end

   cube = zeros(Float64, maxsize, maxsize, maxsize) # Initialize a 3D-array with zeros

   diffusion_coefficient :: Float64 = 0.175 
   room_dimension :: Float64 = 5                      # 5 Meters
   speed_of_gas_molecules :: Float64 = 250.0          # Based on 100 g/mol gas at RT

   timestep :: Float64 = (room_dimension / speed_of_gas_molecules) / maxsize   #  h in seconds
   distance_between_blocks :: Float64 = room_dimension / maxsize
   DTerm :: Float64 = diffusion_coefficient * timestep / (distance_between_blocks * distance_between_blocks)

   cube[1,1,1] = 1.0e21 # Initialize first cell
   
   # Add partition 
   if partition  # setting values of px and py with regard to the 1-based indexing
      px :: Int32 = ceil(maxsize * 0.5)
      py :: Int32 = ceil(maxsize * (1 - 0.75)) # partition height (1 - percent_height) 
      for j = py:maxsize
         for k = 1:maxsize
            cube[px,j,k] = -1
         end
      end
   end

   acctime :: Float64 = 0.0 # accumulated time
   eqratio :: Float64 = 0.0
 
   while eqratio <= 0.99
      for i = 1:maxsize
         for j = 1:maxsize
            for k = 1:maxsize
               for l = 1:maxsize
                  for m = 1:maxsize
                     for n = 1:maxsize
                        if cube[k,j,i] == -1 || cube[n,m,l] == -1
                           continue
                        end
                        if (i == l && j == m && k == n+1) ||  
                           (i == l && j == m && k == n-1) ||  
                           (i == l && j == m+1 && k == n) ||  
                           (i == l && j == m-1 && k == n) ||  
                           (i == l+1 && j == m && k == n) ||  
                           (i == l-1 && j == m && k == n) 
                              change :: Float64 = (cube[k,j,i] - cube[n,m,l]) * DTerm
                              cube[k,j,i] = cube[k,j,i] - change                                
                              cube[n,m,l] = cube[n,m,l] + change
                        end
                     end
                  end
               end
            end
         end
      end

      acctime = acctime + timestep

      sumval :: Float64 = 0.0
      maxval :: Float64 = cube[1,1,1]
      minval :: Float64 = cube[1,1,1]

      for i = 1:maxsize
         for j = 1:maxsize
            for k = 1:maxsize
               if cube[k,j,i] == -1
                  continue
               end
               maxval = max(cube[k,j,i], maxval)
               minval = min(cube[k,j,i], minval)
               sumval = sumval + cube[k,j,i]
            end
         end
      end

      eqratio = minval / maxval
   
      #println(eqratio, "  ", acctime, "  ",
      #        cube[maxsize, 1, 1], "  ", cube[maxsize, maxsize, 1],
      #        cube[maxsize, maxsize, maxsize], "  ", sumval)
      
   end  # end of while loop
   
   println("Box equilibrated in ", acctime, " seconds of simulated time.")

end  # end of let

exit()
