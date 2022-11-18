#!/usr/bin/env python3

""""""""""""""""""""""""""""""""""
    CSC 330
    SIMPLIFIED 3D DIFFUSION MODEL
"""""""""""""""""""""""""""""""""    
from numpy import *
import math
import sys

def main():
    # Get command line arguments
    maxsize = int(sys.argv[1]) 
    flag = sys.argv[2] 

    partition = False # No partition by default
    if flag == 'y':
        partition = True # Turn on partition only if user enter 'y'
 
    cube = zeros((maxsize, maxsize, maxsize), dtype = 'double')   # Initialize a 3D-array with zeroes
    
    # Declare necessary variables
    diffusion_coefficient = 0.175 
    room_dimension = 5                       # 5 Meters
    speed_of_gas_molecules = 250.0           # Based on 100 g/mol gas at RT
    timestep = (room_dimension / speed_of_gas_molecules) / maxsize  # h in seconds
    distance_between_blocks = room_dimension / maxsize
    DTerm = diffusion_coefficient * timestep / (distance_between_blocks ** 2)

    cube [0][0][0] = 1.0e21  # Initialize the first cell
    
    time = 0.0  # To keep up with accumulated time
    ratio = 0.0
    
    # Adding the partition if flag is true
    if partition:
        px = math.ceil(maxsize * 0.5) - 1 # Use lower median for even Msize
        py = math.ceil(maxsize * (1-0.75)) - 1 # Height of partition as a percent of maxsize (1 - percent height)
        for j in range (py, maxsize):
            for k in range (0, maxsize):
                cube[px][j][k] = -1 # Mark cubes with partition with -1
    
    # Go through all blocks
    while ratio < 0.99:
        for i in range (0, maxsize):
             for j in range (0, maxsize):
                  for k in range (0, maxsize):
                       for l in range (0, maxsize):
                            for m in range (0, maxsize):
                                 for n in range (0, maxsize):
                                      # No change if partition (cube value with -1) encountered
                                      if cube[i][j][k] == -1 or cube[l][m][n] == -1:
                                          continue
                                      # Changes occur between adjacent blocks    
                                      if ((i == l and j == m and k == n+1) or  
                                         (i == l and j == m and k == n-1) or  
                                         (i == l and j == m+1 and k == n) or  
                                         (i == l and j == m-1 and k == n) or  
                                         (i == l+1 and j == m and k == n) or  
                                         (i == l-1 and j == m and k == n)) :
                                             change = (cube[i][j][k] - cube[l][m][n]) * DTerm
                                             cube[i][j][k] = cube[i][j][k] - change                                
                                             cube[l][m][n] = cube[l][m][n] + change

        time = time + timestep 
        
        # Variables to keep track of minimus, maximums and total
        sumval = 0.0
        maxval = cube[0][0][0] 
        minval = cube[0][0][0]
        
        # Find the maximum and minimum blocks to calculate the ratio
        for i in range (0, maxsize): 
            for j in range (0, maxsize):
                for k in range (0, maxsize):
                    if cube[i][j][k] == -1: # Ignore partition blocks with -1
                        continue 
                    maxval = max(cube[i][j][k],maxval)
                    minval = min(cube[i][j][k],minval)
                    sumval += cube[i][j][k] 
                    
        ratio = minval / maxval        
        #print(ratio, end="  ") 
        #print(time, "  ", cube[0][0][0], end="  ")
        #print(      "  ", cube[maxsize-1][0][0], end="  ")
        #print(      "  ", cube[maxsize-1][maxsize-1][0], end="  ")
        #print(      "  ", cube[maxsize-1][maxsize-1][maxsize-1], end="  ")
        #print(      "  ", sumval)
    
    print("Box equilibrated in ", time, " seconds of simulated time.")
    

if __name__ == '__main__':
    main()
                    
