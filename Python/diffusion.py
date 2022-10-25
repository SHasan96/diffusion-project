from numpy import *

def main():
    maxsize = int(input("M_size?: ")
    # To do: partition
    # maxsize = 10  # Volume elements, i.e., divisions of the room dimension
    cube = zeros((maxsize, maxsize, maxsize), dtype = 'double')   # Initialize array an array with zeroes

    diffusion_coefficient = 0.175 
    room_dimension = 5                       # 5 Meters
    speed_of_gas_molecules = 250.0           # Based on 100 g/mol gas at RT
    timestep = (room_dimension / speed_of_gas_molecules) / maxsize  # h in seconds
    distance_between_blocks = room_dimension / maxsize

    DTerm = diffusion_coefficient * timestep / (distance_between_blocks ** 2)

    cube [0][0][0] = 1.0e21  # Initialize the first cell
    
    passed = 0
    time = 0.0  # To keep up with accumulated time
    ratio = 0.0
    
    while ratio < 0.99:
        for i in range (0, maxsize):
             for j in range (0, maxsize):
                  for k in range (0, maxsize):
                       for l in range (0, maxsize):
                            for m in range (0, maxsize):
                                 for n in range (0, maxsize):
                                      
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
        
        sumval = 0.0
        maxval = cube[0][0][0] 
        minval = cube[0][0][0]
        
        for i in range (0, maxsize): 
            for j in range (0, maxsize):
                for k in range (0, maxsize): 
                    maxval = max(cube[i][j][k],maxval)
                    minval = min(cube[i][j][k],minval)
                    sumval += cube[i][j][k]; 
                    
        ratio = minval / maxval
        
        print(ratio, " time = ", time) # testing ratio
        print(time, " ", cube[0][0][0], end="")
        print(      " ", cube[maxsize-1][0][0], end="")
        print(      " ", cube[maxsize-1][maxsize-1][0], end="")
        print(      " ", cube[maxsize-1][maxsize-1][maxsize-1], end="")
        print(      " ", sumval)
    
    print("Box equilibrated in ", time, " seconds of simulated time.")
    

if __name__ == '__main__':
    main()
                    
                    
                    
                      
