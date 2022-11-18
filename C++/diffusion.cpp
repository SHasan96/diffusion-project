/*******************************
  CSC 330
  SIMPLIFIED 3D DIFFUSION MODEL
********************************/
#include<cmath> 
#include<iostream>
#include<string.h>
using namespace std;

int main(int argc, char** argv) {
   
    int const maxsize = strtol(argv[1], NULL, 10);  // This is Msize, i.e., cubic divisions of the room dimension
  
    bool partition = false; // No partition by default
    char const *flag = argv[2];
    if (*flag == 'y') { // For argument 'y' set partition as true
        partition = true;
    }
    
    // Dynamically allocating a 3-D array
    // Arrays have a default value of zero in C++
    int i, j, k;
    double*** cube =  new double** [maxsize];
    for (int i = 0; i < maxsize; ++i) {
      cube[i] = new double* [maxsize];
      for (int j = 0; j < maxsize; j++) {
          cube[i][j] = new double [maxsize];
      }
    }   

    //Declare variables (these can be constant)
    double const diffusion_coefficient = 0.175; 
    double const room_dimension = 5;                      // 5 Meters
    double const speed_of_gas_molecules = 250.0;          // Based on 100 g/mol gas at RT
    double const timestep = (room_dimension / speed_of_gas_molecules) / maxsize; // h in seconds
    double const distance_between_blocks = room_dimension / maxsize;
    double const DTerm = diffusion_coefficient * timestep / (distance_between_blocks*distance_between_blocks);

    cube[0][0][0] = 1.0e21; // Initialize the first cell
    
    double time = 0.0; // To keep up with accumulated time
    double ratio = 0.0;
    
    // Adding partition
    if (partition) {
    	int px = ceil(maxsize * 0.5) - 1; // Use the lower of the median for even Msize
    	int py = ceil(maxsize * (1-0.75)) - 1; // Partition height (1 - percent height)
    	// Blocks with partition will be set to -1
    	for (int j=py; j<maxsize; j++){
            for (int k=0; k<maxsize; k++){
                cube[px][j][k] = -1;
            }           
        }
    }
    
    // Loop through all blocks
    do {
        for (int i=0; i<maxsize; i++) { 
            for (int j=0; j<maxsize; j++) { 
                for (int k=0; k<maxsize; k++) { 
                    for (int l=0; l<maxsize; l++) { 
                        for (int m=0; m<maxsize; m++) { 
                            for (int n=0; n<maxsize; n++) { 
                                // No change if partition (a value of -1) is encountered
                                if ((cube[i][j][k] == -1) || (cube[l][m][n] == -1)) {
                                    continue;
                                }
                                // Change occurs between adjacent blocks 
                                if (((i == l) && (j == m) && (k == n+1)) ||  
                                    ((i == l) && (j == m) && (k == n-1)) ||  
                                    ((i == l) && (j == m+1) && (k == n)) ||  
                                    ((i == l) && (j == m-1) && (k == n)) ||  
                                    ((i == l+1) && (j == m) && (k == n)) ||  
                                    ((i == l-1) && (j == m) && (k == n)) ) {
                                        double change = (cube[i][j][k] - cube[l][m][n]) * DTerm;
                                        cube[i][j][k] -= change;                                
                                        cube[l][m][n] += change;                                
                                }          
                            }
                        }
                    }             
                }
            }
        }  

        time += timestep;   

        // Variables to keep track of minimums, maximums and total
        double sumval = 0.0; 
        double maxval = cube[0][0][0]; 
        double minval = cube[0][0][0];

        // Find the minimum and the maximum value blocks to calculate ratio
        for (int i=0; i<maxsize; i++) { 
            for (int j=0; j<maxsize; j++) { 
                for (int k=0; k<maxsize; k++) { 
                    if (cube[i][j][k] == -1) { // Ignore partition blocks (which have -1)
                        continue;
                    }
                    maxval = max(cube[i][j][k],maxval);
                    minval = min(cube[i][j][k],minval);
                    sumval += cube[i][j][k];  // This was used to check for conserved mass
                }
            }
        }
        ratio = minval / maxval;
        //cout <<  ratio << "\t";
        //cout << time << "\t" << cube[0][0][0];
        //cout <<         "\t" << cube[maxsize-1][0][0];
        //cout <<         "\t" << cube[maxsize-1][maxsize-1][0];
        //cout <<         "\t" << cube[maxsize-1][maxsize-1][maxsize-1];
        //cout <<         "\t" << sumval << endl;
    } while ( ratio <= 0.99 );
    
    // Free the allocated array
    for (int i=0; i<maxsize; i++ ) {
        for (int j=0; j<maxsize; j++ ) {
            delete[] cube[i][j];
        }
        delete[] cube[i];
    }
    delete[] cube;
    
    cout << "Box equilibrated in " << time << " seconds of simulated time." << endl;
} 
