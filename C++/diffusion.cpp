#include <cmath> 
#include<iostream>

using namespace std;

int main() {

    int maxsize;  // Volume elements, i.e., cubic divisions of the room dimension
    cout << "M_size?: ";
    cin >> maxsize;

    bool partition = false; // No partition by default
    char flag;
    cout << "Add partition? (y/n): ";
    cin >> flag;
    if (flag == 'y') {
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

    for (i = 0;i < maxsize; i++) {
        for (j = 0;j < maxsize; j++) {
            for (k = 0; k < maxsize; k++ ) {
                cube[i][j][k] = i*maxsize*maxsize + j*maxsize + k + 1.0;
            }
        }
    }

    double diffusion_coefficient = 0.175; 
    double room_dimension = 5;                      // 5 Meters
    double speed_of_gas_molecules = 250.0;          // Based on 100 g/mol gas at RT
    double timestep = (room_dimension / speed_of_gas_molecules) / maxsize; // h in seconds
    double distance_between_blocks = room_dimension / maxsize;
    double DTerm = diffusion_coefficient * timestep / (distance_between_blocks*distance_between_blocks);

    cube[0][0][0] = 1.0e21; // Initialize the first cell
    
    double time = 0.0; // To keep up with accumulated time
    double ratio = 0.0;
    
    if (partition) {
    	// Adding partition
    	int px = ceil(maxsize * 0.5) - 1; // use the lower of the median for even Msize
    	int py = ceil(maxsize * (1-0.75)) - 1; //partition height (1 - percent height)
    	// Blocks with partition will be set to -1
    	for (int j=py; j<maxsize; j++){
            for (int k=0; k<maxsize; k++){
                cube[px][j][k] = -1;
            }           
        }
    }

    do {
        for (int i=0; i<maxsize; i++) { 
            for (int j=0; j<maxsize; j++) { 
                for (int k=0; k<maxsize; k++) { 
                    for (int l=0; l<maxsize; l++) { 
                        for (int m=0; m<maxsize; m++) { 
                            for (int n=0; n<maxsize; n++) { 
                                if ((cube[i][j][k] == -1) || (cube[l][m][n] == -1)) {
                                    continue;
                                } 
                                if (    ( ( i == l )   && ( j == m )   && ( k == n+1) ) ||  
                                        ( ( i == l )   && ( j == m )   && ( k == n-1) ) ||  
                                        ( ( i == l )   && ( j == m+1 ) && ( k == n)   ) ||  
                                        ( ( i == l )   && ( j == m-1 ) && ( k == n)   ) ||  
                                        ( ( i == l+1 ) && ( j == m )   && ( k == n)   ) ||  
                                        ( ( i == l-1 ) && ( j == m )   && ( k == n)   ) ) {

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

        time = time + timestep;   

        double sumval = 0.0;
        double maxval = cube[0][0][0]; 
        double minval = cube[0][0][0];

        for (int i=0; i<maxsize; i++) { 
            for (int j=0; j<maxsize; j++) { 
                for (int k=0; k<maxsize; k++) { 
                    if (cube[i][j][k] == -1) {
                        continue;
                    }
                    maxval = max(cube[i][j][k],maxval);
                    minval = min(cube[i][j][k],minval);
                    sumval += cube[i][j][k];
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
