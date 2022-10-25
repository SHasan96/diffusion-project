#include <cmath> 
#include<iostream>

using namespace std;

int main() {

    const int maxsize = 10; // Volume elements, i.e., divisions of the room dimension
    double cube[maxsize][maxsize][maxsize] = {}; // Initialize array, empty arrays have default values of zero

    double diffusion_coefficient = 0.175; 
    double room_dimension = 5;                      // 5 Meters
    double speed_of_gas_molecules = 250.0;          // Based on 100 g/mol gas at RT
    double timestep = (room_dimension / speed_of_gas_molecules) / maxsize; // h in seconds
    double distance_between_blocks = room_dimension / maxsize;

    double DTerm = diffusion_coefficient * timestep / (distance_between_blocks*distance_between_blocks);

    cube [0][0][0] = 1.0e21; // Initialize the first cell
    
    int pass = 0;
    double time = 0.0; // To keep up with accumulated time
    double ratio = 0.0;

    do {
        for (int i=0; i<maxsize; i++) { 
            for (int j=0; j<maxsize; j++) { 
                for (int k=0; k<maxsize; k++) { 
                    for (int l=0; l<maxsize; l++) { 
                        for (int m=0; m<maxsize; m++) { 
                            for (int n=0; n<maxsize; n++) { 

                                if (    ( ( i == l )   && ( j == m )   && ( k == n+1) ) ||  
                                        ( ( i == l )   && ( j == m )   && ( k == n-1) ) ||  
                                        ( ( i == l )   && ( j == m+1 ) && ( k == n)   ) ||  
                                        ( ( i == l )   && ( j == m-1 ) && ( k == n)   ) ||  
                                        ( ( i == l+1 ) && ( j == m )   && ( k == n)   ) ||  
                                        ( ( i == l-1 ) && ( j == m )   && ( k == n)   ) ) {

                                        double change = (cube[i][j][k] - cube[l][m][n]) * DTerm;
                                        cube[i][j][k] = cube[i][j][k] - change;                                
                                        cube[l][m][n] = cube[l][m][n] + change;                                
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
                    maxval = max(cube[i][j][k],maxval);
                    minval = min(cube[i][j][k],minval);
                    sumval += cube[i][j][k];
                }
            }
        }

        ratio = minval / maxval;

        //c.out <<  ratio << " time = " << time;
        cout << time << " " << cube[0][0][0];
        cout <<         " " << cube[maxsize-1][0][0];
        cout <<         " " << cube[maxsize-1][maxsize-1][0];
        cout <<         " " << cube[maxsize-1][maxsize-1][maxsize-1];
        cout <<         " " << sumval << endl;
    } while ( ratio <= 0.99 );

    cout << "Box equilibrated in " << time << " seconds of simulated time." << endl;
} 
