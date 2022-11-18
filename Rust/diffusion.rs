////////////////////////////////////
//  CSC 330 
//  SIMPLIFIED 3D DIFFUSION MODEL
////////////////////////////////////
use std::env;
fn main() {
    // Get values from command line arguments
    let args: Vec<_> = env::args().collect();
    let maxsize: i32 = args[1].to_string().parse::<i32>().unwrap();   
    let p_flag = args[2].to_string();

    let mut partition: bool = false; // Partition off by default
    if p_flag.trim() == "y" { // Partition turns on if user inputs "y"
       partition = true;
    }     
    
    // Create a 3D-vector (which are dynamic) with initial values of zero
    let mut cube = vec![vec![vec![0.0f64; maxsize as usize]; maxsize as usize]; maxsize as usize];

    // Declare variables (these don't need to be mutable)
    let diffusion_coefficient: f64 = 0.175; 
    let room_dimension: f64 = 5.0;                      // 5 Meters
    let speed_of_gas_molecules: f64 = 250.0;            // Based on 100 g/mol gas at RT
    let timestep: f64 = (room_dimension / speed_of_gas_molecules) / (maxsize as f64); // h in seconds
    let distance_between_blocks = room_dimension / (maxsize as f64);
    let d_term = diffusion_coefficient * timestep / (distance_between_blocks*distance_between_blocks);   

    cube[0][0][0] = 1.0e21; // Initialize the first cell      
 
    // Add partition if user asked for it
    if partition {
       let px: i32 = ((maxsize as f64 * 0.5).ceil() as i32) - 1; // Use the lower of the median for even Msize
       let py: i32 = ((maxsize as f64 * (1.0 - 0.75)).ceil() as i32) - 1; // Partition height (1 - percent height)    
       for j in py..maxsize {
          for k in 0..maxsize {
             cube[px as usize][j as usize][k as usize] = -1.0;
          }
       }
    }

    let mut time: f64 = 0.0; // To keep up with accumulated time
    let mut eqratio: f64 = 0.0;
    // Go through all blocks 
    while eqratio <= 0.99 {
       for i in 0..maxsize { // Note: the ending of the range (maxsize) is exclusive
          for j in 0..maxsize {
             for k in 0..maxsize {
                for l in 0..maxsize {
                   for m in 0..maxsize {
                      for n in 0..maxsize {
                         // No change if a partition block (containing -1.0) is encountered
                         if cube[i as usize][j as usize][k as usize] == -1.0 ||
                            cube[l as usize][m as usize][n as usize] == -1.0 {
                               continue;
                         }
                         // Change occurs between adjacent blocks
                         if ((i == l) && (j == m) && (k == n+1)) ||  
                            ((i == l) && (j == m) && (k == n-1)) ||  
                            ((i == l) && (j == m+1) && (k == n)) ||  
                            ((i == l) && (j == m-1) && (k == n)) ||  
                            ((i == l+1) && (j == m) && (k == n)) ||  
                            ((i == l-1) && (j == m) && (k == n)) {
                               let change: f64 = (cube[i as usize][j as usize][k as usize] 
                                         - cube[l as usize][m as usize][n as usize]) * d_term;
                               cube[i as usize][j as usize][k as usize] -=  change;                                
                               cube[l as usize][m as usize][n as usize] +=  change;  
                          }
                       }
                    }
                 }
              }
           }
        }
        time += timestep;   
        // Variables to keep track of minimums, maximums and total
        let mut sumval: f64 = 0.0; // To keep track of conserved mass (not needed for ratio calculations)
        let mut maxval: f64 = cube[0][0][0]; 
        let mut minval: f64 = cube[0][0][0];
        // Find the minimum and the maximum value blocks to calculate ratio
        for i in 0..maxsize { 
           for j in 0..maxsize { 
              for  k in 0..maxsize { 
                 if cube[i as usize][j as usize][k as usize] == -1.0 { // Ignore partition blocks (which have -1)
                    continue;
                 }
                 maxval = maxval.max(cube[i as usize][j as usize][k as usize]);
                 minval = minval.min(cube[i as usize][j as usize][k as usize]);
                 sumval += cube[i as usize][j as usize][k as usize];
              }
           }
        }       
        eqratio = minval / maxval;        
        //println!("{}   {}   {}   {}   {}   {}", eqratio, time, 
        //      cube[(maxsize-1) as usize][0 as usize][0 as usize],
        //      cube[(maxsize-1) as usize][(maxsize-1) as usize][0 as usize], 
        //      cube[(maxsize-1) as usize][(maxsize-1) as usize][(maxsize-1) as usize], sumval); 
     } // end of while loop         
     println!("Box equilibrated in {} seconds of simulated time.", time);
}

