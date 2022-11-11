////////////////////////////////////
//  CSC 330
//  SIMPLIFIED 3D DIFFUSION MODEL
////////////////////////////////////

fn main() {
    // Getting user input
    let mut input = String::new();
    println!("Msize?: ");
    std::io::stdin().read_line(&mut input).unwrap(); // Read the line
    let mut maxsize = input.parse::<i32>().unwrap(); // Parse and store it as an integer
    
    //let mut cube = Array3::<f64>::zeros((MDIM,MDIM,MDIM)); // Create a 3D-array with zeroes
    let cube = vec![vec![vec![0.0f64; maxsize as usize]; maxsize as usize]; maxsize as usize];

    // Declare variables (these don't need to be mutable)
    let diffusion_coefficient: f64 = 0.175; 
    let room_dimension: f64 = 5.0;                      // 5 Meters
    let speed_of_gas_molecules: f64 = 250.0;            // Based on 100 g/mol gas at RT
    let timestep: f64 = (room_dimension / speed_of_gas_molecules) / (maxsize as f64); // h in seconds
    let distance_between_blocks = room_dimension / (maxsize as f64);
    let DTerm = diffusion_coefficient * timestep / (distance_between_blocks*distance_between_blocks);

    cube[0 as usize][0 as usize][0 as usize] = 1.0e21; // Initialize the first cell
    
    let mut time: f64 = 0.0; // To keep up with accumulated time
    let mut eqratio: f64 = 0.0;
    
    //let val: f64 = MDIM as f64;
    while eqratio <= 0.99 {
       for i in 0..maxsize-1 {
          for j in 0..maxsize-1 {
             for k in 0..maxsize-1 {
                for l in 0..maxsize-1 {
                   for m in 0..maxsize-1 {
                      for n in 0..maxsize-1 {
                         //cube[[i,j,k]]=val*val*(i as f64)+val*(j as f64)+(k as f64)+1.0;
                         // Change occurs between adjacent blocks
                         if ((i == l) && (j == m) && (k == n+1)) ||  
                            ((i == l) && (j == m) && (k == n-1)) ||  
                            ((i == l) && (j == m+1) && (k == n)) ||  
                            ((i == l) && (j == m-1) && (k == n)) ||  
                            ((i == l+1) && (j == m) && (k == n)) ||  
                            ((i == l-1) && (j == m) && (k == n)) {
                               let mut change: f64 = (cube[[i as usize, j as usize, k as usize]] 
                                                      - cube[[l as usize, m as usize , n as usize]]) * DTerm;
                               cube[[i as usize, j as usize, k as usize]] -= change;                                
                               cube[[l as usize, m as usize, n as usize]]  += change;  
                          }
                       }
                    }
                 }
              }
           }
        }

        time += timestep;   

        // Variables to keep track of minimums, maximums and total
        let mut sumval: f64 = 0.0;
        let mut maxval: f64 = cube[[0 as usize, 0 as usize, 0 as usize]]; 
        let mut minval: f64 = cube[[0 as usize, 0 as usize, 0 as usize]];

        // Find the minimum and the maximum value blocks to calculate ratio
        for i in 0..maxsize-1 { 
           for j in 0..maxsize-1 { 
              for  k in 0..maxsize-1 { 
                 //if (cube[i][j][k] == -1) { // Ignore partition blocks (which have -1)
                 //   continue;
                 //}
                 maxval = cube[[i as usize, j as usize, k as usize]].max(maxval);
                 minval = cube[[i as usize, j as usize, k as usize]].min(minval);
                 sumval = sumval + cube[[i as usize, j as usize, k as usize]];
              }
           }
        }
        
        eqratio = minval / maxval;
     } // end of while loop     
    
     println!("Box equilibrated in {} seconds of simulated time.", time);
}

