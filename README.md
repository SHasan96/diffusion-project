# CSC 330 - Project 2
# Simplified 3D Diffusion Model

In this project we model simple 3D diffusion. We assume a cubic room with a size of 5 meters. The room is divided into smaller cubes which in the programs is
represented by 3D arrays/vectors. The smaller the divisions (i.e., the larger the number of divisions and hence the size of the arrays/vectors), the more accurate
is the simulation time for equilibration. We also model the effects of adding a partition in the cubic room.
 
The program was written in 7 programming languages.

The base code was provided by the instructor in Java, and it was used as a reference to write the program in other languages.

## The program flow

The program runs with a selected Msize which is the number of divisions of the cubic room.Once the Mszie is set, we need to specify whether the partition should be added into the room or not. The program starts by adding 1.0e21 molecules added to the first block of the array and runs until the equilibrium ratio is greater 
than 0.99. The amount of time (simulated time for the equilibration) is the final output. 

### Program runtimes
The program has a lot nested loops which makes it run very slow for high values of Msize. Never-the-less it still runs. The actual runtimes differ greatly.
Some languages are significantly slower than others.

## Partition details
My logic behind the partition was to fill the partition blocks with -1. A negative number is easily distinguishable (since the number of molecules
will never be negative). When the number of particles change/move from one block to another, we determine if a block or the block next to it is a partition
block (i.e., check if the value at that array index is -1). If it is then we know it is a partition block and no change or molecule movement takes place.
Such blocks are also ignored for calculating the ratio.

### Partition placement 
The partition is placed halfway into the room and covers three-forths (75%) of the height. However, for even Msizes we need to make a choice if we want to
place the partition one block before or after. Array indices are obviously integer so when determining the partition placement we chose to go with 
the block before. 

For instance, if Msize = 10, then px = 4 (assuming the indexing starts at 0) and py starts at 2. Here px is the x-coordinate of the partition and py is 
its y-coordinate. The partition obviously covers the whole z-axis at these values of px and py. 

The same logic was implemented in all languages (with modifications for ones where array indexing starts at 1). 

## Compilation and execution instructions

The names of all source code files were "diffusion" plus the appropriate file extension.
Listed in order in which they appear in the repo we have the following.

We are taking command line arguments. The format is:

"/<filename/> M p" where M is an integer represent the Msize and p is a character that turns the partition on/off.
The partition will be turned on with only 'y' (case sensitive) in the command line argument and any other character would not.
We will just use 'y' and 'n' for yes and no respectively to specify if we want the partition added.

In the follwing instructions I will use Msize = 10 (as an example) and run with the partition off first and then with it on.  

### Ada
To compile:
```
gnatmake diffusion.adb 
```
To run with no partition:
```
./diffusion 10 n
``` 
To run with the partition:
```
./diffusion 10 n
```

### C++
To compile:
```
g++  -O2 diffusion.cpp -o diffusion
```
An executable with a the name "diffusion" is created.\ 
Note that the -O2 optimization flag is important to run the program faster.\
To run without partition:
```
./diffusion 10 n
```
To run with the partition:
```
./diffusion 10 y
```

### Fortran
To compile:
```
gfortran -O2 diffusion.f95 -o diffusion
```
An executable with a the name "diffusion" is created.\
To run without partition:
```
./diffusion 10 n
```
To run with the partition:
```
./diffusion 10 y
```

### Julia 
To compile:
```
javac Happy.java
```
A Happy.class file is created.\
To run:
```
java Happy
```

### Lisp
Due to lack of proficiency with the SBCL IDE, the program was written as a script which were made executable.\
Add the following line at the top to invoke the SBCL environment:
```
#!/usr/bin/sbcl --script
```
Make the lisp file executable for the user by:
```
chmod u+x Happy.lisp
```
Then, run using:
```
./Happy.lisp
```
or just
```

### Python
Also written as an executable script.\
Add this line at the top:
```
#!/usr/bin/env python3
```
Make the perl file executable for the user by:
```
chmod u+x Happy.py
```
Then, run using:
```
./Happy.py
```
or just
```
Happy.py
```
Python is an interpreted language.\
Alterantively, we can use:
```
python3 Happy.py
```
### Rust

## References:
For tutorial sources such as tutorialspoint, w3schools, youtube, geeks-for-geeks, etc. were used.
Some code ideas were taken from stackoverflow, etc, and changed to fit the program.
The instructor provided the base code in Java which was used as the main source of reference to write in the other languages.

