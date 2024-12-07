# Three-Body Simulation  

A simulation of the three-body problem in Haskell.  

## Features  
- Simulate the motion of three celestial bodies under the influence of gravity.  
- Customize initial conditions for experimentation.  
- Explore the fascinating dynamics of chaotic systems.  

## Installation  

Follow these steps to get started:  

### Prerequisites  
Ensure you have the following installed on your system:  
- [Git](https://git-scm.com/)
- Installing FreeGLUT for MinGW

## Steps for Microsoft Windows

1. **Download FreeGLUT**  
   - Go to the [Transmission Zero FreeGLUT page](http://www.transmissionzero.co.uk/software/freeglut-devel/).
   - Download the **MinGW version** of FreeGLUT.

2. **Extract the ZIP File**  
   - Extract the file `freeglut-MinGW-3.0.0-1.mp.zip`.
   - Navigate to the folder:  
     ```
     freeglut\bin\x64\
     ```
3. **Copy `freeglut.dll`**  
   - Locate the file `freeglut.dll` in the `x64` folder.
   - Copy this file to:  
     ```
     C:\Windows\System32
     ```
4. **Rename the DLL**  
   - Rename `freeglut.dll` in `C:\Windows\System32` to:  
     ```
     glut32.dll
     ```
## Notes
- Ensure that you download the **64-bit version** from the `x64` folder.  
- If you're using a 32-bit system, select the appropriate `x86` folder instead.

After completing these steps, FreeGLUT should be installed and ready to use on your system.

### Steps  
1. Clone the repository:  
   ```bash  
   git clone https://github.com/finnh-dev/three-body-sim.git  
2. Navigate into the repository:
   ```bash
   cd three-body-sim
4. Start the application:
   ```bash
   cabal run :all
