# Three-Body Simulation  

A simulation of the three-body problem in Haskell.  

## Features  
- Simulate the motion of three celestial bodies under the influence of gravity.  
- Customize initial conditions for experimentation.  
- Explore the fascinating dynamics of chaotic systems.  

# Installation  

Follow these steps to get started:  

## Prerequisites  
Ensure you have the following installed on your system:  
- [Git](https://git-scm.com/)
- GLUT

# Steps for installing GLUT for Microsoft Windows

1. **Download FreeGLUT**  
   - Go to the [Transmission Zero FreeGLUT page](http://www.transmissionzero.co.uk/software/freeglut-devel/).
   - Download the **MSVC Package** of FreeGLUT.

2. **Extract the ZIP File**  
   - Extract the file `freeglut-MSVC-3.0.0-2.mp.zip`.
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

# Example for UNIX based operating systems
- Ensure your system is up to date.
- Consult your package manager for the correct name of the package FreeGLUT.
  
 **Example Arch-based Systems**

   Run the following command to install FreeGLUT:
   ```bash
   sudo pacman -S freeglut
   ```
 **Example Debian-based Systems**  
 
   Run the following command to install FreeGLUT:  
   ```bash
   sudo apt-get install freeglut3-dev
   ```
# Usage  
1. Clone the repository:  
   ```bash  
   git clone https://github.com/finnh-dev/three-body-sim.git  
2. Navigate into the repository:
   ```bash
   cd three-body-sim
4. Start the application:
   ```bash
   cabal run :all
# Configuration
If you want to try out other Orbits, you can swap them in the `Init.hs` file.

For example:

`initialConditions = initOrbit FigureEight`

![image](https://github.com/user-attachments/assets/148d35cb-795a-4b70-a49f-0f3ec5590644)


#License
Copyright (c) 2024 Finn H

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
