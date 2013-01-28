# Obsidian

## Next version of Obsidian
* Features: 
    * Local Push and Pull arrays.
    * Global Push and Pull arrays. 
 
* TODOs: 
    * Figure out good ways to express sequential computations per thread. 
    * Be able to launch computations on the GPU entirely from within Haskell (Linux and MAC OSX only).
    * For the Windows users, be able to generate the CUDA kernel launching code as text. 
    * Fix liveness issue. I think some case forgets to release unused memory. 
    * Understand what is going on with Cond and scanBlocks. 
    * I Think that it is a good idea to have the result of a mapG 
      distributed in the shared memories of the MPs that computed it. 
      (To solve the sharing of computed data problem that occurs in scanBlocks). 
      This sounds like havind something very similar to the Distrib array again. 
      Think about other ways than the Distrib array to get this result. 
      
       

## Extensions used and their meaning: 
* MultiParamTypeClasses
* FlexibleContexts 
* FlexibleInstances
* UndecidableInstances
* ScopedTypeVariables 
* TypeFamilies
* GADTs 