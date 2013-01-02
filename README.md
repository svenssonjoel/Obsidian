# Obsidian

## Next version of Obsidian
* Goals: 
    * Push and Pull arrays. 
    * Ways to express computations on global arrays (Across Blocks).
        * Global input arrays, pull arrays, Distributed (Distrib) over blocks 
        * Global Push arrays (GlobArray) 
    * Be able to launch computations on the GPU entirely from within Haskell (Linux and MAC OSX only).
    * For the Windows users, be able to generate the CUDA kernel launching code as text. 

## TODO: 
* Cleaner interface to Programs. 
    * DONE: write "sync" not "BSync"  (now called force) 
    * etc
* Add sequential loop and conditional blocks to ThreadPrograms.


## Extensions used and their meaning: 
* MultiParamTypeClasses
* FlexibleContexts 
* FlexibleInstances
* UndecidableInstances
* ScopedTypeVariables 
* TypeFamilies
* GADTs 