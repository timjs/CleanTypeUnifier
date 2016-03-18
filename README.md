# CleanTypeUnifier

A type unifier for Clean, in Clean.

Copyright &copy; 2016 Camil Staps. Licensed under MIT (see the LICENSE file).

## Installation

Be sure to clone this repository recursively, to include the clean-compiler.

Unfortunately, the git repository of the clean-compiler misses the general Makefile the svn repository has. Therefore, we need to do a bit more than usually to build the compiler modules we need:

    $ cd clean-compiler/main/Unix
    $ make
    $ cd ../../backendC/CleanCompilerSources
    $ make -f Makefile.linux64
    $ cd ../..
    $ mkdir backend/Clean\ System\ Files
    $ ln -s ../../backendC/CleanCompilerSources/backend.a backend/Clean\ System\ Files/backend_library

After this we should be able to build the projects from the root directory:

    $ cpm make

