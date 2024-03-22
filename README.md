# req2something

Utility to convert RE2019 "Scalable Analysis of Real-Time
Requirements" paper benchmark real-time requirements to some other
format.

TODO: tell which formats

## Installation

Opam shall be installed using the command (depending on your linux distribution): 
sudo apt install opam

The project has the folowing dependancies:
- dune
- merlin
- alcotest
- ppx_inline_test
- bisect_ppx

To install them, run the script "./scripts/install.sh" in the project directory

## How to compile

Run the command "dune build" in the project directory

## How to run ocn all requirements file

Run the script ./scripts/VMTlib.sh from project directory to generate in the output directory all the .vmt files on all requirements files
Run the script ./scripts/NuSMV.sh from project directory to generate in the output directory all the .smv files on all requirements files

## How to run unit tests

Run the command "dune runtest" in the project directory

## How to get coverage info when processing all requirements files

Run the script ./scripts/coverage.sh

## How to use the tool

Run the command "./exec --help" in the project directory

## Directories content

* README.md: basic instructions
* reqs/: requirements from RE2019 "Scalable Analysis of Real-Time
  Requirements" paper benchmark (https://zenodo.org/records/3341453)
* scripts/: all scripts used to install and launch the tool
* src/: all source files
  
