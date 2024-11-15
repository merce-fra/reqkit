# req2something

Utility to convert RE2019 "Scalable Analysis of Real-Time
Requirements" paper benchmark real-time requirements to some other
format :
  - nusmv 
  - vmtlib

## Prerequisites

In order to validate the generated smv code using the script NuSMV.sh, the environment variable REQ_VERIFICATION_INSTALL_DIR shall point to the installation directory of the tool req_verification.

The tool NuSMV 2.6.0 shall also be installed (https://nusmv.fbk.eu/downloads.html)


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

## Usage and Examples
- `simple_consistent.req` is rt-consistent (real and integer) since both triggers are "independent".

      ./scripts/reqkit.sh -a rtc -f reqs/simple_consistent.req --time-domain integer

  While the previous check applies BMC, we can actually prove rt-consistency with ic3ia:

      ./scripts/reqkit.sh -a rtc -f reqs/simple_consistent.req --time-domain integer --algorithm ic3ia

- `simple_consistent2.req` looks rt-inconsistent at first however whenever x0000 holds, one of the SUP automaton immediately goes to error; so no inconsistency here

      ./scripts/reqkit.sh -a rtc -f reqs/simple_consistent2.req --time-domain integer --algorithm ic3ia

- [fixme] `simple_consistent3.req` should be rt-inconsistent in my opinion (check this)

- [todo] `simple_inconsistent.req` used to be rt-inconsistent in the integer semantics:

      ./scripts/reqkit.sh -a rtc -f reqs/simple_consistent.req --time-domain integer

  but now it is consistent. Is this normal?
  
  In the real semantics it is consistent with >0, >=0

      ./scripts/reqkit.sh -a rtc -f reqs/simple_consistent.req --time-domain real --delay-domain 0
      ./scripts/reqkit.sh -a rtc -f reqs/simple_consistent.req --time-domain real --delay-domain 1

  but not when delays are >= 1:

      ./scripts/reqkit.sh -a rtc -f reqs/simple_consistent.req --time-domain real --delay-domain 2

  [todo] check this example

- The following is an obviously vacuous requirement set. BMC can only fail to found a non-vacuity witness:

      ./scripts/reqkit.sh -a vacuity -r "ID000" -f reqs/simple_vacuous.req

  We can prove vacuity with ic3ia (this uses opensmt as an interpolator):

      ./scripts/reqkit.sh -a vacuity -r "ID000" -f reqs/simple_vacuous.req --algorithm ic3ia

- An example of a non-vacuous requirement:

      ./scripts/reqkit.sh -a vacuity -r "ID000" -f reqs/simple_consistent.req


## How to run on all requirements file

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
  
