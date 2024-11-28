# ReqKit: Requirement Analysis and Repair Tool Kit

Consistency and vacuity verification, model checking, and repair tool for automata-based requirements.
The tool accepts as input patterns given as simplified universal patterns (SUP), or structured English.

Verification conditions in SMV or VMTLIB formats are generated and discharged by model checkers.


## Installation
This program is written partly in OCaml and in Python.
Pip3 and Opam are assumed to be installed on the system.

Python (>=3.10) requirements are as follows:
- timeout-decorators
- z3-solver

Ocaml (>=5.1) requirements as follows:
- dune
- merlin
- alcotest
- ppx_inline_test
- bisect_ppx

All these requirements can be installed by running `./scripts/install.sh` (assuming opam and pip3 are available).

The following verification engines must also be installed

- NuSMV 2.6.0 (https://nusmv.fbk.eu/downloads.html). The executable `NuSMV` must be on the path.
- Pono-RT (https://github.com/osankur/pono-rt/). The executable `pono` must be on the path.

### Compilation

    cd lib/translator
    dune build

TODO: Automatize these steps, as well as the installation and compilation of NuSMV and Pono executables

## Usage and Examples
## RT-Consistency and Vacuity Checking Using the Pono-RT engine
- Consider `sample1.req`

      ID000: Globally, it is always the case that if "x0000" holds, then "x0001" holds after at most 25 time units
      ID001: Globally, it is always the case that if "x0000" holds, then "!x0001" holds for at least 20 time units

  This is inconsistent with the real >=1 semantics:

      ./reqkit -a rtc -f examples/sample1.req --time-domain real --delay-domain 2

  In fact, if x0000/\~x0001 occurs until time 25-epsilon, then there is a no delay >= 1 for ID0000 to complete its action phase and observe x0001.

  The set is consistent if we allow shorter delays, and for integers (with positive or null delays):

      ./reqkit -a rtc -f examples/sample1.req --time-domain real --delay-domain 0
      ./reqkit -a rtc -f examples/sample1.req --time-domain real --delay-domain 1
      ./reqkit -a rtc -f examples/sample1.req --time-domain integer --delay-domain 0

  It becomes rt-inconsistent again for strict integer delays (>=1)

      ./reqkit -a rtc -f examples/sample1.req --time-domain integer --delay-domain 1

  Non-vacuity:

      ./reqkit -a vacuity -r ID000 -f examples/sample1.req --time-domain integer
      ./reqkit -a vacuity -r ID001 -f examples/sample1.req --time-domain integer

- Consider `sample2.req`:

      ID000: Globally, it is always the case that if "x0000" holds, then "x0001" holds for at least 2 time units
      ID001: Globally, it is always the case that if "x0001" holds, then "!x0000" holds after at most 2 time units

  The same phenomenon occurs as for `sample1.req`; an inconsistency appears if we restrict arbitrary delays to >= 1 since there is no possible delay in this domain to complete an action phase:

      ./reqkit -a rtc -f examples/sample2.req --time-domain real --delay-domain 2

  But the set is consistent for other semantics. 

  Non-vacuity:

      ./reqkit -a vacuity -r ID000 -f examples/sample2.req --time-domain integer
      ./reqkit -a vacuity -r ID001 -f examples/sample2.req --time-domain integer

- Consider `sample3.req`:

      ID000: Globally, it is always the case that if "x0000" holds, then "x0001" holds after at most 24 time units
      ID001: Globally, it is always the case that if "x0000" holds, then "!x0001" holds for at least 24 time units

  Again, the >=1 semantics is flawed so it gives inconsistencies due to the semantics being stucked whenever there is an upper bound like here.

  More interestingly, the inconsistency does not appear here in the integer semantics with 0 delays allowed since after reading x000/\x001 for 24 time units, the automata can just cycle with 0 delays:

      ./reqkit -a rtc -f examples/sample3.req --time-domain integer --delay-domain 0
      ./reqkit -a rtc -f examples/sample3.req --time-domain real --delay-domain 0

  But restricting to strict delays reveals the inconsistency:

      ./reqkit -a rtc -f examples/sample3.req --time-domain integer --delay-domain 1

  With real durations, time can be blocked even if all delays are strictly positive so there is no inconsistency here:

      ./reqkit -a rtc -f examples/sample3.req --time-domain real --delay-domain 1

  In all semantics however both requirements are vacuous because they cannot complete the action phase without going into an error state:

      ./reqkit -a vacuity -r ID000 -f examples/sample3.req --time-domain real --algorithm ic3ia  
      ./reqkit -a vacuity -r ID001 -f examples/sample3.req --time-domain real --algorithm ic3ia  

- Consider `sample4.req`: 

      ID000: Globally, it is always the case that if "x0000" holds, then "x0001" holds for at least 25 time units
      ID001: Globally, it is always the case that if "x0000" holds, then "!x0001" holds for at least 20 time units

  This might look rt-inconsistent at first however whenever x0000 holds, one of the SUP automaton immediately goes to error; so no inconsistency here:

      ./reqkit -a rtc -f examples/sample4.req --time-domain integer --algorithm ic3ia

- Consider `sample5.req`. This obviously a vacuous requirement set. BMC can only fail to found a non-vacuity witness:

      ID000: Globally, it is always the case that if "x0000" holds, then "x0001" holds for at least 25 time units
      ID001: Globally, it is always the case that "!x0000" holds

      ./reqkit -a vacuity -r "ID000" -f examples/sample5.req

  We can prove vacuity with ic3ia (this uses opensmt as an interpolator):

      ./reqkit -a vacuity -r "ID000" -f examples/sample5.req --algorithm ic3ia

- Consider `sample6.req`.

      ID000: Globally, it is always the case that if "x0000" holds, then "x0001" holds for at least 25 time units
      ID001: Globally, it is always the case that if "x0000" holds, then "!x0001" holds for at least 20 time units
      ID002: Globally, it is always the case that if "x0002"  holds for at least 50 time units, then "x0000"  holds afterwards 

  We already established that {ID000, ID001} alone is not inconsistent. Adding ID002 here means that we add a prefix with no error where x002 holds for 50 time units. However, because "holds afterwards" puts no time bound on the realization of the action phase, this is still consistent:

      ./reqkit -a rtc -f examples/sample6.req --algorithm ic3ia

  But vacuous since none of the requirements can be realized:

      ./reqkit -a vacuity -r "ID000" -f examples/sample6.req --algorithm ic3ia
      ./reqkit -a vacuity -r "ID001" -f examples/sample6.req --algorithm ic3ia
      ./reqkit -a vacuity -r "ID002" -f examples/sample6.req --algorithm ic3ia

## Using the NuSMV engine
The NuSMV engine always assumes delay-first and unit-time semantics:

        ./reqkit -a rtc -f examples/sample1.req -e nusmv

## Repair
The repair analysis attempts to automatically repair rt-inconsistent requirement sets.
There are four algorithms that can be specified with the option `--repair-algorithm`.
The first three consist in fixing rt-consistency by adding a freshly generated requirement to the set:
  - `instant`: generate an instantaneous SUP (where all time bounds are 0 - safety properties) - this is the default algorithm
  - `min-instant`: as above but also minimize the total size of the generated expressions 
  - `generate`: generate an arbitrary SUP requirement
The last one consists in minimally modifying a requirement:
  - `modify`: attempt to modify the first requirement in the list

Give here simple and easy to understand examples.

Note that if the original set is vacuous, the repair algorithm cannot fix vacuity by adding a fresh requirement.

    ./reqkit -a repair -f examples/sup/cruise_add.py
    ./reqkit -a repair -f examples/sup/carriage_add.py

