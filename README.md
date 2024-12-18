# ReqKit: Requirement Analysis and Repair Tool Kit

Consistency and vacuity verification, model checking, and repair tool for real-time temporal requirements.
The tool accepts as input patterns given as simplified universal patterns (SUP), or structured English.

Verification conditions in SMV or VMTLIB formats are generated and discharged by model checkers.

## Installation
This program is written partly in OCaml and in Python, and works on Linux.

All requirements can be installed and compiled by running 

        ./scripts/install.sh

assuming opam and pip3 installed.

Python (>=3.10) requirements are as follows:
- timeout-decorators
- z3-solver

Ocaml (=5.1) requirements as follows:
- dune
- merlin
- alcotest
- ppx_inline_test
- bisect_ppx

The tool also needs the following external tools for model checking and LTL formula manipulation:
- Spot 2.12.1 (http://www.lrde.epita.fr) (executables `ltlfilt`, `ltl2tgba`)
- NuSMV 2.6.0 (https://nusmv.fbk.eu/downloads.html) (executable `NuSMV`)
- Pono-RT (https://github.com/osankur/pono-rt/) (executable `pono-rt`)

These can be installed for Linux x86_64 by running the scripts `scripts/setup_*.sh`.

## Input Formats
1. Simplified Universal Patterns (SUP)
    The main input format is simplified universal patterns (SUP). These are interpreted as timed automata as described in the paper.
    The input format defines the list `REQ_SET` of requirements in the following format:

    ```python
    on = Bool('on')
    blink = Bool('blink')
    REQ_SET = [
            [ on , True , True , 0, 0, 0, 0, True, on, Not(on), 10, 10],
            [ And(blink, Not(on)), Not(on), True, 10, 10, 0, 0, True, True, on, 0, 0]
        ]
    ```

    The first two lines declare two variables `on`, and `blink`. The list `REQ_SET` contains a list of SUP requirements.
    Each element specifies the tuple `(TSE, TC, TEE, Tmin, Tmax, Lmin, Lmax, ASE, AC, AEE, Amin, Amax)`. Here Boolean 
    expressions use Z3 constructs, while time bounds are integers.

    The definition above comes from `examples/flashing1.py`. One can see that there is an import statement so that Z3 constructs are in the scope.
    In addition, the parameter `AlPHA` specifies the unfolding bound for bounded model checking (but this is overriden by the command line option `--bmc-bound` which is preferable). Parameters `BETA` and `MAX_PTRACE` are only used for repair and can be set to these default values.
    `COND_INIT` can be used to set the initial values of the variables but it is often kept empty.

    Other examples can be found in `examples/*.py`.

2. Structured English
    The second input format we support is a fragment of structured English used in other tools such as Hanfor.
    The list of all patterns supported are given in the folder `lib/translator/tests/patterns`. Each pattern is translated to SUP;
    these translations are given in the Cram test file `lib/translator/tests/req_patterns.t`.
    This file thus defines the semantics of these patterns in our tool as SUP patterns to which they are converted.

    Examples can be found in `examples/*.req`.

## Usage and Examples
### Examples from the paper
The rt-consistency of R1, R2 can be checked using integer clocks as follows.

        ./reqkit -a rtc --time-domain integer -f examples/flashing1.sup

which runs bounded model checking (BMC) with Pono-RT by default. The size of the unrolling can be specified with `--bmc-bound k`. All delays are positive by default, i.e. greater than or equal to 1.

One can also prove rt-consistency using quantifier elimination and ic3ia as follows:

        ./reqkit -a rtc --time-domain integer --algorithm ic3ia -f examples/flashing1.sup

or using the NuSMV engine with the option `-e nusmv`.

        ./reqkit -a rtc -e nusmv -f examples/flashing1.sup

We can check the rt-inconsistency of R1, R2, R4, and check the rt-consistency of R1, R2', R4

        ./reqkit -a rtc --time-domain integer -f examples/flashing2.sup
        ./reqkit -a rtc --time-domain integer --algorithm ic3ia -f examples/flashing_fix1.sup

The rt-consistency disappears when the time domain is reals:

        ./reqkit -a rtc --time-domain real --algorithm ic3ia -f examples/flashing2.sup

This is due to the fact that even with strict delays, it is possible to pick smaller and smaller delays
and never actually reach the deadline of the action phases of R1 and R2.
The rt-inconstency appears again if we restrict the time domain to be $\geq 1$
(possible choices are positive or zero (0), positive (1), and $\geq 1$ (2)):

        ./reqkit -a rtc --time-domain real --delay-domain 2 -f examples/flashing2.sup 

We can check that all three requirements in the set R1, R2', R4 are non-vacuous. The tool generates witness traces satisfying each requirement:

        ./reqkit -a vacuity -r 0 -f examples/flashing_fix1.sup
        ./reqkit -a vacuity -r 1 -f examples/flashing_fix1.sup
        ./reqkit -a vacuity -r 2 -f examples/flashing_fix1.sup

Note that an integer-valued trace can be computer with the option `--time-domain integer`.

We already saw that R2 is vacuous in the set R1, R2, R3:

        ./reqkit -a vacuity -r 1 -f examples/flashing_vacuous.sup
        ./reqkit -a vacuity -r 1 --algorithm ic3ia -f examples/flashing_vacuous.sup

We can also query properties using LTL model checking, e.g.

        ./reqkit -a ltl --time-domain integer --algorithm ic3ia --formula 'G( !on & X(blink) -> X(on) )' -f examples/flashing_fix1.py  

This is only possible for the safety fragment of LTL using Pono. However the NuSMV engine can check full LTL and CTL:

        ./reqkit -a ltl -e nusmv --formula 'G( on -> F(!on))' -f examples/flashing_fix1.sup
        ./reqkit -a ctl -e nusmv --formula 'AG(blink -> EX (!blink))' -f examples/flashing_fix1.sup

The repair feature can be used as follows:

        ./reqkit -a repair -f examples/carriage_inconsistent.py

Note that this is an experimental feature and there is no termination guarantee.

        ./reqkit -a ltl --algorithm ic3ia --formula 'G( !on & (X on) -> X (X on))' -f examples/flashing_fix1.py

### Examples with the structured English format
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

- Consider `sample5.req`. This is an obviously vacuous requirement set. 

      ID000: Globally, it is always the case that if "x0000" holds, then "x0001" holds for at least 25 time units
      ID001: Globally, it is always the case that "!x0000" holds

    BMC can only fail to found a non-vacuity witness:

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

- We now illustrate LTL and CTL model checking.
    Consider `sample4.req` above which was proved to be rt-consistent. It looks like `x0000` should imply `~x0001`. Let us check this.

            ./reqkit -a ltl -f examples/sample4.req --formula 'G(x0000 -> ~x0001)'
            ./reqkit -a ltl -f examples/sample4.req --algorithm ic3ia --formula 'G(x0000 -> ~x0001)'

    The first check returns unknown due to k-induction not being conclusive, but the second check succeeds.

    At this point we suspect that `x0000 & ~x0001` is always true. Is this the case?

            ./reqkit -a ltl -f examples/sample4.req --formula 'G(x0000 & ~x0001)'

    The tool generates a counterexample: in fact, both can be false, which does not violate any requirement.

    The LTL fragment to be used with the Pono-RT engine is restricted to the safety case. The tool will reject formulas that are outside of this fragment.

    However, the NuSMV engine works with all LTL and CTL formulas. The following formulas hold on the flashing light example:

            ./reqkit -a ltl --formula 'G(on -> F (!on))' -e nusmv -f examples/flashing1.py  
            ./reqkit -a ltl --formula 'G((!on & (G blink)) -> F(on))' -e nusmv -f examples/flashing1.py  
            ./reqkit -a ctl --formula 'AG(blink -> EX (!blink))' -e nusmv -f examples/flashing1.py  
            ./reqkit -a ctl --formula 'AG(blink -> EX (blink))' -e nusmv -f examples/flashing1.py  

    And for example the following does not

            ./reqkit -a ltl --formula 'G(on -> X on)' -e nusmv -f examples/flashing1.py

### Repair
The repair analysis attempts to automatically repair rt-inconsistent requirement sets.
There are four algorithms that can be specified with the option `--repair-algorithm`.
The first three consist in fixing rt-consistency by adding a freshly generated requirement to the set:
  - `instant`: generate an instantaneous SUP (where all time bounds are 0 - safety properties) - this is the default algorithm
  - `min-instant`: as above but also minimize the total size of the generated expressions 
  - `generate`: generate an arbitrary SUP requirement
The last one consists in minimally modifying a requirement:
  - `modify`: attempt to modify the first requirement in the list

Note that if the original set is vacuous, the repair algorithm cannot fix vacuity by adding a fresh requirement.

Some examples:
    ./reqkit -a repair -f examples/sup/cruise_add.py
    ./reqkit -a repair -f examples/sup/carriage_add.py

