# Questions
- in `input_args.ml`

    ```(fill "check_rt_consistency")^"If true check real time consistency and therefore, replace the nextclock keyword with next in vmt file (default is false).");```

# Tasks
- [review] Could Francois check all transitions:
  - if a transition goes through act->idle, then add `vacuity_constraint`; otherwise add `vacuity_unchanged`.
  - a single transition must not contain contradictory next-clock constraints
    (see the fix by removing next-clock constraints from `*_no_clock` functions)
  - for each transition that stops at state s, add `(not s_to_s_no_clock')` for all successor states s' (including err) if such a transition is feasible immediately.

- What is the req syntax we support exactly?

  This is OK:

      ID002: Globally, it is always the case that if "x0002"  holds for at least 50 time units, then "x0000"  holds afterwards

  But not this:

      ID002: Globally, it is always the case that if "x0002"  holds for at least 50 time units, then "x0000"  holds after at most 5 time units

- [task] A simple input format for SUP, and a parser:
  - .req input files are processed as structured English
  - .sup input files are processed directly as SUP

- [task] Do not restrict identifiers to start with x

- Tool integration:
  - None zero return value on error
  - Integrate Reiya's SMV script within this tool
  - Integrate Reiya's repair script within this tool
  - Add script to launch the repair program

# Examples
- Consider `sample1.req`

      ID000: Globally, it is always the case that if "x0000" holds, then "x0001" holds after at most 25 time units
      ID001: Globally, it is always the case that if "x0000" holds, then "!x0001" holds for at least 20 time units

  This is inconsistent with the real >=1 semantics:

      ./scripts/reqkit.sh -a rtc -f reqs/sample1.req --time-domain real --delay-domain 2

  In fact, if x0000/\~x0001 occurs until time 25-epsilon, then there is a no delay >= 1 for ID0000 to complete its action phase and observe x0001.

  The set is consistent if we allow shorter delays, and for integers:

      ./scripts/reqkit.sh -a rtc -f reqs/sample1.req --time-domain real --delay-domain 0
      ./scripts/reqkit.sh -a rtc -f reqs/sample1.req --time-domain real --delay-domain 1
      ./scripts/reqkit.sh -a rtc -f reqs/sample1.req --time-domain integer

  Non-vacuity:

      ./scripts/reqkit.sh -a vacuity -r ID000 -f reqs/sample1.req --time-domain integer
      ./scripts/reqkit.sh -a vacuity -r ID001 -f reqs/sample1.req --time-domain integer

- Consider `sample2.req`:

      ID000: Globally, it is always the case that if "x0000" holds, then "x0001" holds for at least 2 time units
      ID001: Globally, it is always the case that if "x0001" holds, then "!x0000" holds after at most 2 time units

  The same phenomenon occurs as for `sample1.req`; an inconsistency appears if we restrict arbitrary delays to >= 1 since there is no possible delay in this domain to complete an action phase:

      ./scripts/reqkit.sh -a rtc -f reqs/sample2.req --time-domain real --delay-domain 2

  But the set is consistent for other semantics. 

  Non-vacuity:

      ./scripts/reqkit.sh -a vacuity -r ID000 -f reqs/sample2.req --time-domain integer
      ./scripts/reqkit.sh -a vacuity -r ID001 -f reqs/sample2.req --time-domain integer

- Consider `sample3.req`:

      ID000: Globally, it is always the case that if "x0000" holds, then "x0001" holds after at most 24 time units
      ID001: Globally, it is always the case that if "x0000" holds, then "!x0001" holds for at least 24 time units

  Again, the >=1 semantics is flawed so it gives inconsistencies due to the semantics being stucked whenever there is an upper bound like here.

  More interestingly, the inconsistency does not appear here in the integer semantics with 0 delays allowed since after reading x000/\x001 for 24 time units, the automata can just cycle with 0 delays:

      ./scripts/reqkit.sh -a rtc -f reqs/sample3.req --time-domain integer --delay-domain 0
      ./scripts/reqkit.sh -a rtc -f reqs/sample3.req --time-domain real --delay-domain 0

  But restricting to strict delays reveals the inconsistency:

      ./scripts/reqkit.sh -a rtc -f reqs/sample3.req --time-domain integer --delay-domain 1

  With real durations, time can be blocked even if all delays are strictly positive so there is no inconsistency here:

      ./scripts/reqkit.sh -a rtc -f reqs/sample3.req --time-domain real --delay-domain 1

  In all semantics however both requirements are vacuous because they cannot complete the action phase without going into an error state:

      ./scripts/reqkit.sh -a vacuity -r ID000 -f reqs/sample3.req --time-domain real --algorithm ic3ia  
      ./scripts/reqkit.sh -a vacuity -r ID001 -f reqs/sample3.req --time-domain real --algorithm ic3ia  

- Consider `sample4.req`: 

      ID000: Globally, it is always the case that if "x0000" holds, then "x0001" holds for at least 25 time units
      ID001: Globally, it is always the case that if "x0000" holds, then "!x0001" holds for at least 20 time units

  This might look rt-inconsistent at first however whenever x0000 holds, one of the SUP automaton immediately goes to error; so no inconsistency here:

      ./scripts/reqkit.sh -a rtc -f reqs/sample4.req --time-domain integer --algorithm ic3ia

- Consider `sample5.req`. This obviously a vacuous requirement set. BMC can only fail to found a non-vacuity witness:

      ID000: Globally, it is always the case that if "x0000" holds, then "x0001" holds for at least 25 time units
      ID001: Globally, it is always the case that "!x0000" holds

      ./scripts/reqkit.sh -a vacuity -r "ID000" -f reqs/sample5.req

  We can prove vacuity with ic3ia (this uses opensmt as an interpolator):

      ./scripts/reqkit.sh -a vacuity -r "ID000" -f reqs/sample5.req --algorithm ic3ia

- Consider `sample6.req`.

      ID000: Globally, it is always the case that if "x0000" holds, then "x0001" holds for at least 25 time units
      ID001: Globally, it is always the case that if "x0000" holds, then "!x0001" holds for at least 20 time units
      ID002: Globally, it is always the case that if "x0002"  holds for at least 50 time units, then "x0000"  holds afterwards 

  We already established that {ID000, ID001} alone is not inconsistent. Adding ID002 here means that we add a prefix with no error where x002 holds for 50 time units. However, because "holds afterwards" puts no time bound on the realization of the action phase, this is still consistent:

      ./scripts/reqkit.sh -a rtc -f reqs/sample6.req --algorithm ic3ia

  But vacuous since none of the requirements can be realized:

      ./scripts/reqkit.sh -a vacuity -r "ID000" -f reqs/sample6.req --algorithm ic3ia
      ./scripts/reqkit.sh -a vacuity -r "ID001" -f reqs/sample6.req --algorithm ic3ia
      ./scripts/reqkit.sh -a vacuity -r "ID002" -f reqs/sample6.req --algorithm ic3ia
