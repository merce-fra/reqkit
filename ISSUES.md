# Questions
- in `input_args.ml`

    ```(fill "check_rt_consistency")^"If true check real time consistency and therefore, replace the nextclock keyword with next in vmt file (default is false).");```

# Tasks
- [review] Could Francois check all transitions:
  - if a transition goes through act->idle, then add `vacuity_constraint`; otherwise add `vacuity_unchanged`.
  - a single transition must not contain contradictory next-clock constraints
    (see the fix by removing next-clock constraints from `*_no_clock` functions)
  - for each transition that stops at state s, add `(not s_to_s_no_clock')` for all successor states s' (including err) if such a transition is feasible immediately.

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
- `simple_consistent.req` is rt-consistent (real and integer) since both triggers are "independent".

      ./scripts/reqkit.sh -a rtc -f reqs/simple_consistent.req --time-domain integer

  While the previous check applies BMC, we can actually prove rt-consistency with ic3ia:

      ./scripts/reqkit.sh -a rtc -f reqs/simple_consistent.req --time-domain integer --algorithm ic3ia

- `simple_consistent2.req` looks rt-inconsistent at first however whenever x0000 holds, one of the SUP automaton immediately goes to error; so no inconsistency here

      ./scripts/reqkit.sh -a rtc -f reqs/simple_consistent2.req --time-domain integer --algorithm ic3ia

- [fixme] `simple_consistent3.req` should be rt-inconsistent in my opinion (check this)
- [fixme] `simple_inconsistent2.req` should be rt-inconsistent in my opinion (check this)
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
