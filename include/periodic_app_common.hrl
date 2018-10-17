%%%%----------------------------------------------------------------------------------------------------------
%%%                                            Supervisor Names
%%%%----------------------------------------------------------------------------------------------------------

-define(EXPIRABLE_SUPERVISOR_NAME, expirable_sup).
-define(ASYNC_RUNNER_SUPERVISOR_NAME, async_runner_sup).

-define(VERIFY_CHARACTERS, "0123456789").

-define(GUID_LENGTH, 8).


-record(expirable_state, {
  task,
  data,
  return,
  name,
  timeout,
  timeout_ref
}).

-record(periodic_state,
{
  stop = false,
  name,
  type = custom,
  task,
  data,
  interval,
  start_after = 0,
  max,
  number_of_calls = 0,
  interval_ref
}).
