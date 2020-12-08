-type failure() :: erlmachine_system:failure().
-type failure(E) :: erlmachine_system:failure(E).
-type failure(E, R) :: erlmachine_system:failure(E, R).
-type failure(E, R, State) :: erlmachine_system:failure(E, R, State).

-type success(Result) :: erlmachine_system:success(Result).
-type success(Result, State) :: erlmachine_system:successs(Result, State).
-type success() ::  erlmachine_system:successs().

