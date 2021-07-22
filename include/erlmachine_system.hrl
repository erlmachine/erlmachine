-type failure() :: erlmachine_system:failure().
-type failure(E) :: erlmachine_system:failure(E).
-type failure(E, R) :: erlmachine_system:failure(E, R).
-type failure(E, R, S) :: erlmachine_system:failure(E, R, S).

-type success(Res) :: erlmachine_system:success(Res).
-type success(Res, S) :: erlmachine_system:successs(Res, S).
-type success(Res, S, A) :: erlmachine_system:successs(Res, S, A).
-type success() ::  erlmachine_system:successs().

