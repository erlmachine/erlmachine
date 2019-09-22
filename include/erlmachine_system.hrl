-type timeout()::int() > 0|infinity.

-type failure(E, R)::{error, {E::term(), R::term()}.
-type failure(E)::{error, E::term()}.
-type success(Result)::{ok, Result::term()}.
-type success()::ok.

