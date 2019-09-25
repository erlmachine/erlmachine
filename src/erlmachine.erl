-module(erlmachine).
-export([]).
%% The main purpouse of erlmachine project is providing a set of well designed behaviours which are accompanied with visualization tools as well.
%%  Erlmachine doesn't restrict your workflow by the one possible way but instead provide to you ability to implement your own components. This ability is available under flexible mechanism of prototypes and overloading.  

serial_number() -> erlmachine_factory:serial_number(<<"">>).

part_number() -> erlmachine_factory:part_number(<<"">>).
