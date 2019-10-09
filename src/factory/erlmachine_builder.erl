-module(erlmachine_builder).
-export([]).

%%#{id => child_id(),       % mandatory
  %%start => mfargs(),      % mandatory
  %%restart => restart(),   % optional
  %%shutdown => shutdown(), % optional
  %%type => worker(),       % optional
  %%modules => modules()}   % optional

%% processes need to be instantiated by builder before;

%% process_flag(trap_exit, true), Needs to be passed by default;
