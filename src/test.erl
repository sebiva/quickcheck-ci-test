-module(test).
-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

prop_test() ->
  ?FORALL(Xs, list(int()),
          ?FORALL(Ys, list(int()),
                  aggregate([length(Xs++Ys)],
                            Xs ++ Ys == lists:reverse(lists:reverse(Xs ++ Ys))))).