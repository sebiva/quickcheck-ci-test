-module(test).
-compile(export_all).
-compile({parse_transform, eqc_cover}).

-include_lib("eqc/include/eqc.hrl").

prop_test() ->
  ?FORALL(Xs, list(int()),
          ?FORALL(Ys, list(int()),
                  aggregate([length(Xs++Ys)],
                            Xs ++ Ys == lists:reverse(lists:reverse(Xs ++ Ys))))).
