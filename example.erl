-module(example).
-export([f/1]).
f(Float) -> float_to_list(Float, [compact, {decimals, 20}]).
