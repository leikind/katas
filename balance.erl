-module (balance).

-export([balance/1]).

balance(CharList) -> balance(0, CharList).

balance(-1, _) -> false;
balance(0, []) -> true;
balance(_, []) -> false;
balance(Counter, [$(|T]) -> balance(Counter + 1, T);
balance(Counter, [$)|T]) -> balance(Counter - 1, T);
balance(Counter, [_|T])  -> balance(Counter, T).
