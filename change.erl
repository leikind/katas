-module (change).

-export([count_change/2, count_change_demo/2]).



count_change(Combinations, CurrentLeftCoins, 0, _Coins) ->
  [CurrentLeftCoins | Combinations];
count_change(Combinations, _CurrentLeftCoins, _Money, []) ->
  Combinations;


count_change(Combinations, CurrentLeftCoins, Money, Coins = [BiggestCoin|OtherCoins]) ->
  NextAmountOfMoney = Money - BiggestCoin,
  if
    NextAmountOfMoney < 0 ->
      count_change(Combinations, CurrentLeftCoins, Money, OtherCoins);
    true ->
      count_change(Combinations, [BiggestCoin|CurrentLeftCoins], NextAmountOfMoney, Coins) ++
      count_change(Combinations, CurrentLeftCoins, Money, OtherCoins)
  end.


count_change_demo(Money, Coins) ->
  Combinations = count_change([], [], Money, lists:reverse(lists:sort(Coins))),
  lists:foreach(fun(C) -> io:format("~w~n", [C]) end, Combinations),
  ok.

count_change(Money, Coins) ->
  Combinations = count_change([], [], Money, lists:reverse(lists:sort(Coins))),
  length(Combinations).
