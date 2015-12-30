-module(tools).
-export([setnth/3, createx/2, get_random_string/1]).

%% setnth(Index, List, NewElement) -> List.
setnth(1, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].
%% Can add following caluse if you want to be kind and allow invalid indexes.
%% I wouldn't!
%% setnth(_, [], New) -> New.

createx(Init, Length) -> [Init || _ <- lists:seq(1, Length)].


get_random_string(Length) -> get_random_string(Length, "abcdefghijklmnopqrstuvwxyz1234567890").

get_random_string(Length, AllowedChars) ->
  lists:foldl(fun(_, Acc) ->
                      [lists:nth(random:uniform(length(AllowedChars)),
                                 AllowedChars)]
                          ++ Acc
              end, [], lists:seq(1, Length)).