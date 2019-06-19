%%%-------------------------------------------------------------------
%%% @author Elton
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2019 19:54
%%%-------------------------------------------------------------------
-module(sort).
-author("Elton").

%% API
-export([insertionS/1,qsort/3,hsort/1,last/1,droplast/1]).

insertionS([]) ->
  [];
insertionS([H]) ->
  [H];
insertionS([H|L]) ->
  insertionS([H],L).

insertionS(Sorted,[]) ->
  Sorted;
insertionS(Sorted, [H|T]) ->
  insertionS(insert([],Sorted,H), T).

insert(Sorted,[],Elem) ->
  Sorted++[Elem];
insert(Sorted,[H|T],Elem) ->
  if
    Elem =< H -> Sorted++[Elem]++[H]++T;
    true -> insert(Sorted++[H], T, Elem)
  end.

qsort(_PivotMethod,[],_Change) ->
  [];
qsort(_PivotMethod,[Single],_Change) ->
  [Single];
qsort(PivotMethod,List,Change) when length(List) > Change->
  [Pivot|T] = pivot(PivotMethod, List),
  qsort(PivotMethod,[X||X <- T, X < Pivot],Change) ++
    [Pivot] ++
    qsort(PivotMethod,[X||X <- T, X >= Pivot],Change);
qsort(_PivotMethod,List,_Change) ->
  insertionS(List).

pivot(PivotMethod,[H|_T] = List) ->
  case PivotMethod of
    left -> List ;
    middle -> pmid(List);
    right -> pright(List);
    median ->
      Mid = pmid(List),
      Right = pright(List),
      pmed(H,Mid,Right,List);
    random -> prand(List)
  end
  .

pmid(List) ->
  Len = length(List),
  p_nth(Len - Len div 2, List, [])
  .

p_nth(1,[H|Tail],Front) ->
  [H|append(Front,Tail)];
p_nth(Pos,[H|Tail],Front) ->
  p_nth(Pos - 1, Tail, append(Front,H)).

pright(List) ->
  [H|T] = reverse(List),
  [H|reverse(T)].

prand(List) ->
  Len = length(List),
  p_nth(rand:uniform(Len),List,[])
  .

pmed(L,M,R,List) when L >= M, M >= R ->
  pmid(List);
pmed(L,M,R,List) when L >= R, R >= M ->
  pright(List);
pmed(L,M,R,List) when M >= R, R >= L ->
  pright(List);
pmed(L,M,R,List) when M >= L, L >= R->
  List;
pmed(L,M,R,List) when R >= L, L >= M ->
  List;
pmed(L,M,R,List) when R >= M, M >= L ->
  pmid(List).

%%qsort_r([]) ->
%%  [];
%%qsort_r(List) ->
%%  %Pivot = last(List),
%%  %SortList = droplast(List),
%%  [Pivot|RL] = reverse(List),
%%  qsort_r([X||X <- reverse(RL), X < Pivot]) ++
%%    [Pivot] ++
%%    qsort_r([X||X <- reverse(RL), X >= Pivot]).

hsort(_Arg0) ->
  erlang:error(not_implemented).

droplast(List) -> droplast([],List).
droplast(List,[_H]) -> List;
droplast(List,[H|T]) -> droplast(List++[H],T).

last([H]) -> H;
last([_|T]) -> last(T).

reverse([]) -> [];
reverse(L) -> reverse([],L).
reverse(Rev,[H]) ->[H]++Rev;
reverse(Rev,[H|T]) -> reverse([H]++Rev,T).

append([], []) ->
  [];
append([],[H|T]) ->
  [H|T];
append([],Elem) ->
  [Elem];
append([H|T], []) ->
  [H|T];
append(Elem, []) ->
  [Elem];
append([H1|T1],[H2|T2]) ->
  append([H1|T1] ++ [H2], T2);
append([H|T],Elem) ->
  [H|T] ++ [Elem];
append(L,[H|T]) ->
  append([L] ++ [H], T);
append(E1,E2) ->
  [E1] ++ [E2].