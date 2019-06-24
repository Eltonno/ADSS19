%%%-------------------------------------------------------------------
%%% @author Florian Weiß
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2019 19:54
%%%-------------------------------------------------------------------
-module(sort).
-author("Florian Weiß").

%% API
-export([insertionS/1,qsort/3,hsort/1,last/1,droplast/1]).

%%%%%%%%%%%%%
%%
%% Insertion Sort
%%
%%%%%%%%%%%%%

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

%%%%%%%%%%%%%
%%
%% Quicksort
%%
%%%%%%%%%%%%%

qsort(_PivotMethod,[],_Change) when _Change >= 0 ->
  [];
qsort(_PivotMethod,[Single],_Change) when _Change >= 0 ->
  [Single];
qsort(PivotMethod,List,Change) when Change >= 0, length(List) > Change->
  [Pivot|T] = pivot(PivotMethod, List),
  qsort(PivotMethod,[X||X <- T, X < Pivot],Change) ++
    [Pivot] ++
    qsort(PivotMethod,[X||X <- T, X >= Pivot],Change);
qsort(_PivotMethod,List,_Change) ->
  insertionS(List).

pivot(PivotMethod,List) ->
  case PivotMethod of
    left -> List ;
    middle -> pmid(List);
    right -> pright(List);
    median ->
      Mid = pmid(List),
      Right = pright(List),
      pmed(List,Mid,Right);
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

pmed([L|_LT]=List,[M|_MT],[R|_RT]) when L >= M, L >= R ->
  List;
pmed([L|_LT],[M|_MT],[R|_RT]=List) when R >= L, R >= M ->
  List;
pmed([L|_LT],[M|_MT]=List,[R|_RT]) when M >= R, M >= L ->
  List.

%%%%%%%%%%%%%
%%
%% Heap Sort
%%
%%%%%%%%%%%%%

hsort(List) ->
  Heap = buildMaxHeap(List,{1,{}}),
  heapToList(Heap,[]).

%%%%%%%%%%%%%
%%
%% Funktionen für die Erzeugung des Heaps
%%
%%%%%%%%%%%%%

buildMaxHeap([],Heap) ->
  Heap;
buildMaxHeap([H|Tail],Heap) ->
buildMaxHeap(Tail,insertLeaf(H,Heap)).

insertLeaf(Elem,{Number,Heap}) ->
  {Number+1,insertLeaf(calcPath(Number),Heap,Elem)}.

insertLeaf(_,{},Elem) ->
  {Elem};
insertLeaf(_,{Parent},Elem) ->
  reHeap_up({Parent,{Elem},{}});
insertLeaf([l|Path],{Parent,Left,Right},Elem) ->
  reHeap_up({Parent,insertLeaf(Path,Left,Elem),Right});
insertLeaf([r|Path],{Parent,Left,Right},Elem) ->
  reHeap_up({Parent,Left,insertLeaf(Path,Right,Elem)}).

reHeap_up({Parent,{LI},R}) when Parent < LI ->
  {LI,{Parent},R};
reHeap_up({Parent,{LI,LL,LR},R}) when Parent < LI ->
  {LI,{Parent,LL,LR},R};
reHeap_up({Parent,L,{RI}}) when Parent < RI ->
  {RI,L,{Parent}};
reHeap_up({Parent,L,{RI,RL,RR}}) when Parent < RI ->
  {RI,L,{Parent,RL,RR}};
reHeap_up(Heap) ->
  Heap.

%%%%%%%%%%%%%
%%
%% Funktionen für den Sortiervorgang von Heap zur Liste
%%
%%%%%%%%%%%%%

heapToList({_,{}},Sorted) ->
  Sorted;
heapToList(Heap,Sorted) ->
  {Elem,NewHeap} = swap(Heap),
  heapToList(NewHeap,[Elem|Sorted]).

swap({2,{Elem}}) ->
  {Elem,{0,{}}};
swap({_,{Parent,_,_}}=Heap) ->
  {{NumberNew,HeapNew},Elem} = pop(Heap),
  {Parent,{NumberNew,siftDown(push(HeapNew,Elem))}}.

siftDown({Parent}) ->
  {Parent};
siftDown({Parent,Left,{}=Right}=Heap) ->
  L = element(1,Left),
  if
    Parent > L ->
      Heap;
    true ->
      {L,siftDown(setelement(1,Left,Parent)),Right}
  end;
siftDown({Parent,Left,Right}=Heap) ->
  case getMax([{Parent,top},{element(1,Left),l},{element(1,Right),r}]) of
    {_,top} ->
      Heap;
    {L,l} ->
      {L,siftDown(setelement(1,Left,Parent)),Right};
    {R,r} ->
      {R,Left,siftDown(setelement(1,Right,Parent))}
  end.

getMax([{P,_}=PT,{L,_},{R,_}]) when P >= L, P >= R ->
  PT;
getMax([{P,_},{L,_},{R,_}=RT]) when R >= L, R >= P ->
  RT;
getMax([{P,_},{L,_}=LT,{R,_}]) when L >= R, L >= P ->
  LT.

pop({Number,Heap}) ->
  {Heap1,Elem} = pop(calcPath(Number-1),Heap),
  {{Number-1,Heap1},Elem}.

pop(_,{Elem}) ->
  {{},Elem};
pop(_,{Parent,{Elem},{}}) ->
  {{Parent},Elem};
pop([l|Path],{Parent,L,R}) ->
  {L1,Elem} = pop(Path,L),
  {{Parent,L1,R},Elem};
pop([r|Path],{Parent,L,R}) ->
  {R1,Elem} = pop(Path,R),
  {R1,Elem} = pop(Path,R),
  {{Parent,L,R1},Elem}.

push({_P},Elem) ->
  {Elem};
push({_P,L,R},Elem) ->
  {Elem,L,R}.

%%%%%%%%%%%%%
%%
%% Funktion für die Bestimmungen des Pfades
%% Bereitgestellt von Prof. Klauck
%%
%%%%%%%%%%%%%

calcPath(Number) ->
  calcPath(Number,[]).
calcPath(1,Accu) ->
  Accu;
calcPath(Number,Accu) when Number rem 2 =:= 0 ->
  calcPath(Number div 2,[l|Accu]);
calcPath(Number,Accu) when Number rem 2 =/= 0 ->
  calcPath((Number-1) div 2,[r|Accu]).

%%%%%%%%%%%%%%%%%%%%%%
%% Selbst geschriebene Versionen von build in Kunktionen,
%% sowie kleinere Abwandlungen zur Bereitstellung bei den oben
%% entwickelten Funktionen
%%%%%%%%%%%%%%%%%%%%%%


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