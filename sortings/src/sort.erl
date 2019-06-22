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

hsort(List) ->
  Heap = buildMaxHeap(List,{1,{}}),
  heapToList(Heap,[]).

buildMaxHeap([],Heap) ->
  Heap;
buildMaxHeap([H|Tail],Heap) ->
buildMaxHeap(Tail,addLeaf(H,Heap)).

addLeaf(Elem,{Number,Heap}) ->
  {Number+1,addLeaf(calcPath(Number),Heap,Elem)}.

addLeaf(_,{},Elem) ->
  {Elem};
addLeaf(_D,{Parent},Elem) ->
  insertLeft({Parent,{Elem},{}});
addLeaf([l|Path],{Parent,Left,Right},Elem) ->
  insertLeft({Parent,addLeaf(Path,Left,Elem),Right});
addLeaf([r|Path],{Parent,Left,Right},Elem) ->
  insertRight({Parent,Left,addLeaf(Path,Right,Elem)}).

insertLeft({Parent,{LI},R}) when Parent < LI ->
  {LI,{Parent},R};
insertLeft({Parent,{LI,LL,LR},R}) when Parent < LI ->
  {LI,{Parent,LL,LR},R};
insertLeft(X) ->
  X.

insertRight({Parent,L,{RI}}) when Parent < RI ->
  {RI,L,{Parent}};
insertRight({Parent,L,{RI,RL,RR}}) when Parent < RI ->
  {RI,L,{Parent,RL,RR}};
insertRight(X) ->
  X.

heapToList({_,{}},Sorted) ->
  Sorted;
heapToList(Heap,Sorted) ->
  {Elem,NewHeap} = swap(Heap),
  heapToList(NewHeap,[Elem|Sorted]).

swap({2,{Elem}}) ->
  {Elem,{0,{}}};
swap({_,{Parent,_,_}}=Heap) ->
  {{NumberNew,HeapNew},Elem} = getHeapEnd(Heap),
  {Parent,{NumberNew,siftDown(setNewRoot(HeapNew,Elem))}}.

-define(v_(H,Tag),(case size(H) of
                     0 -> {0,none};
                     _ -> {element(1,H),Tag}
                   end)).

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

getHeapEnd({Number,Heap}) ->
  {Heap1,Elem} = getHeapEnd(calcPath(Number-1),Heap),
  {{Number-1,Heap1},Elem}.

getHeapEnd(_,{Elem}) ->
  {{},Elem};
getHeapEnd(_,{Parent,{Elem},{}}) ->
  {{Parent},Elem};
getHeapEnd([l|Path],{Parent,L,R}) ->
  {L1,Elem} = getHeapEnd(Path,L),
  {{Parent,L1,R},Elem};
getHeapEnd([r|D],{Parent,L,R}) ->
  {R1,Elem} = getHeapEnd(D,R),
  {{Parent,L,R1},Elem}.

setNewRoot({_P},Elem) ->
  {Elem};
setNewRoot({_P,L,R},Elem) ->
  {Elem,L,R}.

calcPath(Number) ->
  calcPath(Number,[]).
calcPath(1,Accu) ->
  Accu;
calcPath(Number,Accu) when Number rem 2 =:= 0 ->
  calcPath(Number div 2,[l|Accu]);
calcPath(Number,Accu) when Number rem 2 =/= 0 ->
  calcPath((Number-1) div 2,[r|Accu]).

%%%%%%%%%%%%%%%%%%%%%%
%% Selbst geschriebene Versionen von build in functions
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