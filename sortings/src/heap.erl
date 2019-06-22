-module(heap).

-export([insert/2, append/2,
  %%min/1,max/1,
  %%take_min/1,take_max/1,
  from_list/1,
  to_list/1, to_list/2
  ]).

insert({Size,Heap},Item) ->
  {Size+1,insert(calcPath(Size),Heap,Item)}.


insert(_D,{},Item) ->
  {Item};
insert(_D,{Parent},Item) ->
  siftl({Parent,{Item},{}});
insert([l|D],{Parent,Left,Right}=_H,Item) ->
  siftl({Parent,insert(D,Left,Item),Right});
insert([r|D],{Parent,Left,Right}=_H,Item) ->
  siftr({Parent,Left,insert(D,Right,Item)}).

append(Heap,[]) ->
  Heap;
append(Heap,[A|As]) ->
  append(insert(Heap,A),As).


from_list(L) ->
  append({1,{}},L).

to_list(H) ->
  %lists:reverse(
    to_list(H,[])
%)
.

to_list({_,{}},Out) ->
  Out;
to_list(H,Out) ->
  erlang:display(H),
  {Item,H1} = most(H),
  to_list(H1,[Item|Out]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Internal Functions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(ss(Fun,H,Item),Fun(setelement(1,H,Item))).
-define(v_(H,Tag),(case size(H) of
                     0 -> {0,none};
                     _ -> {element(1,H),Tag}
                   end)).

most({2,{I}}) ->
  {I,{0,{}}};
most({_Size,{P,_,_}}=H) ->
  {{Size1,Heap},Bottom} = take_bottom(H),
  {P,{Size1,siftdown(replace_top(Heap,Bottom))}}
.

siftl({Parent,{LI},R}) when Parent < LI ->
  {LI,{Parent},R};
siftl({Parent,{LI,LL,LR},R}) when Parent < LI ->
  {LI,{Parent,LL,LR},R};
siftl(X) ->
  X.

siftr({Parent,L,{RI}}) when Parent < RI ->
  {RI,L,{Parent}};
siftr({Parent,L,{RI,RL,RR}}) when Parent < RI ->
  {RI,L,{Parent,RL,RR}};
siftr(X) ->
  X.

siftdown({P}) ->
  {P};
siftdown({P,L,R}=Heap) ->
  case getMax([{P,top},?v_(L,l),?v_(R,r)]) of
    {_,top} ->
      Heap;
    {V,l} ->
      {V,siftdown(setelement(1,L,P)),R};
    {V,r} ->
      {V,L,siftdown(setelement(1,R,P))}
  end.

getMax([{P,_}=PT,{L,_},{R,_}]) when P >= L, P >= R ->
  PT;
getMax([{P,_},{L,_},{R,_}=RT]) when R >= L, R >= P ->
  RT;
getMax([{P,_},{L,_}=LT,{R,_}]) when L >= R, L >= P ->
  LT.

take_bottom({Number,Heap}) ->
  {Heap1,Item} = take_bottom(calcPath(Number-1),Heap),
  {{Number-1,Heap1},Item}.

take_bottom(_D,{Item}) ->
  {{},Item};
take_bottom(_D,{Parent,{Item},{}}) ->
  {{Parent},Item};
take_bottom([l|D],{Parent,L,R}) ->
  {L1,Item} = take_bottom(D,L),
  {{Parent,L1,R},Item};
take_bottom([r|D],{Parent,L,R}) ->
  {R1,Item} = take_bottom(D,R),
  {{Parent,L,R1},Item}.

replace_top({_P},Item) ->
  {Item};
replace_top({_P,L,R},Item) ->
  {Item,L,R}.

calcPath(Number) ->
  calcPath(Number,[]).
calcPath(1,Accu) ->
  Accu;
calcPath(Number,Accu) when Number rem 2 =:= 0 ->
  calcPath(Number div 2,[l|Accu]);
calcPath(Number,Accu) when Number rem 2 =/= 0 ->
  calcPath((Number-1) div 2,[r|Accu]).

