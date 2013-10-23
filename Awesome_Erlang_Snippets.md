### TCP Server/Client
***Client***
```erlang
-module(clnt).
-author("pengzuojie@gmail.com").
-export([client/1]).
-define(PortNo, 3001).
client(Message) ->
    {ok,Sock} = gen_tcp:connect("localhost",?PortNo,[{active,false},
                                                    {packet,2}]),
    gen_tcp:send(Sock,Message),
    A = gen_tcp:recv(Sock,0),
    gen_tcp:close(Sock),
    A.
```
***Server***
```erlang
-module(svr).
-author("pengzuojie@gmail.com").
-export([start/1, start_servers/2, server/1, loop/1]).
-define(LPort, 3001).
start(Num) ->
    case gen_tcp:listen(?LPort,[{active, false},{packet,2}]) of
        {ok, ListenSock} ->
            start_servers(Num,ListenSock),
            {ok, Port} = inet:port(ListenSock),
            Port;
        {error,Reason} ->
            {error,Reason}
    end.
start_servers(0,_) ->
    ok;
start_servers(Num,LS) ->
    spawn(?MODULE,server,[LS]),
    start_servers(Num-1,LS).
server(LS) ->
    case gen_tcp:accept(LS) of
        {ok,S} ->
            loop(S),
            server(LS);
        Other ->
            io:format("accept returned ~w - goodbye!~n",[Other]),
            ok
    end.
loop(S) ->
    inet:setopts(S,[{active,once}]),
    receive
        {tcp,S,Data} ->
            Answer = "hello from svr", % Not implemented in this example
            gen_tcp:send(S,Answer),
            loop(S);
        {tcp_closed,S} ->
            io:format("Socket ~w closed [~w]~n",[S,self()]),
            ok
    end.
```
###Sieve of Eratosthenes
```erlang
-module(prime).
-compile(export_all).
range(N, N) ->
	[N];
range(Min, Max) ->
	[Min | range(Min + 1, Max)].
remove_multiples(N, [H | T]) when H rem N == 0 ->
	remove_multiples(N, T);
remove_multiples(N, [H | T]) ->
	[H | remove_multiples(N, T)];
remove_multiples(_, []) ->
	[].
sieve([H | T]) ->
	[H | sieve(remove_multiples(H, T))];
sieve([]) ->
	[].
prime(N) ->
	sieve(range(2, N)).
```
###Quick Sort
```erlang
qsort([]) -> [];   
qsort([H | T]) ->   
	qsort([X || X <- T, X =< H]) ++ [H] ++ qsort([X || X <- T, X > H]).   
```
###Full Permutations
```erlang
perms([]) -> [[]];
perms(L)  -> [[H | T] || H <- L, T <- perms(L -- [H])].
```
###Unbalance Binary Tree
```erlang
-module(binary_tree).
-compile(export_all).
tab(0) ->
	nil;
tab(D)  ->
	io:format("\t"),
	tab(D - 1).
lookup(Key, nil) ->
	not_found;
lookup(Key, {Key, V, _, _}) ->
	{fond, V};
lookup(Key, {K, _, Smaller, _}) when Key < K ->
	lookup(Key, Smaller);
lookup(Key, {K, _, _, Bigger}) when Key > K ->
	lookup(Key, Bigger).
insert(Key, Value, nil) ->
	{Key, Value, nil, nil};
% if key exists, update it
insert(Key, Value, {Key, _, Smaller, Bigger}) ->
	{Key, Value, Smaller, Bigger};
insert(Key, Value, {K, V, Smaller, Bigger}) when Key < K ->
	{K, V, insert(Key, Value, Smaller), Bigger};
insert(Key, Value, {K, V, Smaller, Bigger}) when Key > K ->
	{K, V, Smaller, insert(Key, Value, Bigger)}.
print_tree(T) ->
	print_tree(T, 0).
print_tree(nil, D) ->
	tab(D),
	io:format("nil~n", []);
print_tree({Key, Value, Smaller, Bigger}, D) ->
	D1 = D + 1,
	print_tree(Bigger, D1),
	tab(D),
	io:format("~w ===> ~w~n", [Key, Value]),
	print_tree(Smaller, D1).
delete(Key, nil) ->
	nil;
delete(Key, {Key, _, nil, nil}) ->
	nil;
delete(Key, {Key, _, Smaller, nil}) ->
	Smaller;
delete(Key, {Key, _, nil, Bigger}) ->
	Bigger;
delete(Key, {K, _, Smaller, Bigger}) when Key == K ->
	{K2, V2, Smaller2} = delete_sp(Smaller),
	{K2, V2, Smaller2, Bigger};
delete(Key, {K, V, Smaller, Bigger}) when Key < K ->
	{K, V, delete(Key, Smaller), Bigger};
delete(Key, {K, V, Smaller, Bigger}) when Key > K ->
	{K, V, Smaller, delete(Key, Bigger)}.
delete_sp({Key, Value, nil, nil}) ->
	{Key, Value, nil};
delete_sp({Key, Value, Smaller, nil}) ->
	{Key, Value, Smaller};
delete_sp({Key, Value, Smaller, Bigger}) ->
	{K2, V2, Bigger2} = delete_sp(Bigger),
	{K2, V2, {Key, Value, Smaller, Bigger2}}.
run() ->
	S1 = nil,
	S2 = insert(1, joe, S1),
	S3 = insert(2, fred, S2),
	S4 = insert(3, jane, S3),
	S5 = insert(4, kalle, S4),
	S6 = insert(5, thomas, S5),
	S7 = insert(6, rickard, S6),
	S8 = insert(7, susan, S7),
	S9 = insert(8, tobbe, S8),
	S10 = insert(9, dan, S9),
	S11 = insert(10, arvin, S10),
	S12 = insert(11, den, S11),
	S13 = insert(12, vn, S12),
	S14 = insert(13, in, S13),
	S15 = insert(14, rn, S14),
	S16 = insert(15, rnn, S15),
	print_tree(S16).
```
