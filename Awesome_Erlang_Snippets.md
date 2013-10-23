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
