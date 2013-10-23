###Quick Sort
```erlang
qsort([]) -> [];   
qsort([H | T]) ->   
	qsort([X || X <- T, X =< H]) ++ [H] ++ qsort([X || X <- T, X > H]).   
```
