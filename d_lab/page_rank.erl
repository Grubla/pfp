%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%% This implements a page rank algorithm using map-reduce
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
-module(page_rank).
-compile(export_all).
%% Use map_reduce to count word occurrences
map(Url,ok) ->
  dets:open_file(web,[{file,"web.dat"}]),
  [{Url,Body}] = dets:lookup(web,Url),
  Urls = crawl:find_urls(Url,Body),
  [{U,1} || U <-Urls].

reduce(Url,Ns) -> 
  %io:format(user, "I am ~p~n", [self()]),
  [{Url,lists:sum(Ns)}].

%% 188 seconds
page_rank() ->
  dets:open_file(web,[{file,"web.dat"}]),
  Urls = dets:foldl(fun({K,_},Keys) -> [K|Keys] end,[],web),
  map_reduce:map_reduce_seq(fun map/2, fun reduce/2, [{Url,ok} || Url <- Urls]).
  
%% 86 seconds
page_rank_par() ->
  dets:open_file(web,[{file,"web.dat"}]),
  Urls = dets:foldl(fun({K,_},Keys) -> [K|Keys] end,[],web),
  map_reduce:map_reduce_par(fun map/2, 32, fun reduce/2, 32,[{Url,ok} || Url <- Urls]).

page_rank_dist() ->
  dets:open_file(web,[{file,"web.dat"}]),
  Urls = dets:foldl(fun({K,_},Keys) -> [K|Keys] end,[],web),
  map_reduce:map_reduce_dist(fun map/2, 32, fun reduce/2, 32,[{Url,ok} || Url <- Urls]).

benchmark() ->
  %{Seq,_}   = timer:tc(?MODULE,page_rank,[]),
  %{Par,_}   = timer:tc(?MODULE,page_rank_par,[]),
  io:format("sista startar nu"),
  {_,Output} = timer:tc(?MODULE,page_rank_dist,[]),
  file:write_file("outputfile2.dat", io_lib:fwrite("~p.\n", [Output])).
  %{Seq, Par, Dist}.


