-module(main).
-export([main/1]).

main([Hostname, Port]) ->
  {ok, Sock} = gen_tcp:connect(Hostname, list_to_integer(Port), [binary]),
  loop_send(Sock),
  gen_tcp:close(Sock);

main(_) ->
  io:fwrite("Usage: hostname port~n", []).

loop_send(Sock) ->
  case io:get_line("") of
    eof -> eof;
    String ->
      gen_tcp:send(Sock, String),
      case gen_tcp:recv(Sock, 0) of
        {ok, RecvData} -> io:fwrite("~s~n", [RecvData]);
        {error, Reason} -> io:fwrite("~p~n", [Reason])
      end
  end;
