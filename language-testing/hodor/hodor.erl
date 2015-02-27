-module(main).
-export([main/1]).

main([]) ->   hodor_loop();
main(Args) -> io:fwrite("~s~n", [string:join(hodorize_list(Args), " ")]).

hodor_loop() ->
  case io:get_line("") of
    eof -> ok;
    String ->
      io:fwrite("~s", [hodorize_sentence(String)]),
      hodor_loop();
    _ -> ok
  end.

hodorize_sentence(String) ->
  string:join(hodorize_list(string:tokens(String, " ")), " ").

hodorize_list([Head|Tail]) -> [hodorize_word(Head) | hodorize_list(Tail)];
hodorize_list([]) -> [].

hodorize_word(Word) ->
  re:replace(Word, "[a-zA-Z0-9]+", "hodor", [{return, list}]).
