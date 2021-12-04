
-module(bmp280).

-export([
    open/2,
    write/3,
    read/3
]).

% @doc Open een connectie naar de bmp_280
open(Bus, Address) ->
    case circuit_i2c:open(Bus) of
        {ok, Ref} ->
            {ok, {bmp280, Ref, Address}};
        {error, _} = Error ->
            Error
    end.

read({bmp280, Ref, Address}, Register, NrBytes) ->
    circuits_i2c:write_read(Ref, Address, <<Register>>, NrBytes).

write({bmp280, Ref, Address}, Register, Data) ->
    circuits_i2c:write(Ref, Address, [Register, Data]).