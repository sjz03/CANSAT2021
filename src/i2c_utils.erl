

-module(i2c_utils).

-export([
    detect_devices/1
]).

% @doc Detect devices on the i2c bus. Returns a list with detected
% device addresses.
% 
% Example: i2c_utils:detect_devices("i2c-1").
detect_devices(Bus) ->
    {ok, Ref} = circuits_i2c:open(Bus),
    try
        AddressRange = lists:seq(3, 127),
        lists:filter(fun(A) -> 
                        is_read_possible(Ref, A) 
                    end, AddressRange)
    after
        circuits_i2c:close(Ref)
    end.

%%
%% Helpers
%% 

is_read_possible(Ref, Address) ->
    case circuits_i2c:read(Ref, Address, 1) of
        {ok, _} -> true;
        {error, _} -> false
    end.