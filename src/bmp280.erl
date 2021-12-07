
-module(bmp280).

-include("bmp280.hrl").

-export([
    open/2,
    write/3,
    read/3,

    id/1,
    status/1,
    reset/1,
    setopt/1, setopt/2
]).

% @doc Open een connectie naar de bmp_280
open(Bus, Address) ->
    case circuits_i2c:open(Bus) of
        {ok, Ref} ->
            {ok, {bmp280, Ref, Address}};
        {error, _} = Error ->
            Error
    end.

read({bmp280, Ref, Address}, Register, NrBytes) ->
    circuits_i2c:write_read(Ref, Address, <<Register>>, NrBytes).

write({bmp280, Ref, Address}, Register, Data) ->
    circuits_i2c:write(Ref, Address, [Register, Data]).

% @doc Get the id of the chip, should be 16#58
id(Ref) ->
    {ok, <<C>>} = read(Ref, ?BMP280_ID, 1),
    C.

% @doc Get the status of the chip.
status(Ref) ->  
    {ok, <<_:4, Measuring:1, _:2, ImUpdate:1>>} = read(Ref, ?BMP280_STATUS, 1),

    #{
        measuring => Measuring =:= 1,
        im_update => ImUpdate =:= 1
    }.

% @doc Do a complete reset power on of the bmp280
reset(Ref) ->
    ok = write(Ref, ?BMP280_RESET, 16#B6).


setopt(Ref) ->
    setopt(Ref, #{ power_mode => normal, 
                   oversampling_temperature => '1x',
                   oversampling_pressure => '4x'
                 }).

setopt(Ref, #{ power_mode := Mode,
               oversampling_temperature := OvrSampT, 
               oversampling_pressure := OvrSampP }) ->
    T = oversampling(OvrSampT),
    P = oversampling(OvrSampP),
    M = power_mode(Mode),

    ok = write(Ref, ?BMP280_CTRL_MAES, <<T:3, P:3, M:2 >>).

power_mode(normal) -> 3; % device keeps sampling
power_mode(forced) -> 1; % device does one measurement and goes to sleep
power_mode(sleep) -> 0. % sleep

oversampling('1x') -> 1; % temp 16 bit / 0.0050 °C, 16 bit / 2.62 Pa
oversampling('2x') -> 2; % temp 17 bit / 0.0025 °C, 17 bit / 1.31 Pa
oversampling('4x') -> 3; % temp 18 bit / 0.0012 °C, 18 bit / 0.66 Pa
oversampling('8x') -> 4; % temp 19 bit / 0.0006 °C, 19 bit / 0.33 Pa
oversampling('16x') -> 5. % temp 20 bit / 0.0003 °C, 20 bit / 0.16 Pa

