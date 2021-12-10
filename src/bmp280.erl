
-module(bmp280).

-include("bmp280.hrl").

-export([
    open/2,
    write/3,
    read/3,

    id/1,
    status/1,
    reset/1,
    setopt/1, setopt/2,

    read_calibration/1,
    calibration/1,

    read_raw_sample/1,
    measurement_from_raw_sample/2
]).

-record(bmp280_calib, {
    dig_t1,
    dig_t2,
    dig_t3,
    dig_p1,
    dig_p2,
    dig_p3,
    dig_p4,
    dig_p5,
    dig_p6,
    dig_p7,
    dig_p8,
    dig_p9
}).


%%
%% Low-level Interface
%% 

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

read_calibration(Ref) ->
    read(Ref, ?BMP280_CALIB00, 24).

read_raw_sample(Ref) ->
    read(Ref, ?BMP280_PRESS_MSB, 6).

measurement_from_raw_sample(RawSample, Calibration) ->
    <<RawPressure:20, _:4, RawTemperature:20, _:4>> = RawSample,
    Temperature = raw_to_temperature(RawTemperature, Calibration),
    Pressure = raw_to_pressure(RawPressure, Temperature, Calibration),

    #{ temperature => Temperature,
       pressure => Pressure
     }.

raw_to_temperature(RawTemp, #bmp280_calib{dig_t1=T1, dig_t2=T2, dig_t3=T3}) ->
    Var1 = (RawTemp / 16384 - T1 / 1024) * T2,
    Var2 = (RawTemp / 131072 - T1 / 8192) * (RawTemp / 131072 - T1 / 8192) * T3,
    (Var1 + Var2) / 5120.

raw_to_pressure(RawPressure, Temperature,
                #bmp280_calib{dig_p1=P1, dig_p2=P2, dig_p3=P3,
                              dig_p4=P4, dig_p5=P5, dig_p6=P6,
                              dig_p7=P7, dig_p8=P8, dig_p9=P9}) ->
    TFine = Temperature * 5120,

    V1 = TFine / 2 - 64000,

    V2 = V1 * V1 * P6 / 32768,
    V3 = V2 + V1 * P5 * 2,
    V4 = V3 / 4 + P4 * 65536,

    V5 = (P3 * V1 * V1 / 524288 + P2 * V1) / 524288,
    V6 = (1 + V5 / 32768) * P1,

    Pr = 1048576 - RawPressure,
    Pr1 = (Pr - V4 / 4096) * 6250 / V6,
    
    V7 = P9 * Pr1 * Pr1 / 2147483648,
    V8 = Pr1 * P8 / 32768,
    
    Pr2 = Pr1 + (V7 + V8 + P7) / 16,

    Pr2.

calibration(Data) ->
    <<T1:16/little-unsigned, T2:16/little-signed, T3:16/little-signed,
      P1:16/little-unsigned, P2:16/little-signed, P3:16/little-signed,
      P4:16/little-signed, P5:16/little-signed, P6:16/little-signed, 
      P7:16/little-signed, P8:16/little-signed, P9:16/little-signed>> = Data,

      #bmp280_calib{
          dig_t1=T1, dig_t2=T2, dig_t3=T3,
          dig_p1=P1, dig_p2=P2, dig_p3=P3,
          dig_p4=P4, dig_p5=P5, dig_p6=P6,
          dig_p7=P7, dig_p8=P8, dig_p9=P9
      }.
      
%%
%% Helpers
%% 

power_mode(normal) -> 3; % device keeps sampling
power_mode(forced) -> 1; % device does one measurement and goes to sleep
power_mode(sleep) -> 0. % sleep

oversampling('1x') -> 1; % temp 16 bit / 0.0050 °C, 16 bit / 2.62 Pa
oversampling('2x') -> 2; % temp 17 bit / 0.0025 °C, 17 bit / 1.31 Pa
oversampling('4x') -> 3; % temp 18 bit / 0.0012 °C, 18 bit / 0.66 Pa
oversampling('8x') -> 4; % temp 19 bit / 0.0006 °C, 19 bit / 0.33 Pa
oversampling('16x') -> 5. % temp 20 bit / 0.0003 °C, 20 bit / 0.16 Pa

