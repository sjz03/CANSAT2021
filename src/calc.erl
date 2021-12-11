%%
%% Utility functions.
%% 

-module(calc).

-export([pressure_to_altitude/2, sea_level_pressure/2]).

%% @doc Derive the height from the pressure and the current sea level pressure
pressure_to_altitude(Pressure, SeaLevelPressure) ->
    44330 * (1 - math:pow(Pressure / SeaLevelPressure, 1 / 5.255)).

%% @doc Calculate the current sea level pressure from a known altitude and
%%      the current pressure.
sea_level_pressure(Pressure, Altitude) ->
    Pressure / math:pow(1 - Altitude / 44330, 5.255).