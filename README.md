dimanalyser
===========

A dimensional analysis software of physical units. The purpose of this tool is to analyse variable assignments and mathematical expressions to check whether the physical units are compatible.  If there is an error, a line number where the error occurred is displayed, as well as a traceback of where the incompatible variables were assigned their physical unit.


Unit definition by comment or deduction from mathematical expressions
---------------------------------------------------------------------

```
real :: distance, mass, time ! U(m) U(kg) U(s)
```

defines variable `distance` as meters, `mass` as kilograms and `time` as seconds. The analyser infers the units where it was not given as comments, for example, when

```
real :: force

force = mass*distance/time**2
```

then `force` is handled as a variable holding a value in Newton. Likewise,

```
real :: time_spent, distance ! U(s) U(m)
real :: velocity

distance = velocity*time_spent
```

will define `velocity` as holding a value in units `m/s`.


Scope resolution and subroutine calls
-------------------------------------

The analyser is capable of distinguishing variables of same names as defined in different scopes, as well as passes the units on to subroutines where applicable.


Extensive example
-----------------

See `res/test/fortran/` for an example. `dimanalyser testfortran.f90` reads the file `testfortran.f90` and checks the mathematical expressions for consistency errors. The full output of the analysis is given in `output.txt`.