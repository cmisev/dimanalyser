REAL :: momentum ! U(kg*m/s)


PROGRAM test

    REAL :: velocity, v(1,3),speed(3) !U(m/s)
    REAL :: energy
    REAL, DIMENSION(3) :: force !U(N)
    REAL :: mass !U(kg)

    energy = mass*velocity**2

    SUBROUTINE testsubroutine(a,b,c):
    END SUBROUTINE

END PROGRAM
