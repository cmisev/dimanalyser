REAL :: momentum ! U(kg*m/s)


PROGRAM test

    REAL :: velocity, v(1,3),speed(3) !U(m/s)
    REAL :: energy !U(J)
    REAL, DIMENSION(3) :: force !U(N)
    REAL :: mass !U(kg)

    energy = mass*velocity**2

    !CALL testsubroutine(energy,mass,velocity)

CONTAINS

    SUBROUTINE testsubroutine(a,b,c):
        REAL, INTENT(IN) :: a,b,c ! U(J),U(kg),U(m/s)

        a = b*c**2

    END SUBROUTINE

END PROGRAM
