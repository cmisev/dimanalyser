REAL :: momentum ! U(kg*m/s)


PROGRAM test

    REAL :: velocity, v(1,3),speed(3) !U(m/s)
    REAL :: energy,energy2 !U(J)
    REAL, DIMENSION(3) :: force !U(N)
    REAL :: mass !U(kg)


    energy = 0.5*mass*velocity**2

    CALL testsubroutine(energy,mass,speed)

    IF (energy2.eq.0.5*mass*velocity**2) GOTO 11

11: IF(energy2.gt.energy) THEN
        STOP
    ELSE IF (energy2.lt.energy) THEN
        DO mass=0.0,5.0,0.1 ! U(kg)
            DO WHILE energy2.lt.energy2
                CALL testsubroutine(energy2,mass,speed)
            END DO
        END DO
    ELSE
        energy2 = 0.5*mass*velocity**2
    END IF
    STOP(1)


CONTAINS

    SUBROUTINE testsubroutine(a,b,c):
        REAL, INTENT(IN) :: a
        REAL, INTENT(IN) :: b,c ! U(kg),U(m/s)
        REAL :: d ! U(kg)

        a = b*c**2

        d = a

        DO 20 mass=0.0,5.0,0.1 ! U(J), U(kg), U(kg)
            DO WHILE energy2.lt.energy2
                CALL testsubroutine(energy2,mass,speed)
            END DO
     20 CONTINUE

    END SUBROUTINE

END PROGRAM
