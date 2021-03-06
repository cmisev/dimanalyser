! Test unit declarations
!========================

BLOCK test_unitdeclarations

    REAL :: a,b,c ! U((J*s)/s^2) U(kg) U(s) U(m)
    REAL :: a,b,c ! U(1) U(kg)
    REAL :: a,b,c ! U(kg)

END BLOCK

! Declare subroutine before calling it, declared parameter units
!===============================================================

SUBROUTINE test_before_call_dpu(a,b,c)
    REAL, INTENT(IN) :: a,b,c ! U(m) U(kg) U(s)

    REAL :: d,e,f,g,h

    d = a
    e = d
    f = e

    b = f


END SUBROUTINE




PROGRAM test

    REAL :: xmin,dx ! U(m)
    REAL :: xmax
    REAL :: x1,x2, x3,x4
    REAL :: mass,time ! U(kg) U(s)

    x1 = x2

    DO x1=xmin,xmax,dx

        x2 = x1
        x1 = x3

        DO WHILE (x2 .gt. 0)
            x2 = x2 - 0.5 - 0.1 + 3.7! U(m) U(m)
            CALL test_before_call_dpu(x2,mass,time)
        END DO

        IF (x2.eq. 0) CALL test_after_call_upu(x1,mass,time)  ! U(m)

        IF (x1.ge. 0.5) THEN ! U(m)
            CALL test_after_call_upu(x4,mass,time)
        ELSE IF (x1 .ne. 0.5) THEN !U(m)
        END IF
    END DO

CONTAINS

    SUBROUTINE test_after_call_upu(a,b,c)
        REAL :: a,b,c ! U(kg)

        a = b**2/c
    END SUBROUTINE

    SUBROUTINE test_no_parameters()
        REAL :: energy !U(J)
        REAL :: mass ! U(kg)
        REAL :: velocity

        energy = 0.5*mass*velocity**2

    END SUBROUTINE

    SUBROUTINE test_no_parameters_nobraces
    END SUBROUTINE

END PROGRAM
