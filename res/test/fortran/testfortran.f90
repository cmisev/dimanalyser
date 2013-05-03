! Test unit declarations
!========================

BLOCK test_unitdeclarations

    REAL :: a,b,c ! U(1) U(kg) U(s) U(m)
    REAL :: a,b,c ! U(1) U(kg)
    REAL :: a,b,c ! U(kg)

END BLOCK

! Declare subroutine before calling it, declared parameter units
!===============================================================

SUBROUTINE test_before_call_dpu(a,b,c)
    REAL, INTENT(IN) :: a,b,c ! U(m) U(kg) U(s)

END SUBROUTINE




PROGRAM test

    REAL :: xmin,xmax,dx ! U(m)
    REAL :: x1,x2
    REAL :: mass,time ! U(kg) U(s)

    DO x1=xmin,xmax,dx

        x2 = x1
        DO WHILE (x2 .gt. 0)
            x2 = x2 - 0.5 ! U(m)
            CALL test_before_call_dpu(x2,mass,time)
        END DO

        IF (x2.eq. 0) CALL test_after_call_upu(x1,mass,time)  ! U(m)

        IF (x1.gt. 0.5) THEN ! U(m)
        ELSE IF (x1 .eq. 0.5) THEN !U(m)
        END IF
    END DO

CONTAINS

    SUBROUTINE test_after_call_upu(a,b,c)
        REAL :: a,b,c ! U(kg)
    END SUBROUTINE

    SUBROUTINE test_no_parameters()
    END SUBROUTINE

    SUBROUTINE test_no_parameters_nopraces
    END SUBROUTINE

END PROGRAM
