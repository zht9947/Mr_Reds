#INCLUDE 'MR_H_ALIGN_PADDING.H'
!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE)
!
! PURPOSE:
!
!   TO
!
! DEFINITION OF VARIABLES:
!
!
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_MOD_GEN_INI_ZB

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_GEN_INI_ZB

    PUBLIC :: ERROR_BISECT_SOLVE_NO_UNIQUE_ROOT_IN_REGION ,   &
    & ERROR_NEWTON_SOLVE_ZERO_DERIVATIVE ,   &
    & ERROR_NEWTON_SOLVE_MAX_NUMBER_OF_ITERATION

    INTEGER , PARAMETER :: ERROR_BISECT_SOLVE_NO_UNIQUE_ROOT_IN_REGION = -2001

    INTEGER , PARAMETER :: ERROR_NEWTON_SOLVE_ZERO_DERIVATIVE          = -3001
    INTEGER , PARAMETER :: ERROR_NEWTON_SOLVE_MAX_NUMBER_OF_ITERATION  = -3002

!***********************************************************************************************************************************

  CONTAINS

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE)
!
! PURPOSE:
!
!   TO
!
! DEFINITION OF VARIABLES:
!
!
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_GEN_INI_ZB( NI , NJ , ZB )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) :: ZB

    INTEGER(IJID_KIND) :: I , J

    !BLOCK
      DO I = 1 , NI
        CALL MR_CALC_EXP_COEFFIS( RTH( I ) , ZBL( I ) , ZBR( I ) , ALPHA( I ) , BETA( I ) , GAMA( I ) )
      END DO
    !END BLOCK

    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        ZB( I , J ) = GAMA( I ) - BETA( I ) * EXP( - ALPHA( I ) * ( NJ - J + 0.5 ) / NJ )
      END DO
    END DO

  END SUBROUTINE MR_GEN_INI_ZB

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE)
!
! PURPOSE:
!
!   TO
!
! DEFINITION OF VARIABLES:
!
!
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_CALC_EXP_COEFFIS( RTH , ZBL , ZBR , ALPHA , BETA , GAMA )
   !DIR$ ATTRIBUTES VECTOR :: MR_CALC_EXP_COEFFIS
    IMPLICIT NONE

    REAL   (FDRD_KIND) , INTENT(IN ) :: RTH
    REAL   (FDRD_KIND) , INTENT(IN ) :: ZBL , ZBR

    REAL   (CARD_KIND) , INTENT(IN ) :: ALPHA , BETA , GAMA

    REAL   (CARD_KIND) , SAVE :: ALPHA_MIS = -8.001 , ALPHA_PLS = +10.001


!***********************************************************************************************************************************

  CONTAINS

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE)
!
! PURPOSE:
!
!   TO
!
! DEFINITION OF VARIABLES:
!
!
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  FUNCTION MR_BISECT_SOLVE_NONLINEAR_EQN( FUNC , X_LBOUND , X_RBOUND , EPS , R , ERROR ) RESULT( X )
   !DIR$ ATTRIBUTES VECTOR :: MR_BISEC_SOLVE_NONLINEAR_EQN
    IMPLICIT NONE

    REAL   (CARD_KIND) :: X

    INTERFACE
      REAL   (CARD_KIND) FUNCTION FUNC( X )
       !DIR$ ATTRIBUTES VECTOR :: FUNC
        REAL   (CARD_KIND) , INTENT(IN ) :: X
      END FUNCTION FUNC
    END INTERFACE

    REAL   (CARD_KIND) , INTENT(IN ) :: X_LBOUND , X_RBOUND

    REAL   (CARD_KIND) , INTENT(IN ) :: EPS , R

    INTEGER            , INTENT(OUT) :: ERROR

    REAL   (CARD_KIND) :: X_LAPPRX , X_RAPPRX , X_MIDDLE
    REAL   (CARD_KIND) :: Y_LAPPRX , Y_RAPPRX , Y_MIDDLE

    X_LAPPRX = X_LBOUND ; Y_LAPPRX = FUNC( X_LAPPRX )
    X_RAPPRX = X_RBOUND ; Y_RAPPRX = FUNC( X_RAPPRX )

    IF( Y_LAPPRX * Y_RAPPRX > 0 ) THEN
      X = HUGE(X)
      ERROR = ERROR_BISECT_SOLVE_NO_UNIQUE_ROOT_IN_REGION
      RETURN
    END IF

    DO WHILE( ABS( X_RAPPRX - X_LAPPRX ) > EPS )
      X_MIDDLE = 0.5 * ( X_LAPPRX + X_RAPPRX ) ; Y_MIDDLE = FUNC( X_MIDDLE )

      IF( ABS( Y_MIDDLE ) <= R ) THEN
        EXIT
      ELSE IF( Y_MIDDLE * Y_LAPPRX < 0 ) THEN
        X_RAPPRX = X_MIDDLE ; Y_RAPPRX = Y_MIDDLE
      ELSE
        X_LAPPRX = X_MIDDLE ; Y_LAPPRX = Y_MIDDLE
      END IF

    END DO

    X = 0.5 * ( X_LAPPRX + X_RAPPRX )
    STATUS = .TRUE.

  END FUNCTION MR_BISECT_SOLVE_NONLINEAR_EQN

  END SUBROUTINE MR_CALC_EXP_COEFFIS

  END MODULE MR_MOD_GEN_INI_ZB