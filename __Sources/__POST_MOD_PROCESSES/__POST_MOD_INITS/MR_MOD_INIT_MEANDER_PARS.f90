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
  MODULE MR_MOD_INIT_MEANDER_PARS

    USE MR_ERRORS_NONLINEAR_EQN_SOLVE

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_CURVED_GEOS

    USE MR_DEF_MEANDER_PARS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_INIT_MEANDER_PARS

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
  SUBROUTINE MR_INIT_MEANDER_PARS( ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    REAL   (GJRD_KIND) :: LAMBTH_BK_L_HALF , LAMBTH_BK_R_HALF
    REAL   (GJRD_KIND) :: LTH_BK_L_HALF , LTH_BK_R_HALF

    REAL   (GJRD_KIND) :: BEND_AREA

    INTEGER(IJID_KIND) :: I , J

    LAMBTH_BK_R_HALF = 0.0
    J = 0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        LAMBTH_BK_R_HALF = LAMBTH_BK_R_HALF + JVV( I , J ,1,1)
      END DO
    !END J = 0
    LAMBTH_BK_L_HALF = 0.0
    J = NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        LAMBTH_BK_L_HALF = LAMBTH_BK_L_HALF + JVV( I , J ,1,1)
      END DO
    !END J = NJ
    LAMBTH = ( LAMBTH_BK_L_HALF + LAMBTH_BK_R_HALF ) / NBENDS

    LTH_BK_R_HALF = 0.0
    J = 0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        LTH_BK_R_HALF = LTH_BK_R_HALF + SQRT( GVV( I , J ,1,1) )
      END DO
    !END J = 0
    LTH_BK_L_HALF = 0.0
    J = NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        LTH_BK_L_HALF = LTH_BK_L_HALF + SQRT( GVV( I , J ,1,1) )
      END DO
    !END J = NJ
    LTH = ( LTH_BK_L_HALF + LTH_BK_R_HALF ) / NBENDS

    SINUOSITY = LTH / LAMBTH

    CALL MR_BISECT_SOLVE_FEQ_THETA0( SINUOSITY , THETA0 , 0.0_GJRD_KIND , 2.4048255_GJRD_KIND , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in initializing equivalent Theta0 from Sinuosity"
      RETURN
    ELSE IF( LTH_BK_L_HALF < LTH_BK_R_HALF ) THEN
      THETA0 = - THETA0
    END IF

    BEND_AREA = 0.0
    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        BEND_AREA = BEND_AREA + MW( I , J )
      END DO
    END DO
    BEND_AREA = BEND_AREA / NBENDS

    BTH = 2.0 * BEND_AREA / LTH
    LAMBTH2BTH = LAMBTH / BTH

  END SUBROUTINE MR_INIT_MEANDER_PARS

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
  SUBROUTINE MR_BISECT_SOLVE_FEQ_THETA0( SINUOSITY , THETA0 , THETA0_LBOUND , THETA0_RBOUND , ERROR )

    IMPLICIT NONE

    REAL   (GJRD_KIND) , INTENT(IN ) :: SINUOSITY

    REAL   (GJRD_KIND) , INTENT(OUT) :: THETA0

    REAL   (GJRD_KIND) , INTENT(IN ) :: THETA0_LBOUND , THETA0_RBOUND

    REAL   (GJRD_KIND) , PARAMETER   :: EPS_THETA0 = 10.0*EPSILON(EPS_THETA0)
    REAL   (CARD_KIND) , PARAMETER   :: EPS_FEQ_THETA0 = 10.0*EPSILON(EPS_FEQ_THETA0)

    INTEGER            , INTENT(OUT) :: ERROR

    REAL   (GJRD_KIND) :: THETA0_LAPPRX , THETA0_RAPPRX , THETA0_MIDDLE
    REAL   (CARD_KIND) :: FEQ_THETA0_LAPPRX , FEQ_THETA0_RAPPRX , FEQ_THETA0_MIDDLE

    THETA0_LAPPRX = THETA0_LBOUND
    FEQ_THETA0_LAPPRX = MR_FUNC_FEQ_THETA0( SINUOSITY , THETA0_LAPPRX )
    IF( ABS( FEQ_THETA0_LAPPRX ) <= EPS_FEQ_THETA0 ) THEN
      THETA0 = THETA0_LAPPRX
      ERROR = 0
      RETURN
    END IF
    THETA0_RAPPRX = THETA0_RBOUND
    FEQ_THETA0_RAPPRX = MR_FUNC_FEQ_THETA0( SINUOSITY , THETA0_RAPPRX )
    IF( ABS( FEQ_THETA0_RAPPRX ) <= EPS_FEQ_THETA0 ) THEN
      THETA0 = THETA0_RAPPRX
      ERROR = 0
      RETURN
    END IF

    IF( FEQ_THETA0_LAPPRX * FEQ_THETA0_RAPPRX > 0 ) THEN
      THETA0 = HUGE(THETA0)
      ERROR = ERROR_BISECT_SOLVE_NO_UNIQUE_ROOT_IN_REGION
      RETURN
    END IF

    DO WHILE( 0.5 * ABS( THETA0_RAPPRX - THETA0_LAPPRX ) > EPS_THETA0 )

      THETA0_MIDDLE = 0.5 * ( THETA0_LAPPRX + THETA0_RAPPRX )
      FEQ_THETA0_MIDDLE = MR_FUNC_FEQ_THETA0( SINUOSITY , THETA0_MIDDLE )

      IF( ABS( FEQ_THETA0_MIDDLE ) <= EPS_FEQ_THETA0 ) THEN
        EXIT
      ELSE IF( FEQ_THETA0_LAPPRX * FEQ_THETA0_MIDDLE < 0 ) THEN
        THETA0_RAPPRX = THETA0_MIDDLE
        FEQ_THETA0_RAPPRX = FEQ_THETA0_MIDDLE
      ELSE
        THETA0_LAPPRX = THETA0_MIDDLE
        FEQ_THETA0_LAPPRX = FEQ_THETA0_MIDDLE
      END IF

    END DO

    THETA0 = 0.5 * ( THETA0_LAPPRX + THETA0_RAPPRX )
    ERROR = 0

  END SUBROUTINE MR_BISECT_SOLVE_FEQ_THETA0

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
  FUNCTION MR_FUNC_FEQ_THETA0( SINUOSITY , THETA0 ) RESULT( FEQ_THETA0 )

   !DIR$ ATTRIBUTES VECTOR :: MR_FUNC_FEQ_THETA0

    IMPLICIT NONE

    REAL   (GJRD_KIND) , INTENT(IN ) :: SINUOSITY
    REAL   (GJRD_KIND) , INTENT(IN ) :: THETA0

    REAL   (CARD_KIND) :: FEQ_THETA0

    FEQ_THETA0 = 1.0 / SINUOSITY -   &
    &   ( 1.0 - 2.2499997*(THETA0/3.0)**02 + 1.2656208*(THETA0/3.0)**04 - 0.3163866*(THETA0/3.0)**06   &
    &         + 0.0444479*(THETA0/3.0)**08 - 0.0039444*(THETA0/3.0)**10 + 0.0002100*(THETA0/3.0)**12 )
    ! POLYMINAL APPROXIMATION OF BESSEL FUNCTION OF THETA0 OF FIRST KIND AND ZEROTH ORDER

  END FUNCTION MR_FUNC_FEQ_THETA0

  END MODULE MR_MOD_INIT_MEANDER_PARS