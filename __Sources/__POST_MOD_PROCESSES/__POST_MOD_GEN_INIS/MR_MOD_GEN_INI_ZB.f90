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

    USE MR_ERRORS_INIBATHYGEN

    USE MR_KINDS

    USE MR_MAC_PI

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_GEN_INI_ZB

    REAL   (CARD_KIND) , ALLOCATABLE , DIMENSION(:) :: ALPHA_LBOUND , ALPHA_RBOUND

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
  SUBROUTINE MR_GEN_INI_ZB( THETA0 , BTH , LTH , HTH , DZB_BK_MIN , DZB_BK_MAX , XI0 , XXIM , NBENDS , NI , NJ , ZB , ERROR )

    IMPLICIT NONE

    REAL   (PARD_KIND) , INTENT(IN ) :: THETA0
    REAL   (PARD_KIND) , INTENT(IN ) :: BTH
    REAL   (PARD_KIND) , INTENT(IN ) :: LTH , HTH
    REAL   (PARD_KIND) , INTENT(IN ) :: DZB_BK_MIN , DZB_BK_MAX
    REAL   (PARD_KIND) , INTENT(IN ) :: XI0 , XXIM

    INTEGER            , INTENT(IN ) :: NBENDS

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) :: ZB

    INTEGER            , INTENT(OUT) , DIMENSION(1:NI1(NI,KIND(ERROR))) :: ERROR

    REAL   (PARD_KIND) :: COEFFI_XXIM
    REAL   (PARD_KIND) :: COEFFI_DZB_BK_MIN_N_MAX_1 , COEFFI_DZB_BK_MIN_N_MAX_2

    REAL   (PARD_KIND) :: CURVTH_MAX

    REAL   (PARD_KIND) :: DXI , DYJ

    REAL   (PARD_KIND) :: XI , YJ
    REAL   (PARD_KIND) :: CURVTH , RADITH
    REAL   (PARD_KIND) :: DZB_BK_L , DZB_BK_R

    REAL   (CARD_KIND) , DIMENSION(1:NI1(NI,CARD_KIND)) :: ALPHA , BETA , GAMA

    INTEGER(IJID_KIND) :: I , J

    COEFFI_XXIM = ( 0.25 - XXIM ) / ( 1.0 + SIN( 2.0 * PI * XXIM ) )

    COEFFI_DZB_BK_MIN_N_MAX_1 = 0.25 * ( DZB_BK_MAX + DZB_BK_MIN ) / BTH
    COEFFI_DZB_BK_MIN_N_MAX_2 = 0.50 * ( DZB_BK_MAX - DZB_BK_MIN ) / BTH

    CURVTH_MAX = 2.0 * PI * THETA0 * BTH / LTH

    DXI = 0.5_PARD_KIND * NBENDS / NI
    DYJ = 1.0_PARD_KIND / NJ

    IF( (.NOT. ALLOCATED(ALPHA_LBOUND) ) .AND. (.NOT. ALLOCATED(ALPHA_RBOUND) ) ) THEN
      ALLOCATE( ALPHA_LBOUND(1:NI1(NI,CARD_KIND)) , ALPHA_RBOUND(1:NI1(NI,CARD_KIND)) )
      ALPHA_LBOUND = -8.001 ; ALPHA_RBOUND = +10.001
    END IF

   !DIR$ NOVECTOR
    DO I = 1 , NI

      XI = (I-0.5) * DXI

      CURVTH = CURVTH_MAX * SIN( 2.0 * PI * XI )
      RADITH = 1.0 / SIGN( MAX( ABS(CURVTH) , 10.0*EPSILON(RADITH) ) , CURVTH )

      CALL MR_CALC_DZB_BK( XI0 , COEFFI_XXIM ,   &
      & COEFFI_DZB_BK_MIN_N_MAX_1 , COEFFI_DZB_BK_MIN_N_MAX_2 , XI , DZB_BK_L , DZB_BK_R , ERROR( I ) )
      IF( .NOT. ERROR( I ) < 0 ) THEN

        CALL MR_CALC_DZB_PROFILE_COEFFIS( RADITH , DZB_BK_L , DZB_BK_R ,   &
        & ALPHA_LBOUND( I ) , ALPHA_RBOUND( I ) , ALPHA( I ) , BETA( I ) , GAMA( I ) , ERROR( I ) )

      END IF

    END DO

    IF( ANY( ERROR < 0 ) ) THEN
      RETURN
    END IF

    DO J = 1 , NJ

      YJ = (J-0.5) * DYJ

     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        ZB( I , J ) = ( GAMA( I ) - BETA( I ) * EXP( -ALPHA( I ) * ( 1.0 - YJ ) ) ) * BTH - HTH
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
  SUBROUTINE MR_CALC_DZB_BK( XI0 , COEFFI_XXIM ,   &
  & COEFFI_DZB_BK_MIN_N_MAX_1 , COEFFI_DZB_BK_MIN_N_MAX_2 , XI , DZB_BK_L , DZB_BK_R , ERROR )

    IMPLICIT NONE

    REAL   (PARD_KIND) , INTENT(IN ) :: XI0
    REAL   (PARD_KIND) , INTENT(IN ) :: COEFFI_XXIM
    REAL   (PARD_KIND) , INTENT(IN ) :: COEFFI_DZB_BK_MIN_N_MAX_1 , COEFFI_DZB_BK_MIN_N_MAX_2

    REAL   (PARD_KIND) , INTENT(IN ) :: XI

    REAL   (PARD_KIND) , INTENT(OUT) :: DZB_BK_L , DZB_BK_R

    INTEGER            , INTENT(OUT) :: ERROR

    REAL   (PARD_KIND) :: DUMMY_DZB_BK_1 , DUMMY_DZB_BK_2

    REAL   (PARD_KIND) :: XXI , XXXI

    XXI = XI - XI0

    CALL MR_NEWTON_SOLVE_FUNC_XXXI( COEFFI_XXIM , XXI , XXXI , XXI , ERROR )

    DUMMY_DZB_BK_1 = COEFFI_DZB_BK_MIN_N_MAX_1 - COEFFI_DZB_BK_MIN_N_MAX_1 * COS( 4.0 * PI * XXXI )
    DUMMY_DZB_BK_2 = COEFFI_DZB_BK_MIN_N_MAX_2 * SIN( 2.0 * PI * XXXI )

    DZB_BK_L = DUMMY_DZB_BK_1 - DUMMY_DZB_BK_2
    DZB_BK_R = DUMMY_DZB_BK_1 + DUMMY_DZB_BK_2

  END SUBROUTINE MR_CALC_DZB_BK

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
  SUBROUTINE MR_NEWTON_SOLVE_FUNC_XXXI( COEFFI_XXIM , XXI , XXXI , XXXI_NEAR , ERROR )

    IMPLICIT NONE

    REAL   (PARD_KIND) , INTENT(IN ) :: COEFFI_XXIM

    REAL   (PARD_KIND) , INTENT(IN ) :: XXI

    REAL   (PARD_KIND) , INTENT(OUT) :: XXXI

    REAL   (PARD_KIND) , INTENT(IN ) :: XXXI_NEAR

    REAL   (PARD_KIND) , PARAMETER   :: EPS_XXXI = 10.0*EPSILON(EPS_XXXI)
    REAL   (CARD_KIND) , PARAMETER   :: EPS_FUNC_XXXI = 10.0*EPSILON(EPS_FUNC_XXXI)

    INTEGER            , PARAMETER   :: N_ITERS = 10000

    INTEGER            , INTENT(OUT) :: ERROR

    REAL   (PARD_KIND) :: XXXI0 , XXXI1
    REAL   (CARD_KIND) :: FUNC_XXXI0 , FUNC_XXXI1
    REAL   (CARD_KIND) :: FFUNC_XXXI0

    INTEGER            :: I_ITER

    XXXI0 = XXXI_NEAR
    FUNC_XXXI0 = MR_FUNC_XXXI( COEFFI_XXIM , XXI , XXXI0 )

    DO I_ITER = 1 , N_ITERS

      FFUNC_XXXI0 = MR_FFUNC_XXXI( COEFFI_XXIM , XXI , XXXI0 )
      IF( FFUNC_XXXI0 <= EPS_FUNC_XXXI ) THEN
        XXXI = HUGE(XXXI)
        ERROR = ERROR_NEWTON_SOLVE_ZERO_DERIVATIVE
        RETURN
      END IF

      XXXI1 = XXXI0 - FUNC_XXXI0 / FFUNC_XXXI0

      FUNC_XXXI1 = MR_FUNC_XXXI( COEFFI_XXIM , XXI , XXXI1 )
      IF( ABS( XXXI1 - XXXI0 ) <= EPS_XXXI .OR. ABS( FUNC_XXXI1 ) <= EPS_FUNC_XXXI ) THEN
        XXXI = XXXI1
        ERROR = 0
        RETURN
      ELSE
        XXXI0 = XXXI1
        FUNC_XXXI0 = FUNC_XXXI1
      END IF

    END DO

    XXXI = HUGE(XXXI)
    ERROR = ERROR_NEWTON_SOLVE_MAX_NUMBER_OF_ITERATION

  END SUBROUTINE MR_NEWTON_SOLVE_FUNC_XXXI
  
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
  FUNCTION MR_FUNC_XXXI( COEFFI_XXIM , XXI , XXXI ) RESULT( FUNC_XXXI )

   !DIR$ ATTRIBUTES VECTOR : UNIFORM( COEFFI_XXIM ) :: MR_FUNC_XXXI

    IMPLICIT NONE

    REAL   (CARD_KIND)               :: FUNC_XXXI

    REAL   (PARD_KIND) , INTENT(IN ) :: COEFFI_XXIM

    REAL   (PARD_KIND) , INTENT(IN ) :: XXI

    REAL   (PARD_KIND) , INTENT(IN ) :: XXXI

    FUNC_XXXI = (XXXI-XXI) - COEFFI_XXIM * ( 1.0 - COS( 2.0 * PI * (XXXI+XXI) ) )

  END FUNCTION MR_FUNC_XXXI

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
  FUNCTION MR_FFUNC_XXXI( COEFFI_XXIM , XXI , XXXI ) RESULT( FFUNC_XXXI )

   !DIR$ ATTRIBUTES VECTOR : UNIFORM( COEFFI_XXIM ) :: MR_FFUNC_XXXI

    IMPLICIT NONE

    REAL   (CARD_KIND)               :: FFUNC_XXXI

    REAL   (PARD_KIND) , INTENT(IN ) :: COEFFI_XXIM

    REAL   (PARD_KIND) , INTENT(IN ) :: XXI

    REAL   (PARD_KIND) , INTENT(IN ) :: XXXI

    FFUNC_XXXI = 1.0 - 2.0 * PI * COEFFI_XXIM * SIN( 2.0 * PI * (XXXI+XXI) )

  END FUNCTION MR_FFUNC_XXXI

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
  SUBROUTINE MR_CALC_DZB_PROFILE_COEFFIS( RADITH , DZB_BK_L , DZB_BK_R ,   &
  & ALPHA_LBOUND , ALPHA_RBOUND , ALPHA , BETA , GAMA , ERROR )

    IMPLICIT NONE

    REAL   (PARD_KIND) , INTENT(IN ) :: RADITH
    REAL   (PARD_KIND) , INTENT(IN ) :: DZB_BK_L , DZB_BK_R

    REAL   (CARD_KIND) , INTENT(IN ) :: ALPHA_LBOUND , ALPHA_RBOUND

    REAL   (CARD_KIND) , INTENT(OUT) :: ALPHA , BETA , GAMA

    INTEGER            , INTENT(OUT) :: ERROR

    CALL MR_BISECT_SOLVE_FUNC_ALPHA( RADITH , DZB_BK_L , DZB_BK_R , ALPHA , ALPHA_LBOUND , ALPHA_RBOUND , ERROR )

    CALL MR_CALC_BETA_N_GAMA( DZB_BK_L , DZB_BK_R , ALPHA , BETA , GAMA )

  END SUBROUTINE MR_CALC_DZB_PROFILE_COEFFIS

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
  SUBROUTINE MR_BISECT_SOLVE_FUNC_ALPHA( RADITH , DZB_BK_L , DZB_BK_R , ALPHA , ALPHA_LBOUND , ALPHA_RBOUND , ERROR )

    IMPLICIT NONE

    REAL   (PARD_KIND) , INTENT(IN ) :: RADITH
    REAL   (PARD_KIND) , INTENT(IN ) :: DZB_BK_L , DZB_BK_R

    REAL   (CARD_KIND) , INTENT(OUT) :: ALPHA

    REAL   (CARD_KIND) , INTENT(IN ) :: ALPHA_LBOUND , ALPHA_RBOUND

    REAL   (CARD_KIND) , PARAMETER   :: EPS_ALPHA = 10.0*EPSILON(EPS_ALPHA)
    REAL   (CARD_KIND) , PARAMETER   :: EPS_FUNC_ALPHA = 10.0*EPSILON(EPS_FUNC_ALPHA)

    INTEGER            , INTENT(OUT) :: ERROR

    REAL   (CARD_KIND) :: ALPHA_LAPPRX , ALPHA_RAPPRX , ALPHA_MIDDLE
    REAL   (CARD_KIND) :: FUNC_ALPHA_LAPPRX , FUNC_ALPHA_RAPPRX , FUNC_ALPHA_MIDDLE

    ALPHA_LAPPRX = ALPHA_LBOUND
    FUNC_ALPHA_LAPPRX = MR_FUNC_ALPHA( RADITH , DZB_BK_L , DZB_BK_R , ALPHA_LAPPRX )
    ALPHA_RAPPRX = ALPHA_RBOUND
    FUNC_ALPHA_RAPPRX = MR_FUNC_ALPHA( RADITH , DZB_BK_L , DZB_BK_R , ALPHA_RAPPRX )

    IF( FUNC_ALPHA_LAPPRX * FUNC_ALPHA_RAPPRX > 0 ) THEN
      ALPHA = HUGE(ALPHA)
      ERROR = ERROR_BISECT_SOLVE_NO_UNIQUE_ROOT_IN_REGION
      RETURN
    END IF

    DO WHILE( 0.5 * ABS( ALPHA_RAPPRX - ALPHA_LAPPRX ) > EPS_ALPHA )

      ALPHA_MIDDLE = 0.5 * ( ALPHA_LAPPRX + ALPHA_RAPPRX )
      FUNC_ALPHA_MIDDLE = MR_FUNC_ALPHA( RADITH , DZB_BK_L , DZB_BK_R , ALPHA_MIDDLE )

      IF( ABS( FUNC_ALPHA_MIDDLE ) <= EPS_FUNC_ALPHA ) THEN
        EXIT
      ELSE IF( FUNC_ALPHA_LAPPRX * FUNC_ALPHA_MIDDLE < 0 ) THEN
        ALPHA_RAPPRX = ALPHA_MIDDLE
        FUNC_ALPHA_RAPPRX = FUNC_ALPHA_MIDDLE
      ELSE
        ALPHA_LAPPRX = ALPHA_MIDDLE
        FUNC_ALPHA_LAPPRX = FUNC_ALPHA_MIDDLE
      END IF

    END DO

    ALPHA = 0.5 * ( ALPHA_LAPPRX + ALPHA_RAPPRX )
    ERROR = 0

  END SUBROUTINE MR_BISECT_SOLVE_FUNC_ALPHA

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
  FUNCTION MR_FUNC_ALPHA( RADITH , DZB_BK_L , DZB_BK_R , ALPHA ) RESULT( FUNC_ALPHA )

   !DIR$ ATTRIBUTES VECTOR :: MR_FUNC_ALPHA

    IMPLICIT NONE

    REAL   (CARD_KIND)               :: FUNC_ALPHA

    REAL   (PARD_KIND) , INTENT(IN ) :: RADITH
    REAL   (PARD_KIND) , INTENT(IN ) :: DZB_BK_L , DZB_BK_R

    REAL   (CARD_KIND) , INTENT(IN ) :: ALPHA

    REAL   (CARD_KIND) :: BETA , GAMA

    CALL MR_CALC_BETA_N_GAMA( DZB_BK_L , DZB_BK_R , ALPHA , BETA , GAMA )

    FUNC_ALPHA =    &
    & BETA * ( EXP(-ALPHA) * ( ALPHA*(RADITH-0.5) - 1.0 ) - ALPHA*(RADITH+0.5) + 1.0 ) +   &
    & GAMA * ALPHA * ALPHA * RADITH

  END FUNCTION MR_FUNC_ALPHA

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
  SUBROUTINE MR_CALC_BETA_N_GAMA( DZB_BK_L , DZB_BK_R , ALPHA , BETA , GAMA )

   !DIR$ ATTRIBUTES VECTOR :: MR_CALC_BETA_N_GAMA

    IMPLICIT NONE

    REAL   (PARD_KIND) , INTENT(IN ) :: DZB_BK_L , DZB_BK_R

    REAL   (CARD_KIND) , INTENT(IN ) :: ALPHA

    REAL   (CARD_KIND) , INTENT(OUT) :: BETA , GAMA

    BETA = ( DZB_BK_L - DZB_BK_R ) / ( EXP(-ALPHA) - 1.0 )
    GAMA =   DZB_BK_L + BETA

  END SUBROUTINE MR_CALC_BETA_N_GAMA

  END MODULE MR_MOD_GEN_INI_ZB