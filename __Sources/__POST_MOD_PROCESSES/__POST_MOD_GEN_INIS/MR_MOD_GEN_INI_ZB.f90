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

    USE MR_ERRORS_NONLINEAR_EQN_SOLVE

    USE MR_KINDS

    USE MR_MAC_PI

    USE MR_DEF_RANKS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_CURVED_GEOS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_GEN_INI_ZB

    REAL   (GJRD_KIND) , ALLOCATABLE , DIMENSION(:) :: DAREA
    REAL   (GJRD_KIND) , ALLOCATABLE , DIMENSION(:) :: WTH , RTH
    REAL   (GJRD_KIND) , ALLOCATABLE , DIMENSION(:) :: RTH_BK_L , RTH_BK_R

    REAL   (PARD_KIND) , ALLOCATABLE , DIMENSION(:) :: DZB_BK_L , DZB_BK_R

    REAL   (CARD_KIND) , ALLOCATABLE , DIMENSION(:) :: ALFA_LBOUND , ALFA_RBOUND

    REAL   (CARD_KIND) , ALLOCATABLE , DIMENSION(:) :: ALFA , BETA , GAMA

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
  SUBROUTINE MR_INIT_WTH_N_RTH

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( DAREA(1:NI1(NI,GJRD_KIND)) )

      DAREA = 0
      DO J = 1 , NJ
        !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          DAREA( I ) = DAREA( I ) + MW( I , J )
        END DO
      END DO

      !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        WTH( I ) = 2.0 * DAREA( I )   &
        / ( SQRT( GVV( I , 0 ,1,1) ) + SQRT( GVV( I ,NJ ,1,1) ) )
      END DO

    DEALLOCATE( DAREA )

   !DIR$ VECTOR ALIGNED
    DO I = 1 , NI

      RTH( I ) =   &
      ( ( SIGN( ACOS( JOO(I-1, 0 ,1,1) / SQRT(GOO(I-1, 0 ,1,1)) ) , JOO(I-1, 0 ,2,1) )   &
        + SIGN( ACOS( JOO(I-1,NJ ,1,1) / SQRT(GOO(I-1,NJ ,1,1)) ) , JOO(I-1,NJ ,2,1) )   &
        )   &
      - ( SIGN( ACOS( JOO( I , 0 ,1,1) / SQRT(GOO( I , 0 ,1,1)) ) , JOO( I , 0 ,2,1) )   &
        + SIGN( ACOS( JOO( I ,NJ ,1,1) / SQRT(GOO( I ,NJ ,1,1)) ) , JOO( I ,NJ ,2,1) )   &
        )   &
      ) / ( SQRT( GVV( I , 0 ,1,1) ) + SQRT( GVV( I ,NJ ,1,1) ) )

      RTH( I ) = 1.0 / SIGN( MAX( ABS(RTH( I )) , 10.0*EPSILON(RTH( I )) ) , RTH( I ) )

      RTH_BK_L( I ) = RTH( I ) + 0.5 * WTH( I )
      RTH_BK_R( I ) = RTH( I ) - 0.5 * WTH( I )

    END DO

  END SUBROUTINE MR_INIT_WTH_N_RTH

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
  SUBROUTINE MR_GEN_INI_ZB( HTH , DZB_BK_MIN , DZB_BK_MAX , XI0 , XXIM , NBENDS , NI , NJ , ZB , ERROR , ERRMSG )

    IMPLICIT NONE

    REAL   (PARD_KIND) , INTENT(IN ) :: HTH
    REAL   (PARD_KIND) , INTENT(IN ) :: DZB_BK_MIN , DZB_BK_MAX
    REAL   (PARD_KIND) , INTENT(IN ) :: XI0 , XXIM

    INTEGER            , INTENT(IN ) :: NBENDS

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) :: ZB

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    REAL   (PARD_KIND) :: COEFFI_XXIM
    REAL   (PARD_KIND) :: COEFFI_DZB_BK_MIN_N_MAX_1 , COEFFI_DZB_BK_MIN_N_MAX_2

    REAL   (PARD_KIND) :: DYJ

    INTEGER(IJID_KIND) :: I , J

    COEFFI_XXIM = ( 0.25 - XXIM ) / ( 1.0 + SIN( 2.0 * PI * XXIM ) )

    COEFFI_DZB_BK_MIN_N_MAX_1 = 0.25 * ( DZB_BK_MAX + DZB_BK_MIN )
    COEFFI_DZB_BK_MIN_N_MAX_2 = 0.50 * ( DZB_BK_MAX - DZB_BK_MIN )

    IF( (.NOT. ALLOCATED(DZB_BK_L) ) .AND. (.NOT. ALLOCATED(DZB_BK_R) ) ) THEN
      ALLOCATE( DZB_BK_L(1:NI1(NI,PARD_KIND)) , DZB_BK_R(1:NI1(NI,PARD_KIND)) )
      CALL MR_GEN_INI_DZB_BK( XI0 , COEFFI_XXIM , COEFFI_DZB_BK_MIN_N_MAX_1 , COEFFI_DZB_BK_MIN_N_MAX_2 ,   &
      & NBENDS , NI , DZB_BK_L , DZB_BK_R , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        DEALLOCATE( DZB_BK_L , DZB_BK_R )
        RETURN
      END IF
    END IF

      IF( (.NOT. ALLOCATED(WTH) ) .AND. (.NOT. ALLOCATED(RTH) )   &
      &  .AND. (.NOT. ALLOCATED(RTH_BK_L) ) .AND. (.NOT. ALLOCATED(RTH_BK_R) ) ) THEN
        ALLOCATE( WTH(1:NI1(NI,GJRD_KIND)) , RTH(1:NI1(NI,GJRD_KIND)) ,   &
        & RTH_BK_L(1:NI1(NI,GJRD_KIND)) , RTH_BK_R(1:NI1(NI,GJRD_KIND)) )
        CALL MR_INIT_WTH_N_RTH
      END IF

        ALLOCATE( ALFA(1:NI1(NI,CARD_KIND)) , BETA(1:NI1(NI,CARD_KIND)) , GAMA(1:NI1(NI,CARD_KIND)) )

          IF( (.NOT. ALLOCATED(ALFA_LBOUND) ) .AND. (.NOT. ALLOCATED(ALFA_RBOUND) ) ) THEN
            ALLOCATE( ALFA_LBOUND(1:NI1(NI,CARD_KIND)) , ALFA_RBOUND(1:NI1(NI,CARD_KIND)) )
            ALFA_LBOUND = -8.001 ; ALFA_RBOUND = +10.001
          END IF

            CALL MR_GEN_INI_DZB_PROFILE_COEFFIS( NI , DZB_BK_L , DZB_BK_R , RTH_BK_L , RTH_BK_R , RTH , WTH ,   &
            & ALFA_LBOUND , ALFA_RBOUND , ALFA , BETA , GAMA , ERROR , ERRMSG )
            IF( ERROR < 0 ) THEN
              DEALLOCATE( ALFA , BETA , GAMA )
              RETURN
            END IF

            DYJ = 1.0_PARD_KIND / NJ

            DO J = 1 , NJ
             !DIR$ VECTOR ALIGNED
              DO I = 1 , NI
                ZB( I , J ) = ( GAMA( I ) - BETA( I ) * EXP( -ALFA( I ) * ( 1.0 - (J-0.5)*DYJ ) ) ) - HTH
              END DO
            END DO

          DEALLOCATE( ALFA_LBOUND , ALFA_RBOUND )

        DEALLOCATE( ALFA , BETA , GAMA )

      DEALLOCATE( WTH , RTH , RTH_BK_L , RTH_BK_R )

    DEALLOCATE( DZB_BK_L , DZB_BK_R )

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
  SUBROUTINE MR_GEN_INI_DZB_BK( XI0 , COEFFI_XXIM , COEFFI_DZB_BK_MIN_N_MAX_1 , COEFFI_DZB_BK_MIN_N_MAX_2 ,   &
  & NBENDS , NI , DZB_BK_L , DZB_BK_R , ERROR , ERRMSG )

    IMPLICIT NONE

    REAL   (PARD_KIND) , INTENT(IN ) :: XI0
    REAL   (PARD_KIND) , INTENT(IN ) :: COEFFI_XXIM
    REAL   (PARD_KIND) , INTENT(IN ) :: COEFFI_DZB_BK_MIN_N_MAX_1 , COEFFI_DZB_BK_MIN_N_MAX_2

    INTEGER            , INTENT(IN ) :: NBENDS

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI

    REAL   (PARD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,PARD_KIND)) :: DZB_BK_L , DZB_BK_R

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    REAL   (PARD_KIND) , DIMENSION(1:NI1(NI,PARD_KIND)) :: XXI , XXXI

    REAL   (PARD_KIND) :: DUMMY_DZB_BK_1 , DUMMY_DZB_BK_2

    REAL   (PARD_KIND) :: DXI

    CHARACTER( 2**03 ) :: I_CHAR
    INTEGER(IJID_KIND) :: I

    DXI = 0.5_PARD_KIND * NBENDS / NI

   !DIR$ VECTOR ALIGNED
    DO I = 1 , NI
      XXI( I ) = (I-0.5)*DXI - XI0
    END DO

   !DIR$ NOVECTOR
    DO I = 1 , NI
      CALL MR_NEWTON_SOLVE_FEQ_XXXI( COEFFI_XXIM , XXI( I ) , XXXI( I ) , XXI( I ) , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE( I_CHAR , '(I<LEN(I_CHAR)>)' ) I
        ERRMSG = TRIM(ERRMSG)//" for cross section no. "//TRIM(ADJUSTL(I_CHAR))
        RETURN
      END IF
    END DO

   !DIR$ VECTOR ALIGNED
    DO I = 1 , NI

      DUMMY_DZB_BK_1 = COEFFI_DZB_BK_MIN_N_MAX_1 - COEFFI_DZB_BK_MIN_N_MAX_1 * COS( 4.0 * PI * XXXI( I ) )
      DUMMY_DZB_BK_2 = COEFFI_DZB_BK_MIN_N_MAX_2 * SIN( 2.0 * PI * XXXI( I ) )

      DZB_BK_L( I ) = DUMMY_DZB_BK_1 - DUMMY_DZB_BK_2
      DZB_BK_R( I ) = DUMMY_DZB_BK_1 + DUMMY_DZB_BK_2

    END DO

  END SUBROUTINE MR_GEN_INI_DZB_BK

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
  SUBROUTINE MR_NEWTON_SOLVE_FEQ_XXXI( COEFFI_XXIM , XXI , XXXI , XXXI_NEAR , ERROR , ERRMSG )

    IMPLICIT NONE

    REAL   (PARD_KIND) , INTENT(IN ) :: COEFFI_XXIM

    REAL   (PARD_KIND) , INTENT(IN ) :: XXI

    REAL   (PARD_KIND) , INTENT(OUT) :: XXXI

    REAL   (PARD_KIND) , INTENT(IN ) :: XXXI_NEAR

    REAL   (PARD_KIND) , PARAMETER   :: EPS_XXXI = 10.0*EPSILON(EPS_XXXI)
    REAL   (CARD_KIND) , PARAMETER   :: EPS_FEQ_XXXI = 10.0*EPSILON(EPS_FEQ_XXXI)
    REAL   (CARD_KIND) , PARAMETER   :: EPS_FFEQ_XXXI = 10.0*EPSILON(EPS_FFEQ_XXXI)

    INTEGER            , PARAMETER   :: N_ITERS = 10000

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    REAL   (PARD_KIND) :: XXXI0 , XXXI1
    REAL   (CARD_KIND) :: FEQ_XXXI0 , FEQ_XXXI1
    REAL   (CARD_KIND) :: FFEQ_XXXI0

    INTEGER            :: I_ITER

    ERRMSG = ""

    XXXI0 = XXXI_NEAR
    FEQ_XXXI0 = MR_FUNC_FEQ_XXXI( COEFFI_XXIM , XXI , XXXI0 )

    DO I_ITER = 1 , N_ITERS

      FFEQ_XXXI0 = MR_FUNC_FFEQ_XXXI( COEFFI_XXIM , XXI , XXXI0 )
      IF( FFEQ_XXXI0 <= EPS_FFEQ_XXXI ) THEN
        XXXI = HUGE(XXXI)
        ERROR = ERROR_NEWTON_SOLVE_ZERO_DERIVATIVE
        ERRMSG = "Zero deivative when using newton method to solve xxxi"
        RETURN
      END IF

      XXXI1 = XXXI0 - FEQ_XXXI0 / FFEQ_XXXI0

      FEQ_XXXI1 = MR_FUNC_FEQ_XXXI( COEFFI_XXIM , XXI , XXXI1 )
      IF( ABS( XXXI1 - XXXI0 ) <= EPS_XXXI .OR. ABS( FEQ_XXXI1 ) <= EPS_FEQ_XXXI ) THEN
        XXXI = XXXI1
        ERROR = 0
        RETURN
      ELSE
        XXXI0 = XXXI1
        FEQ_XXXI0 = FEQ_XXXI1
      END IF

    END DO

    XXXI = HUGE(XXXI)
    ERROR = ERROR_NEWTON_SOLVE_MAX_NUMBER_OF_ITERATION
    ERRMSG = "Max number of iterations when using newton method to solve xxxi"

  END SUBROUTINE MR_NEWTON_SOLVE_FEQ_XXXI
  
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
  FUNCTION MR_FUNC_FEQ_XXXI( COEFFI_XXIM , XXI , XXXI ) RESULT( FEQ_XXXI )

   !DIR$ ATTRIBUTES VECTOR : UNIFORM( COEFFI_XXIM ) :: MR_FUNC_FEQ_XXXI

    IMPLICIT NONE

    REAL   (PARD_KIND) , INTENT(IN ) :: COEFFI_XXIM

    REAL   (PARD_KIND) , INTENT(IN ) :: XXI
    REAL   (PARD_KIND) , INTENT(IN ) :: XXXI

    REAL   (CARD_KIND) :: FEQ_XXXI

    FEQ_XXXI = (XXXI-XXI) - COEFFI_XXIM * ( 1.0 - COS( 2.0 * PI * (XXXI+XXI) ) )

  END FUNCTION MR_FUNC_FEQ_XXXI

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
  FUNCTION MR_FUNC_FFEQ_XXXI( COEFFI_XXIM , XXI , XXXI ) RESULT( FFEQ_XXXI )

   !DIR$ ATTRIBUTES VECTOR : UNIFORM( COEFFI_XXIM ) :: MR_FUNC_FFEQ_XXXI

    IMPLICIT NONE

    REAL   (PARD_KIND) , INTENT(IN ) :: COEFFI_XXIM

    REAL   (PARD_KIND) , INTENT(IN ) :: XXI
    REAL   (PARD_KIND) , INTENT(IN ) :: XXXI

    REAL   (CARD_KIND) :: FFEQ_XXXI

    FFEQ_XXXI = 1.0 - 2.0 * PI * COEFFI_XXIM * SIN( 2.0 * PI * (XXXI+XXI) )

  END FUNCTION MR_FUNC_FFEQ_XXXI

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
  SUBROUTINE MR_GEN_INI_DZB_PROFILE_COEFFIS( NI , DZB_BK_L , DZB_BK_R , RTH_BK_L , RTH_BK_R , RTH , WTH ,   &
  & ALFA_LBOUND , ALFA_RBOUND , ALFA , BETA , GAMA , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI

    REAL   (PARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,PARD_KIND)) :: DZB_BK_L , DZB_BK_R
    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,GJRD_KIND)) :: RTH_BK_L , RTH_BK_R , RTH , WTH

    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND)) :: ALFA_LBOUND , ALFA_RBOUND

    REAL   (CARD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,CARD_KIND)) :: ALFA , BETA , GAMA

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    CHARACTER( 2**03 ) :: I_CHAR
    INTEGER(IJID_KIND) :: I

   !DIR$ NOVECTOR
    DO I = 1 , NI
      CALL MR_BISECT_SOLVE_FEQ_ALFA( DZB_BK_L( I ) , DZB_BK_R( I ) , RTH_BK_L( I ) , RTH_BK_R( I ) , RTH( I ) , WTH( I ) ,   &
      & ALFA( I ) , ALFA_LBOUND( I ) , ALFA_RBOUND( I ) , ERROR , ERRMSG )
      IF( ERROR < 0 ) THEN
        WRITE( I_CHAR , '(I<LEN(I_CHAR)>)' ) I
        ERRMSG = TRIM(ERRMSG)//" for cross section no. "//TRIM(ADJUSTL(I_CHAR))
        RETURN
      END IF
    END DO

   !DIR$ VECTOR ALIGNED
    DO I = 1 , NI
      CALL MR_CALC_BETA_N_GAMA( DZB_BK_L( I ) , DZB_BK_R( I ) , ALFA( I ) , BETA( I ) , GAMA( I ) )
    END DO

  END SUBROUTINE MR_GEN_INI_DZB_PROFILE_COEFFIS

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
  SUBROUTINE MR_BISECT_SOLVE_FEQ_ALFA( DZB_BK_L , DZB_BK_R , RTH_BK_L , RTH_BK_R , RTH , WTH ,   &
  & ALFA , ALFA_LBOUND , ALFA_RBOUND , ERROR , ERRMSG )

    IMPLICIT NONE

    REAL   (PARD_KIND) , INTENT(IN ) :: DZB_BK_L , DZB_BK_R
    REAL   (GJRD_KIND) , INTENT(IN ) :: RTH_BK_L , RTH_BK_R , RTH , WTH

    REAL   (CARD_KIND) , INTENT(OUT) :: ALFA

    REAL   (CARD_KIND) , INTENT(IN ) :: ALFA_LBOUND , ALFA_RBOUND

    REAL   (CARD_KIND) , PARAMETER   :: EPS_ALFA = 10.0*EPSILON(EPS_ALFA)
    REAL   (CARD_KIND) , PARAMETER   :: EPS_FEQ_ALFA = 10.0*EPSILON(EPS_FEQ_ALFA)

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    REAL   (CARD_KIND) :: ALFA_LAPPRX , ALFA_RAPPRX , ALFA_MIDDLE
    REAL   (CARD_KIND) :: FEQ_ALFA_LAPPRX , FEQ_ALFA_RAPPRX , FEQ_ALFA_MIDDLE

    ERRMSG = ""

    ALFA_LAPPRX = ALFA_LBOUND
    FEQ_ALFA_LAPPRX = MR_FUNC_FEQ_ALFA( DZB_BK_L , DZB_BK_R , RTH_BK_L , RTH_BK_R , RTH , WTH , ALFA_LAPPRX )
    IF( ABS( FEQ_ALFA_LAPPRX ) <= EPS_FEQ_ALFA ) THEN
      ALFA = ALFA_LAPPRX
      ERROR = 0
      RETURN
    END IF
    ALFA_RAPPRX = ALFA_RBOUND
    FEQ_ALFA_RAPPRX = MR_FUNC_FEQ_ALFA( DZB_BK_L , DZB_BK_R , RTH_BK_L , RTH_BK_R , RTH , WTH , ALFA_RAPPRX )
    IF( ABS( FEQ_ALFA_RAPPRX ) <= EPS_FEQ_ALFA ) THEN
      ALFA = ALFA_RAPPRX
      ERROR = 0
      RETURN
    END IF

    IF( FEQ_ALFA_LAPPRX * FEQ_ALFA_RAPPRX > 0 ) THEN
      ALFA = HUGE(ALFA)
      ERROR = ERROR_BISECT_SOLVE_NO_UNIQUE_ROOT_IN_REGION
      ERRMSG = "No unique root in given region when using bisection method to solve alpha"
      RETURN
    END IF

    DO WHILE( 0.5 * ABS( ALFA_RAPPRX - ALFA_LAPPRX ) > EPS_ALFA )

      ALFA_MIDDLE = 0.5 * ( ALFA_LAPPRX + ALFA_RAPPRX )
      FEQ_ALFA_MIDDLE = MR_FUNC_FEQ_ALFA( DZB_BK_L , DZB_BK_R , RTH_BK_L , RTH_BK_R , RTH , WTH , ALFA_MIDDLE )

      IF( ABS( FEQ_ALFA_MIDDLE ) <= EPS_FEQ_ALFA ) THEN
        EXIT
      ELSE IF( FEQ_ALFA_LAPPRX * FEQ_ALFA_MIDDLE < 0 ) THEN
        ALFA_RAPPRX = ALFA_MIDDLE
        FEQ_ALFA_RAPPRX = FEQ_ALFA_MIDDLE
      ELSE
        ALFA_LAPPRX = ALFA_MIDDLE
        FEQ_ALFA_LAPPRX = FEQ_ALFA_MIDDLE
      END IF

    END DO

    ALFA = 0.5 * ( ALFA_LAPPRX + ALFA_RAPPRX )
    ERROR = 0

  END SUBROUTINE MR_BISECT_SOLVE_FEQ_ALFA

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
  FUNCTION MR_FUNC_FEQ_ALFA( DZB_BK_L , DZB_BK_R , RTH_BK_L , RTH_BK_R , RTH , WTH , ALFA ) RESULT( FEQ_ALFA )

   !DIR$ ATTRIBUTES VECTOR :: MR_FUNC_FEQ_ALFA

    IMPLICIT NONE

    REAL   (PARD_KIND) , INTENT(IN ) :: DZB_BK_L , DZB_BK_R
    REAL   (GJRD_KIND) , INTENT(IN ) :: RTH_BK_L , RTH_BK_R , RTH , WTH

    REAL   (CARD_KIND) , INTENT(IN ) :: ALFA

    REAL   (CARD_KIND) :: BETA , GAMA

    REAL   (CARD_KIND) :: FEQ_ALFA

    CALL MR_CALC_BETA_N_GAMA( DZB_BK_L , DZB_BK_R , ALFA , BETA , GAMA )

    FEQ_ALFA =   &
    & BETA * ( EXP(-ALFA) * ( ALFA * ( RTH_BK_R / WTH ) - 1.0 ) - ALFA * ( RTH_BK_L / WTH ) + 1.0 ) +   &
    & GAMA * ALFA * ALFA * ( RTH / WTH )

  END FUNCTION MR_FUNC_FEQ_ALFA

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
  SUBROUTINE MR_CALC_BETA_N_GAMA( DZB_BK_L , DZB_BK_R , ALFA , BETA , GAMA )

   !DIR$ ATTRIBUTES VECTOR :: MR_CALC_BETA_N_GAMA

    IMPLICIT NONE

    REAL   (PARD_KIND) , INTENT(IN ) :: DZB_BK_L , DZB_BK_R

    REAL   (CARD_KIND) , INTENT(IN ) :: ALFA

    REAL   (CARD_KIND) , INTENT(OUT) :: BETA , GAMA

    BETA = ( DZB_BK_L - DZB_BK_R ) / ( EXP(-ALFA) - 1.0 )
    GAMA =   DZB_BK_L + BETA

  END SUBROUTINE MR_CALC_BETA_N_GAMA

  END MODULE MR_MOD_GEN_INI_ZB