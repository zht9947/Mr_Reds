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
  MODULE MR_MOD_OUTPUT_AVERAGE

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_FIELD_VARS_AVERAGE

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_OUTPUT_AVERAGE

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
  SUBROUTINE MR_OUTPUT_AVERAGE( FILE_AVERAGE_NAME , T , ERROR , ERRMSG )

    USE MR_MOD_OPEN_N_CLOSE_FILE_DEFAULT

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_AVERAGE_NAME

    INTEGER                          :: FILE_AVERAGE_ID

    REAL   (TMRD_KIND) , INTENT(IN ) :: T

    CHARACTER( 2**04 )               :: DUMMY_T_CHAR

    REAL   (PARD_KIND)               :: DUMMY_BASE , DUMMY_REF

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    REAL   (CARD_KIND)               :: KSI , DKSI

    INTEGER(IJID_KIND)               :: I

    CALL MR_OPEN_FILE_DEFAULT( FILE_AVERAGE_NAME , FILE_AVERAGE_ID , ERROR , ERRMSG , READONLY=.FALSE. )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_AVERAGE_NAME)
      RETURN
    END IF

    WRITE( DUMMY_T_CHAR , '(F15.3,"s")') T
    DUMMY_T_CHAR = ADJUSTL( DUMMY_T_CHAR )
    DUMMY_T_CHAR( SCAN( DUMMY_T_CHAR , " " ) : LEN( DUMMY_T_CHAR ) ) = "----------------"
    WRITE( FILE_AVERAGE_ID , '( ("---------------") , 11("--------------------") ,   &
    & "T=", A ,/,"KSI",T15,"H (m)",T35,"f (Pa)",T75,"ma (Pa)",T115,"Tau (Pa)",T155,"Total Tau (Pa)",T175,   &
    & "q (m^2/s)",T215,"k (J/m^3)")' ) DUMMY_T_CHAR

    DUMMY_BASE = 0.0 ; DUMMY_REF = ZR
    AVERAGE_PLAN_H           = AVERAGE_PLAN_H           * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE
    AVERAGE_XSEC_H           = AVERAGE_XSEC_H           * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE

    DUMMY_BASE = 0.0 ; DUMMY_REF = TUVR
    AVERAGE_PLAN_UNIT_FORCES = AVERAGE_PLAN_UNIT_FORCES * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE
    AVERAGE_XSEC_UNIT_FORCES = AVERAGE_XSEC_UNIT_FORCES * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE

    DUMMY_BASE = 0.0 ; DUMMY_REF = TUVR
    AVERAGE_PLAN_UNIT_MOTION = AVERAGE_PLAN_UNIT_MOTION * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE
    AVERAGE_XSEC_UNIT_MOTION = AVERAGE_XSEC_UNIT_MOTION * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE

    DUMMY_BASE = 0.0 ; DUMMY_REF = R0 * UVR * UVR
    AVERAGE_PLAN_UNIT_ENERGY = AVERAGE_PLAN_UNIT_ENERGY * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE
    AVERAGE_XSEC_UNIT_ENERGY = AVERAGE_XSEC_UNIT_ENERGY * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE

    DUMMY_BASE = 0.0 ; DUMMY_REF = TUVR
    AVERAGE_PLAN_TBFUV       = AVERAGE_PLAN_TBFUV       * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE
    AVERAGE_XSEC_TBFUV       = AVERAGE_XSEC_TBFUV       * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE
    AVERAGE_PLAN_TBFUV_MOD   = AVERAGE_PLAN_TBFUV_MOD   * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE
    AVERAGE_XSEC_TBFUV_MOD   = AVERAGE_XSEC_TBFUV_MOD   * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE

    DUMMY_BASE = 0.0 ; DUMMY_REF = TUVR
    AVERAGE_PLAN_TBUV        = AVERAGE_PLAN_TBUV        * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE
    AVERAGE_XSEC_TBUV        = AVERAGE_XSEC_TBUV        * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE
    AVERAGE_PLAN_TBUV_MOD    = AVERAGE_PLAN_TBUV_MOD    * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE
    AVERAGE_XSEC_TBUV_MOD    = AVERAGE_XSEC_TBUV_MOD    * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE

    DUMMY_BASE = 0.0 ; DUMMY_REF = UVR * ZR
    AVERAGE_PLAN_QUV         = AVERAGE_PLAN_QUV         * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE
    AVERAGE_XSEC_QUV         = AVERAGE_XSEC_QUV         * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE

    WRITE( FILE_AVERAGE_ID , '(" --------",T15,11(ES13.6,7X))' )   &
    & AVERAGE_PLAN_H , AVERAGE_PLAN_UNIT_FORCES(:) , AVERAGE_PLAN_UNIT_MOTION(:) ,   &
    & AVERAGE_PLAN_TBUV(:) , AVERAGE_PLAN_TBUV_MOD, AVERAGE_PLAN_QUV(:) , AVERAGE_PLAN_UNIT_ENERGY
    DKSI = 0.5 / REAL(NI,CARD_KIND)
    KSI  = DKSI / REAL(2.0,CARD_KIND)
    DO I = 1 , NI
      WRITE( FILE_AVERAGE_ID , '(X,F8.6,T15,11(ES13.6,7X))' )   &
      & KSI ,   &
      & AVERAGE_XSEC_H( I ) , AVERAGE_XSEC_UNIT_FORCES( I ,:) , AVERAGE_XSEC_UNIT_MOTION( I ,:) ,   &
      & AVERAGE_XSEC_TBUV( I ,:) , AVERAGE_XSEC_TBUV_MOD( I ) , AVERAGE_XSEC_QUV( I ,:) , AVERAGE_XSEC_UNIT_ENERGY( I )
      KSI = KSI + DKSI
    END DO

    CALL MR_CLOSE_FILE_DEFAULT( FILE_AVERAGE_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_AVERAGE_NAME)
      RETURN
    END IF

  END SUBROUTINE MR_OUTPUT_AVERAGE

  END MODULE MR_MOD_OUTPUT_AVERAGE