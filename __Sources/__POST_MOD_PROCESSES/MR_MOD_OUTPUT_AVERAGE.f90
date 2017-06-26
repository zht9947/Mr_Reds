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
!   2015-03-26    |     DR. HYDE     |    ORIGINAL CODE.
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
!   2015-03-26    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_OUTPUT_AVERAGE( FILE_AVERAGE_NAME , T , ERROR , ERRMSG )

    USE MR_MOD_CREATE_OPEN_N_CLOSE_FILE_DEFAULT

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

    ERRMSG = ""

    CALL MR_OPEN_FILE_DEFAULT( FILE_AVERAGE_NAME , "WRITE" , FILE_AVERAGE_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_AVERAGE_NAME)
      RETURN
    END IF

    WRITE( DUMMY_T_CHAR , '(F15.3,"s")') T
    DUMMY_T_CHAR = ADJUSTL( DUMMY_T_CHAR )
    DUMMY_T_CHAR( SCAN( DUMMY_T_CHAR , " " ) : LEN( DUMMY_T_CHAR ) ) = "----------------"
    WRITE( FILE_AVERAGE_ID , '("--------------------------------------------------------------------------------",   &
    & "-----------------T=", A ,/,"KSI",T15,"H (m)",T35,"Tauf (Pa)",T55,"Tau (Pa)",T75,"q (m^2/s)")' ) DUMMY_T_CHAR

    DUMMY_BASE = 0.0 ; DUMMY_REF = ZR
    AVERAGE_PLAN_H     = AVERAGE_PLAN_H     * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE
    AVERAGE_XSEC_H     = AVERAGE_XSEC_H     * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE

    DUMMY_BASE = 0.0 ; DUMMY_REF = TUVR
    AVERAGE_PLAN_TBFUV = AVERAGE_PLAN_TBFUV * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE
    AVERAGE_XSEC_TBFUV = AVERAGE_XSEC_TBFUV * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE

    DUMMY_BASE = 0.0 ; DUMMY_REF = TUVR
    AVERAGE_PLAN_TBUV  = AVERAGE_PLAN_TBUV  * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE
    AVERAGE_XSEC_TBUV  = AVERAGE_XSEC_TBUV  * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE

    DUMMY_BASE = 0.0 ; DUMMY_REF = UVR * ZR
    AVERAGE_PLAN_QUV   = AVERAGE_PLAN_QUV   * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE
    AVERAGE_XSEC_QUV   = AVERAGE_XSEC_QUV   * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE

    WRITE( FILE_AVERAGE_ID , '(" --------",T15,ES13.6,T35,ES13.6,T55,ES13.6,T75,ES13.6)' )   &
    & AVERAGE_PLAN_H , AVERAGE_PLAN_TBFUV , AVERAGE_PLAN_TBUV , AVERAGE_PLAN_QUV
    DKSI = REAL(0.5,CARD_KIND) / REAL(NI,CARD_KIND)
    KSI  = DKSI / REAL(2.0,CARD_KIND)
    DO I = 1 , NI
      WRITE( FILE_AVERAGE_ID , '(X,F8.6,T15,ES13.6,T35,ES13.6,T55,ES13.6,T75,ES13.6)' )   &
      & KSI , AVERAGE_XSEC_H( I ) , AVERAGE_XSEC_TBFUV( I ) , AVERAGE_XSEC_TBUV( I ) , AVERAGE_XSEC_QUV( I )
      KSI = KSI + DKSI
    END DO

    CALL MR_CLOSE_FILE_DEFAULT( FILE_AVERAGE_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_AVERAGE_NAME)
      RETURN
    END IF

  END SUBROUTINE MR_OUTPUT_AVERAGE

  END MODULE MR_MOD_OUTPUT_AVERAGE