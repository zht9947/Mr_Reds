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

    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_FIELD_VARS_AVERAGE
    USE MR_DEF_FIELD_VARS_DSET_NAMES
    USE MR_DEF_FIELD_VARS_UNITS

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

    REAL   (TMRD_KIND) , INTENT(IN ) :: T

    INTEGER                          :: FILE_AVERAGE_ID

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    REAL   (FDRD_KIND)               :: SS_AVERAGE
    REAL   (FDRD_KIND)               :: UV_AVERAGE(1:2)

    REAL   (PARD_KIND)               :: DUMMY_BASE , DUMMY_REF

    INTEGER                          :: DUMMY_CHAR_LEN

    ERRMSG = ""

    CALL MR_OPEN_FILE_DEFAULT( FILE_AVERAGE_NAME , "WRITE" , FILE_AVERAGE_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_AVERAGE_NAME)
      RETURN
    END IF

    WRITE( FILE_AVERAGE_ID , '( )' )
    WRITE( FILE_AVERAGE_ID , '("----------------------------------------------------------------------")' )
    WRITE( FILE_AVERAGE_ID , '("T = ",F15.5,"s")' ) T

  ! WRITE AVERAGE_H
    DUMMY_BASE = 0.0 ; DUMMY_REF = ZR
    SS_AVERAGE = AVERAGE_H
    SS_AVERAGE = SS_AVERAGE * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE
    DUMMY_CHAR_LEN = LEN_TRIM(DSET_NAME_H)
    WRITE( FILE_AVERAGE_ID , '(T53,TL<DUMMY_CHAR_LEN>,A," [",A,"] ",T70," : ",ES13.6)' )   &
    & TRIM(DSET_NAME_H)    , TRIM(UNIT_H)    ,   &
    & SS_AVERAGE

  ! WRITE AVERAGE_UV
    DUMMY_BASE = 0.0 ; DUMMY_REF = UVR
    UV_AVERAGE = AVERAGE_UV
    SS_AVERAGE = AVERAGE_UV_MOD
    UV_AVERAGE = UV_AVERAGE * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE
    SS_AVERAGE = SS_AVERAGE * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE
    DUMMY_CHAR_LEN = LEN_TRIM(DSET_NAME_UV)
    WRITE( FILE_AVERAGE_ID , '(T53,TL<DUMMY_CHAR_LEN>,A," [",A,"] ",T70," : ",ES13.6," , ","(",ES13.6,",",ES13.6,")" )' )   &
    & TRIM(DSET_NAME_UV)   , TRIM(UNIT_UV)   ,   &
    & SS_AVERAGE , UV_AVERAGE

  ! WRITE AVERAGE_TBUV
    DUMMY_BASE = 0.0 ; DUMMY_REF = TUVR
    UV_AVERAGE = AVERAGE_TBUV
    SS_AVERAGE = AVERAGE_TBUV_MOD
    UV_AVERAGE = UV_AVERAGE * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE
    SS_AVERAGE = SS_AVERAGE * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE
    DUMMY_CHAR_LEN = LEN_TRIM(DSET_NAME_TBUV)
    WRITE( FILE_AVERAGE_ID , '(T53,TL<DUMMY_CHAR_LEN>,A," [",A,"] ",T70," : ",ES13.6," , ","(",ES13.6,",",ES13.6,")" )' )   &
    & TRIM(DSET_NAME_TBUV) , TRIM(UNIT_TBUV) ,   &
    & SS_AVERAGE , UV_AVERAGE

  ! WRITE AVERAGE_QUV
    DUMMY_BASE = 0.0 ; DUMMY_REF = UVR * ZR
    UV_AVERAGE = AVERAGE_QUV
    UV_AVERAGE = UV_AVERAGE * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE
    DUMMY_CHAR_LEN = LEN_TRIM("Unit Discharge")
    WRITE( FILE_AVERAGE_ID , '(T53,TL<DUMMY_CHAR_LEN>,A," [",A,"] ",T70," : ",TR13  ,"   ","(",ES13.6,",",ES13.6,")" )' )   &
    & "Unit Discharge"     , "m^2/s"         ,   &
    & UV_AVERAGE

    WRITE( FILE_AVERAGE_ID , '( )' )

  ! WRITE NUMFR
    WRITE( FILE_AVERAGE_ID , '(4X,"Fr  = ",ES13.6," , ","(",ES13.6,",",ES13.6,")" )' )   &
    & AVERAGE_NUMFR_MOD , AVERAGE_NUMFR

  ! WRITE NUMFRS
    WRITE( FILE_AVERAGE_ID , '(4X,"Fr* = ",ES13.6," , ","(",ES13.6,",",ES13.6,")" )' )   &
    & AVERAGE_NUMFRS_MOD , AVERAGE_NUMFRS

    CALL MR_CLOSE_FILE_DEFAULT( FILE_AVERAGE_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_AVERAGE_NAME)
      RETURN
    END IF

  END SUBROUTINE MR_OUTPUT_AVERAGE

  END MODULE MR_MOD_OUTPUT_AVERAGE