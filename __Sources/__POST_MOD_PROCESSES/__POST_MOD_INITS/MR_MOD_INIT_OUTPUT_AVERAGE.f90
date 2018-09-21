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
  MODULE MR_MOD_INIT_OUTPUT_AVERAGE

    USE MR_KINDS

    USE MR_DEF_CONSTS_N_REF_PARS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_INIT_OUTPUT_AVERAGE

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
  SUBROUTINE MR_INIT_OUTPUT_AVERAGE( FILE_AVERAGE_NAME , ERROR , ERRMSG )

    USE MR_MOD_CREATE_FILE_DEFAULT
    USE MR_MOD_OPEN_N_CLOSE_FILE_DEFAULT

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_AVERAGE_NAME

    INTEGER                          :: FILE_AVERAGE_ID , MULTI_DSETS_ID

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    CALL MR_OPEN_FILE_DEFAULT( FILE_AVERAGE_NAME , FILE_AVERAGE_ID , ERROR , ERRMSG , READONLY=.FALSE. )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_AVERAGE_NAME)
      RETURN
    END IF

    WRITE( FILE_AVERAGE_ID , '("Rho (kg/m^3)",/,ES13.6)' ) R0
    WRITE( FILE_AVERAGE_ID , '("g (m/s^2)",/,ES13.6)' ) GR

    CALL MR_CLOSE_FILE_DEFAULT( FILE_AVERAGE_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_AVERAGE_NAME)
      RETURN
    END IF

  END SUBROUTINE MR_INIT_OUTPUT_AVERAGE

  END MODULE MR_MOD_INIT_OUTPUT_AVERAGE