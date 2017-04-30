!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE)
!
! PURPOSE:
!
!
!
! DEFINITION OF VARIABLES:
!
!                         T_UNITS    :    TIME UNITS
!                     COMPRESSION    :    COMPRESSION OPTION FOR A DATASET REPRESENTED BY AN INTEGER
!                                      \\ A VALUE OF -1 FOR THE COMPRESSION OPTION IS NO COMPRESSION.
!                                         A COMPRESSION OPTION BETWEEN 0 AND 9 INDICATES THE COMPRESSION LEVEL
!                                           WHERE 0 IS THE MINIMUM COMPRESSION;
!                                             AND 9 IS THE MAXIMUM COMPRESSION.
!                                         HIGHER COMPRESSION LEVELS RESULT IN SMALLER FILES BUT ARE SLOWER.
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_MOD_CREATE_DSET

    USE XMDF

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CREATE_DSET_UV
    PUBLIC :: MR_CREATE_DSET_SS

    CHARACTER( 2**05 ) , PARAMETER :: T_UNITS = TS_SECONDS
    INTEGER            , PARAMETER :: COMPRESSION = -1   ! NO COMPRESSION

!***********************************************************************************************************************************

  CONTAINS

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE)
!
! PURPOSE:
!
!
!
! DEFINITION OF VARIABLES:
!
!
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_CREATE_DSET_UV( MULTI_DSETS_ID , PATH_UV_IN_MULTI_DSETS , UNIT_UV , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MULTI_DSETS_ID

    CHARACTER(   *   ) , INTENT(IN ) :: PATH_UV_IN_MULTI_DSETS
    CHARACTER(   *   ) , INTENT(IN ) :: UNIT_UV

    INTEGER                          :: DSET_UV_ID

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY

    ERRMSG = ""
    CALL XF_CREATE_VECTOR_DATASET( MULTI_DSETS_ID , TRIM(PATH_UV_IN_MULTI_DSETS) , TRIM(UNIT_UV) ,   &
    & T_UNITS , COMPRESSION , DSET_UV_ID , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in creating vector dataset group"
    ELSE

      CALL XF_CLOSE_GROUP( DSET_UV_ID , ERROR_DUMMY )
      IF( ERROR_DUMMY < 0 .AND. ERROR >= 0 ) THEN
        ERROR = ERROR_DUMMY
        ERRMSG = "Error in closing vector dataset group"
      END IF

    END IF
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(PATH_UV_IN_MULTI_DSETS)//" in multiple datasets"
      RETURN
    END IF

  END SUBROUTINE MR_CREATE_DSET_UV

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE)
!
! PURPOSE:
!
!
!
! DEFINITION OF VARIABLES:
!
!
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_CREATE_DSET_SS( MULTI_DSETS_ID , PATH_SS_IN_MULTI_DSETS , UNIT_SS , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MULTI_DSETS_ID

    CHARACTER(   *   ) , INTENT(IN ) :: PATH_SS_IN_MULTI_DSETS
    CHARACTER(   *   ) , INTENT(IN ) :: UNIT_SS

    INTEGER                          :: DSET_SS_ID

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY

    ERRMSG = ""
    CALL XF_CREATE_SCALAR_DATASET( MULTI_DSETS_ID , TRIM(PATH_SS_IN_MULTI_DSETS) , TRIM(UNIT_SS) ,   &
    & T_UNITS , COMPRESSION , DSET_SS_ID , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in creating scalar dataset group"
    ELSE

      CALL XF_CLOSE_GROUP( DSET_SS_ID , ERROR_DUMMY )
      IF( ERROR_DUMMY < 0 .AND. ERROR >= 0 ) THEN
        ERROR = ERROR_DUMMY
        ERRMSG = "Error in closing scalar dataset group"
      END IF

    END IF
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(PATH_SS_IN_MULTI_DSETS)//" in multiple datasets"
      RETURN
    END IF

  END SUBROUTINE MR_CREATE_DSET_SS

  END MODULE MR_MOD_CREATE_DSET