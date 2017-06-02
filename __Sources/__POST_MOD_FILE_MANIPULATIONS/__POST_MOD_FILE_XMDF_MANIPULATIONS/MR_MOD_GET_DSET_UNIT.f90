#INCLUDE 'MR_H_ALIGN_PADDING.H'
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
!
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-04-14    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_MOD_GET_DSET_UNIT

    USE XMDF

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_GET_DSET_UNIT

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
  SUBROUTINE MR_GET_DSET_UNIT( MULTI_DSETS_ID , PATH_DSET_IN_MULTI_DSETS , DSET_UNIT , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MULTI_DSETS_ID

    CHARACTER(   *   ) , INTENT(IN ) :: PATH_DSET_IN_MULTI_DSETS

    INTEGER                          :: DSET_ID

    CHARACTER(   *   ) , INTENT(OUT) :: DSET_UNIT

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY

    ERRMSG = ""
    CALL XF_OPEN_GROUP( MULTI_DSETS_ID , TRIM(PATH_DSET_IN_MULTI_DSETS) , DSET_ID , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in openning dataset group"
    ELSE

      CALL XF_GET_DATASET_UNITS( DSET_ID , DSET_UNIT , ERROR )
      IF( ERROR < 0 ) THEN
        ERRMSG = "Error in getting unit from dataset group"
      END IF

      CALL XF_CLOSE_GROUP( DSET_ID , ERROR_DUMMY )
      IF( ERROR_DUMMY < 0 .AND. ERROR >= 0 ) THEN
        ERROR = ERROR_DUMMY
        ERRMSG = "Error in closing dataset group"
      END IF

    END IF
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(PATH_DSET_IN_MULTI_DSETS)//" in multiple datasets"
      RETURN
    END IF

  END SUBROUTINE MR_GET_DSET_UNIT

  END MODULE MR_MOD_GET_DSET_UNIT