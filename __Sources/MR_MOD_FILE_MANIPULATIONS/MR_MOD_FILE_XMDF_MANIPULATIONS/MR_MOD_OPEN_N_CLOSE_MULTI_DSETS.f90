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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_MOD_OPEN_N_CLOSE_MULTI_DSETS

    USE XMDF

    USE MR_MOD_OPEN_N_CLOSE_MESH_IN_XMDF ,   &
    ONLY : XF_PATH_MESH_IN_XMDF

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_OPEN_MULTI_DSETS
    PUBLIC :: MR_CLOSE_MULTI_DSETS

    PUBLIC :: XF_PATH_MULTI_DSETS

    CHARACTER( 2**06 ) , PARAMETER :: XF_PATH_MULTI_DSETS =   &
    & TRIM(XF_PATH_MESH_IN_XMDF)//"/"//TRIM(MULTI_DATASET_LOCATION)

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_OPEN_MULTI_DSETS( FILE_XMDF_ID , MULTI_DSETS_ID , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: FILE_XMDF_ID
    INTEGER            , INTENT(OUT) :: MULTI_DSETS_ID
    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    ERRMSG = ""
    CALL XF_OPEN_GROUP( FILE_XMDF_ID , TRIM(XF_PATH_MULTI_DSETS) , MULTI_DSETS_ID , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in opening multiple datasets"
      RETURN
    END IF

  END SUBROUTINE MR_OPEN_MULTI_DSETS

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_CLOSE_MULTI_DSETS( MULTI_DSETS_ID , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MULTI_DSETS_ID
    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    ERRMSG = ""
    CALL XF_CLOSE_GROUP( MULTI_DSETS_ID , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in closing multiple datasets"
      RETURN
    END IF

  END SUBROUTINE MR_CLOSE_MULTI_DSETS

  END MODULE MR_MOD_OPEN_N_CLOSE_MULTI_DSETS