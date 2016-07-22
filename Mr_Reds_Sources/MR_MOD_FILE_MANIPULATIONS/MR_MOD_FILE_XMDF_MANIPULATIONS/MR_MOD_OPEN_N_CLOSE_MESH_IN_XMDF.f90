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
  MODULE MR_MOD_OPEN_N_CLOSE_MESH_IN_XMDF

    USE XMDF

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_OPEN_MESH_IN_XMDF
    PUBLIC :: MR_CLOSE_MESH_IN_XMDF
    
    PUBLIC :: XF_PATH_MESH_IN_XMDF

    CHARACTER( 2**06 ) , PARAMETER :: XF_PATH_MESH_IN_XMDF = "2DMeshModule/mesh"

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
  SUBROUTINE MR_OPEN_MESH_IN_XMDF( FILE_XMDF_ID , MESH_IN_XMDF_ID , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: FILE_XMDF_ID
    INTEGER            , INTENT(OUT) :: MESH_IN_XMDF_ID
    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    ERRMSG = ""
    CALL XF_OPEN_GROUP( FILE_XMDF_ID , TRIM(XF_PATH_MESH_IN_XMDF) , MESH_IN_XMDF_ID , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in opening mesh"
      RETURN
    END IF

  END SUBROUTINE MR_OPEN_MESH_IN_XMDF

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
  SUBROUTINE MR_CLOSE_MESH_IN_XMDF( MESH_IN_XMDF_ID , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MESH_IN_XMDF_ID
    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    ERRMSG = ""
    CALL XF_CLOSE_GROUP( MESH_IN_XMDF_ID , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in closing mesh"
      RETURN
    END IF

  END SUBROUTINE MR_CLOSE_MESH_IN_XMDF

  END MODULE MR_MOD_OPEN_N_CLOSE_MESH_IN_XMDF