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
  MODULE MR_MOD_GET_RANKS2

    USE XMDF

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_GET_NND_NEM

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
  SUBROUTINE MR_GET_NND_NEM( MESH_IN_XMDF_ID , NND , NEM , ERROR , ERRMSG )
  
    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MESH_IN_XMDF_ID

    INTEGER(NDID_KIND) , INTENT(OUT) :: NND
    INTEGER(EMID_KIND) , INTENT(OUT) :: NEM

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    ERRMSG = ""
    
    CALL XF_GET_NUMBER_OF_NODES( MESH_IN_XMDF_ID , NND , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in getting then number of nodes from mesh"
      RETURN
    END IF

    CALL XF_GET_NUMBER_OF_ELEMENTS( MESH_IN_XMDF_ID , NEM , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in getting then number of elements from mesh"
      RETURN
    END IF

  END SUBROUTINE MR_GET_NND_NEM

  END MODULE MR_MOD_GET_RANKS2