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
  MODULE MR_MOD_OPEN_N_CLOSE_PROPERTIES

    USE XMDF

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_OPEN_PROPERTIES
    PUBLIC :: MR_CLOSE_PROPERTIES

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
  SUBROUTINE MR_OPEN_PROPERTIES( PARENT_ID , PROP_ID , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: PARENT_ID
    INTEGER            , INTENT(OUT) :: PROP_ID
    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    ERRMSG = ""
    CALL XF_OPEN_PROPERTY_GROUP( PARENT_ID , PROP_ID , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in opening /PROPERTIES"
      RETURN
    END IF

  END SUBROUTINE MR_OPEN_PROPERTIES

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
  SUBROUTINE MR_CLOSE_PROPERTIES( PROP_ID , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: PROP_ID
    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    ERRMSG = ""
    CALL XF_CLOSE_GROUP( PROP_ID , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in closing /PROPERTIES"
      RETURN
    END IF

  END SUBROUTINE MR_CLOSE_PROPERTIES

  END MODULE MR_MOD_OPEN_N_CLOSE_PROPERTIES