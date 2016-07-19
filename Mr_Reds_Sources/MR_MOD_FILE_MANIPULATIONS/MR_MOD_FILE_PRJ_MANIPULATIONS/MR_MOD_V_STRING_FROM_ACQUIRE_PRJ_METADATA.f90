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
  MODULE MR_MOD_ACQUIRE_PRJ_METADATA

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_ACQUIRE_PRJ_METADATA
    
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
  SUBROUTINE MR_ACQUIRE_PRJ_METADATA( V_STRING , PRJ_METADATA , ERROR , ERRMSG )

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: V_STRING
    
    CHARACTER(   *   ) , INTENT(OUT) :: PRJ_METADATA
    
    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    READ( V_STRING , * , IOSTAT=ERROR ) PRJ_METADATA
    IF( ERROR == -1 ) THEN
      ERRMSG = "End of file when acquiring Project Metadata from file"
      RETURN
    ELSE IF( ERROR > 0 ) THEN
      ERROR = - ERROR
      ERRMSG = "Error in acquiring Project Metadata from file"
      RETURN
    END IF

  END SUBROUTINE MR_ACQUIRE_PRJ_METADATA
  
  END MODULE MR_MOD_ACQUIRE_PRJ_METADATA