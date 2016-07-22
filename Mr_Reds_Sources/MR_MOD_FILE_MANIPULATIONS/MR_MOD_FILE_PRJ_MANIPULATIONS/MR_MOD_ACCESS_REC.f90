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
  MODULE MR_MOD_ACCESS_REC

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_ACCESS_REC

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
  SUBROUTINE MR_ACCESS_REC( FILE_PRJ_ID , REC , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: FILE_PRJ_ID

    CHARACTER(   *   ) , INTENT(OUT) :: REC

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    ERRMSG = ""

    REC = ""

    READ( FILE_PRJ_ID , '(A)' , IOSTAT=ERROR ) REC
    IF( ERROR == -1 ) THEN
      ERRMSG = "End of file when accessing record"
      RETURN
    ELSE IF( ERROR > 0 ) THEN
      ERROR = - ERROR
      ERRMSG = "Error in accessing record"
      RETURN
    END IF

  END SUBROUTINE MR_ACCESS_REC

  END MODULE MR_MOD_ACCESS_REC