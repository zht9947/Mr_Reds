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
  MODULE MR_MOD_READ_SLOPE

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_READ_SLOPE
    
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
  SUBROUTINE MR_READ_SLOPE( V_STRING , VALUE , ERROR , ERRMSG )

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: V_STRING

    REAL   (FDRD_KIND) , INTENT(OUT) :: VALUE

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    ERRMSG = ""
    READ( V_STRING , * , IOSTAT=ERROR ) VALUE
    IF( ERROR == -1 ) THEN
      ERRMSG = "Null string when reading value of"
      RETURN
    ELSE IF( ERROR > 0 ) THEN
      ERROR = - ERROR
      ERRMSG = "Error in reading value from string """//TRIM(ADJUSTL(V_STRING))//""" of"
      RETURN
    END IF

  END SUBROUTINE MR_READ_SLOPE
  
  END MODULE MR_MOD_READ_SLOPE