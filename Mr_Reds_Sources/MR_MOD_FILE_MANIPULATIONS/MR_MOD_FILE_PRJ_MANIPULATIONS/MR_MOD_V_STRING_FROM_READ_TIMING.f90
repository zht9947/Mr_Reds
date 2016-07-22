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
  MODULE MR_MOD_READ_TIMING

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_READ_ITS
    PUBLIC :: MR_READ_T
    PUBLIC :: MR_READ_TIME_FACTOR
    
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
  SUBROUTINE MR_READ_ITS( V_STRING , VALUE , ERROR , ERRMSG )

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: V_STRING
    
    INTEGER(TSID_KIND) , INTENT(OUT) :: VALUE

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    ERRMSG = ""
    READ( V_STRING , * , IOSTAT=ERROR ) VALUE
    IF( ERROR == -1 ) THEN
      ERRMSG = "No digits when reading value of"
      RETURN
    ELSE IF( ERROR > 0 ) THEN
      ERROR = - ERROR
      ERRMSG = "Error in reading value of"
      RETURN
    END IF

  END SUBROUTINE MR_READ_ITS
  
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
  SUBROUTINE MR_READ_T( V_STRING , VALUE , ERROR , ERRMSG )

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: V_STRING

    REAL   (TMRD_KIND) , INTENT(OUT) :: VALUE

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    ERRMSG = ""
    READ( V_STRING , * , IOSTAT=ERROR ) VALUE
    IF( ERROR == -1 ) THEN
      ERRMSG = "No digits when reading value of"
      RETURN
    ELSE IF( ERROR > 0 ) THEN
      ERROR = - ERROR
      ERRMSG = "Error in reading value of"
      RETURN
    END IF

  END SUBROUTINE MR_READ_T
  
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
  SUBROUTINE MR_READ_TIME_FACTOR( V_STRING , VALUE , ERROR , ERRMSG )

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: V_STRING

    REAL   (PARD_KIND) , INTENT(OUT) :: VALUE

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    ERRMSG = ""
    READ( V_STRING , * , IOSTAT=ERROR ) VALUE
    IF( ERROR == -1 ) THEN
      ERRMSG = "No digits when reading value of"
      RETURN
    ELSE IF( ERROR > 0 ) THEN
      ERROR = - ERROR
      ERRMSG = "Error in reading value of"
      RETURN
    END IF

  END SUBROUTINE MR_READ_TIME_FACTOR

  END MODULE MR_MOD_READ_TIMING