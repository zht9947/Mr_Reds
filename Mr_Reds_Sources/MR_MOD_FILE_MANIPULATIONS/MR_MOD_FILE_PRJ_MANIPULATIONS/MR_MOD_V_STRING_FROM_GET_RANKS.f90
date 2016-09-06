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
  MODULE MR_MOD_GET_RANKS

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_GET_NI_NJ_NK

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
  SUBROUTINE MR_GET_NI_NJ_NK( V_STRING , NI , NJ , NK , ERROR , ERRMSG )

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: V_STRING

    INTEGER(IJID_KIND) , INTENT(OUT) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(OUT) :: NK

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    ERRMSG = ""
    READ( V_STRING , * , IOSTAT=ERROR ) NI , NJ , NK
    IF( ERROR == -1 ) THEN
      ERRMSG = "No or no enough digits when getting numbers of "   &
      //"columns (NI), rows (NJ) and layers (NK)"
      RETURN
    ELSE IF( ERROR > 0 ) THEN
      ERROR = - ERROR
      ERRMSG = "Error in getting numbers of "   &
      //"columns (NI), rows (NJ) and layers (NK)"
      RETURN
    END IF

  END SUBROUTINE MR_GET_NI_NJ_NK

  END MODULE MR_MOD_GET_RANKS