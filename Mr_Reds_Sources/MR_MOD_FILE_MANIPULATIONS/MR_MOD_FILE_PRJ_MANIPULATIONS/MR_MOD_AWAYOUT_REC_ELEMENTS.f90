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
  MODULE MR_MOD_AWAYOUT_REC_ELEMENTS

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_AWAYOUT_LABEL
    PUBLIC :: MR_AWAYOUT_ALIAS
    
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
  SUBROUTINE MR_AWAYOUT_LABEL( REC , LABEL , ERROR , ERRMSG )

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(INOUT) :: REC

    CHARACTER(   *   ) , INTENT(OUT) :: LABEL

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    ERRMSG = ""

    LABEL = ""

    READ( REC , * , IOSTAT=ERROR ) LABEL
    IF( ERROR == -1 ) THEN
      ERRMSG = "No characters likely being label in record"
      RETURN
    ELSE IF( ERROR > 0 ) THEN
      ERROR = - ERROR
      ERRMSG = "Error in drawing out label from record"
      RETURN
    END IF
    
    REC( MAX(INDEX(REC,TRIM(LABEL))-1,1)   &
    &  : MIN(INDEX(REC,TRIM(LABEL))+LEN_TRIM(LABEL),LEN(REC)) ) = ""

  END SUBROUTINE MR_AWAYOUT_LABEL
  
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
  SUBROUTINE MR_AWAYOUT_ALIAS( REC , ALIAS , ERROR , ERRMSG )

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(INOUT) :: REC

    CHARACTER(   *   ) , INTENT(OUT) :: ALIAS

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    ERRMSG = ""
    
    ALIAS = ""

    READ( REC , * , IOSTAT=ERROR ) ALIAS
    IF( ERROR == -1 ) THEN
      ERRMSG = "No characters likely being alias in record"
      RETURN
    ELSE IF( ERROR > 0 ) THEN
      ERROR = - ERROR
      ERRMSG = "Error in drawing out alias from record"
      RETURN
    END IF

    REC( MAX(INDEX(REC,TRIM(ALIAS))-1,1)   &
    &  : MIN(INDEX(REC,TRIM(ALIAS))+LEN_TRIM(ALIAS),LEN(REC)) ) = ""

  END SUBROUTINE MR_AWAYOUT_ALIAS
  
  END MODULE MR_MOD_AWAYOUT_REC_ELEMENTS