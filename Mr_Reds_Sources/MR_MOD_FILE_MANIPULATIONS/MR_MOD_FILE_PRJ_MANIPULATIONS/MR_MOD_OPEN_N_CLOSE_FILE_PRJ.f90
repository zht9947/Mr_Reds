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
  MODULE MR_MOD_OPEN_N_CLOSE_FILE_PRJ

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_OPEN_FILE_PRJ
    PUBLIC :: MR_CLOSE_FILE_PRJ

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
  SUBROUTINE MR_OPEN_FILE_PRJ( FILE_PRJ_NAME , FILE_PRJ_ID , ERROR , ERRMSG )

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_PRJ_NAME

    INTEGER            , INTENT(OUT) :: FILE_PRJ_ID

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    LOGICAL                          :: BE_OPENED

    INTEGER                          :: IQID

    ERRMSG = ""
    CALL SYSTEM_CLOCK( FILE_PRJ_ID )
    DO IQID = 1 , HUGE(FILE_PRJ_ID)
      INQUIRE( FILE_PRJ_ID , OPENED=BE_OPENED )
      IF( BE_OPENED ) THEN
        FILE_PRJ_ID = MOD( FILE_PRJ_ID , HUGE(FILE_PRJ_ID) ) + 1
      ELSE
        OPEN( FILE_PRJ_ID , FILE=TRIM(FILE_PRJ_NAME) , STATUS='OLD' , ACTION='READ' , IOSTAT=ERROR )
        IF( ERROR > 0 ) THEN
          ERROR = - ERROR
          ERRMSG = "Error in opening file"
        END IF

        RETURN

      END IF
    END DO

    ERROR = -1 ; ERRMSG = "No available unit assigned for file"

  END SUBROUTINE MR_OPEN_FILE_PRJ

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
  SUBROUTINE MR_CLOSE_FILE_PRJ( FILE_PRJ_ID , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: FILE_PRJ_ID

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    ERRMSG = ""
    CLOSE( FILE_PRJ_ID , IOSTAT=ERROR )
    IF( ERROR > 0 ) THEN
      ERROR = - ERROR
      ERRMSG = "Error in closing file"
    END IF

    RETURN

  END SUBROUTINE MR_CLOSE_FILE_PRJ

  END MODULE MR_MOD_OPEN_N_CLOSE_FILE_PRJ