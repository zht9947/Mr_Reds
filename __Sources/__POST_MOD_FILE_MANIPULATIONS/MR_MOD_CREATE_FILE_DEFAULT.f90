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
  MODULE MR_MOD_CREATE_FILE_DEFAULT

    USE MR_ERRORS_FILE_MANIPULATE

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CREATE_FILE_DEFAULT

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
  SUBROUTINE MR_CREATE_FILE_DEFAULT( FILE_NAME , ERROR , ERRMSG , OVERWRITE )

    USE MR_MOD_OPEN_N_CLOSE_FILE_DEFAULT

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_NAME

    INTEGER                          :: FILE_ID

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    LOGICAL            , INTENT(IN ) , OPTIONAL :: OVERWRITE

    LOGICAL            :: BE_OPENED

    INTEGER            :: IQID

    ERRMSG = ""
    CALL SYSTEM_CLOCK( FILE_ID )
    DO IQID = 1 , HUGE(FILE_ID)
      INQUIRE( FILE_ID , OPENED=BE_OPENED )
      IF( BE_OPENED ) THEN
        FILE_ID = MOD( FILE_ID , HUGE(FILE_ID) ) + 1
      ELSE
        IF( PRESENT(OVERWRITE) .AND. OVERWRITE ) THEN
          OPEN( FILE_ID , FILE=TRIM(FILE_NAME) , STATUS='REPLACE' , IOSTAT=ERROR )
          IF( ERROR > 0 ) THEN
            ERROR = - ERROR
            ERRMSG = "Error in creating file"
            RETURN
          END IF
        ELSE
          OPEN( FILE_ID , FILE=TRIM(FILE_NAME) , STATUS='NEW' , IOSTAT=ERROR )
          IF( ERROR > 0 ) THEN
            ERROR = ERROR_CANNOT_CREATE_NEW_FILE
            ERRMSG = "Error in creating file"
            RETURN
          END IF
        END IF

        CALL MR_CLOSE_FILE_DEFAULT( FILE_ID , ERROR , ERRMSG )
        IF( ERROR < 0 ) THEN
          RETURN
        END IF

        RETURN

      END IF

    END DO

    ERROR = - 1
    ERRMSG = "No available unit assigned for file"

  END SUBROUTINE MR_CREATE_FILE_DEFAULT

  END MODULE MR_MOD_CREATE_FILE_DEFAULT