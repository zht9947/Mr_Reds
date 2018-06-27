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
  MODULE MR_MOD_OPEN_N_CLOSE_FILE_DEFAULT

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_OPEN_FILE_DEFAULT
    PUBLIC :: MR_CLOSE_FILE_DEFAULT

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
  SUBROUTINE MR_OPEN_FILE_DEFAULT( FILE_NAME , FILE_ACTION , FILE_ID , ERROR , ERRMSG )

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_NAME
    CHARACTER(   *   ) , INTENT(IN ) :: FILE_ACTION

    INTEGER            , INTENT(OUT) :: FILE_ID

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    LOGICAL                          :: BE_OPENED

    INTEGER                          :: IQID

    ERRMSG = ""
    CALL SYSTEM_CLOCK( FILE_ID )
    DO IQID = 1 , HUGE(FILE_ID)
      INQUIRE( FILE_ID , OPENED=BE_OPENED )
      IF( BE_OPENED ) THEN
        FILE_ID = MOD( FILE_ID , HUGE(FILE_ID) ) + 1
      ELSE
        SELECT CASE( TRIM(FILE_ACTION) )
        CASE( "READ" , "R" , "Read" , "read", "r" )
          OPEN( FILE_ID , FILE=TRIM(FILE_NAME) , STATUS='OLD' , ACTION='READ'  , POSITION='REWIND' , IOSTAT=ERROR )
          IF( ERROR > 0 ) THEN
            ERROR = - ERROR
            ERRMSG = "Error in opening file"
            RETURN
          END IF
        CASE( "WRITE" , "W" , "Write" , "write", "w" )
          OPEN( FILE_ID , FILE=TRIM(FILE_NAME) , STATUS='OLD' , ACTION='WRITE' , POSITION='APPEND' , IOSTAT=ERROR )
          IF( ERROR > 0 ) THEN
            ERROR = - ERROR
            ERRMSG = "Error in opening file"
            RETURN
          END IF
        CASE DEFAULT
          OPEN( FILE_ID , FILE=TRIM(FILE_NAME) , STATUS='OLD' , ACTION='READWRITE' , IOSTAT=ERROR )
          IF( ERROR > 0 ) THEN
            ERROR = - ERROR
            ERRMSG = "Error in opening file"
            RETURN
          END IF
        END SELECT

        RETURN

      END IF

    END DO

    ERROR = - 1
    ERRMSG = "No available unit assigned for file"

  END SUBROUTINE MR_OPEN_FILE_DEFAULT

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
  SUBROUTINE MR_CLOSE_FILE_DEFAULT( FILE_ID , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: FILE_ID

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    ERRMSG = ""
    CLOSE( FILE_ID , IOSTAT=ERROR )
    IF( ERROR > 0 ) THEN
      ERROR = - ERROR
      ERRMSG = "Error in closing file"
    END IF

    RETURN

  END SUBROUTINE MR_CLOSE_FILE_DEFAULT

  END MODULE MR_MOD_OPEN_N_CLOSE_FILE_DEFAULT