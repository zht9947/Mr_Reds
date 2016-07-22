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
  MODULE MR_MOD_OPEN_N_CLOSE_FILE_XMDF

    USE XMDF

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_OPEN_FILE_XMDF
    PUBLIC :: MR_CLOSE_FILE_XMDF

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
  SUBROUTINE MR_OPEN_FILE_XMDF( FILE_XMDF_NAME , ACTION , FILE_XMDF_ID , ERROR , ERRMSG )

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_XMDF_NAME
    CHARACTER(   *   ) , INTENT(IN ) :: ACTION

    INTEGER            , INTENT(OUT) :: FILE_XMDF_ID

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    ERRMSG = ""
    SELECT CASE( ACTION )
    CASE( "READ" , "R" , "Read" , "read", "r" )
      CALL XF_OPEN_FILE( TRIM(FILE_XMDF_NAME) , .TRUE. , FILE_XMDF_ID , ERROR )
      IF( ERROR < 0 ) THEN
        ERRMSG = "Error in opening file"
        RETURN
      END IF
    CASE DEFAULT
      CALL XF_OPEN_FILE( TRIM(FILE_XMDF_NAME) , .FALSE. , FILE_XMDF_ID , ERROR )
      IF( ERROR < 0 ) THEN
        ERRMSG = "Error in opening file"
        RETURN
      END IF
    END SELECT

  END SUBROUTINE MR_OPEN_FILE_XMDF

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
  SUBROUTINE MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: FILE_XMDF_ID

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    ERRMSG = ""
    CALL XF_CLOSE_FILE( FILE_XMDF_ID , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in closing file"
      RETURN
    END IF

  END SUBROUTINE MR_CLOSE_FILE_XMDF

  END MODULE MR_MOD_OPEN_N_CLOSE_FILE_XMDF