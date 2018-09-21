!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE)
!
! PURPOSE:
!
!   TO
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
  MODULE MR_MOD_CTRL_RETRY_CREATING_FILES

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CTRL_RETRY_CREATING_FILES

!***********************************************************************************************************************************

  CONTAINS

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE)
!
! PURPOSE:
!
!   TO
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
  SUBROUTINE MR_CTRL_RETRY_CREATING_FILES

    USE MR_MOD_OPERATOR_CHAR_STRING

    IMPLICIT NONE

    CHARACTER( 1 ) :: Y_OR_N

    DO
      WRITE(*,'("Retry creating files by deleting the old ones? (Y/N): ", $ )')
      READ(*,*) Y_OR_N
      SELECT CASE( .MRCHARUPPER.(Y_OR_N) )
      CASE( "Y" )
        RETURN
      CASE( "N" )
        STOP
      END SELECT
    END DO

  END SUBROUTINE MR_CTRL_RETRY_CREATING_FILES

  END MODULE MR_MOD_CTRL_RETRY_CREATING_FILES