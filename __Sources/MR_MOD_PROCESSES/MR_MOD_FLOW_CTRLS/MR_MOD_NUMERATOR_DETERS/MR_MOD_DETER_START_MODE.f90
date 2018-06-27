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
  MODULE MR_MOD_DETER_START_MODE

    USE MR_NUM_START_MODE

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_DETER_START_MODE

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
  SUBROUTINE MR_DETER_START_MODE

    IMPLICIT NONE

    INTEGER :: ERROR

    WRITE(*,'("Which start mode would be preferred? ")')
    WRITE(*,'("    1. Cold mode; 2. Hot mode ")')
    DO
      WRITE(*,'("  Select No. ", $ )')
      READ(*,*,IOSTAT=ERROR) START_MODE
      IF( ERROR == 0 ) THEN
        SELECT CASE( START_MODE )
        CASE( COLD_MODE , HOT_MODE )
          RETURN
        END SELECT
      END IF
    END DO

  END SUBROUTINE MR_DETER_START_MODE

  END MODULE MR_MOD_DETER_START_MODE