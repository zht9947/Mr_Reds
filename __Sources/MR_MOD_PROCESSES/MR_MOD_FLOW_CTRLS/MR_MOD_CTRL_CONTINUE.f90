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
  MODULE MR_MOD_CTRL_CONTINUE

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CTRL_CONTINUE

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
  SUBROUTINE MR_CTRL_CONTINUE

    IMPLICIT NONE

    CHARACTER(   1   ) :: Y_OR_N

    DO
      WRITE(*,'(2X,"Continue? (Y/N): ", $ )')
      READ(*,*) Y_OR_N
      SELECT CASE( Y_OR_N )
      CASE( "Y" , "y" )
        EXIT
      CASE( "N" , "n" )
        STOP
      END SELECT
    END DO

  END SUBROUTINE MR_CTRL_CONTINUE

  END MODULE MR_MOD_CTRL_CONTINUE