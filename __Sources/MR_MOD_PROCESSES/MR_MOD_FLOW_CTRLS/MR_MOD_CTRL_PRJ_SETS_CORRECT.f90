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
  MODULE MR_MOD_CTRL_PRJ_SETS_CORRECT

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CTRL_PRJ_SETS_CORRECT

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
  SUBROUTINE MR_CTRL_PRJ_SETS_CORRECT

    IMPLICIT NONE

    CHARACTER(   1   ) :: Y_OR_N

    DO
      WRITE(*,'(2X,"Are all these settings above correct? (Y/N): ", $ )')
      READ(*,*) Y_OR_N
      SELECT CASE( Y_OR_N )
      CASE( "Y" , "y" )
        RETURN
      CASE( "N" , "n" )
        STOP
      END SELECT
    END DO

  END SUBROUTINE MR_CTRL_PRJ_SETS_CORRECT

  END MODULE MR_MOD_CTRL_PRJ_SETS_CORRECT