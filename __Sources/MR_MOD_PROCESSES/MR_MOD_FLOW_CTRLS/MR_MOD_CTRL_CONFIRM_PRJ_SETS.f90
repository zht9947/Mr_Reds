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
  MODULE MR_MOD_CTRL_CONFIRM_PRJ_SETS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CTRL_CONFIRM_PRJ_SETS

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
  SUBROUTINE MR_CTRL_CONFIRM_PRJ_SETS

    USE MR_MOD_OPERATOR_CHAR_STRING

    IMPLICIT NONE

    CHARACTER(   1   ) :: Y_OR_N

    DO
      WRITE(*,'(2X,"Are all these settings above correct? (Y/N): ", $ )')
      READ(*,*) Y_OR_N
      SELECT CASE( .MRCHARUPPER.(Y_OR_N) )
      CASE( "Y" )
        RETURN
      CASE( "N" )
        STOP
      END SELECT
    END DO

  END SUBROUTINE MR_CTRL_CONFIRM_PRJ_SETS

  END MODULE MR_MOD_CTRL_CONFIRM_PRJ_SETS