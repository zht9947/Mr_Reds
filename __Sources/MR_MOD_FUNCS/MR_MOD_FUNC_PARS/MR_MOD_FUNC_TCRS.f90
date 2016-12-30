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
!   2015-03-26    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_MOD_FUNC_TCRS

    USE MR_KINDS

    USE MR_DEF_CONSTS_N_REF_PARS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_FUNC_TCRS

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
!   2015-03-26    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  FUNCTION MR_FUNC_TCRS( D0 , DS ) RESULT( TCRS )

    IMPLICIT NONE

    REAL(PARD_KIND) , INTENT(IN ) :: D0 , DS

    REAL(PARD_KIND) :: TCRS

    TCRS = RBT / FR * (SS-1.0) * (D0/ZR) *   &
    ( 0.130 *           EXP( -0.015 * DS * DS ) / (DS**0.392)   &
    + 0.045 * ( 1.000 - EXP( -0.068 *   DS    )               )   &
    )

  END FUNCTION MR_FUNC_TCRS

  END MODULE MR_MOD_FUNC_TCRS
