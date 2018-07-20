!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE) MR_ERRORS_INIBATHYGEN
!
! PURPOSE:
!
!   TO DEFINE THE UNIQUE ERROR PARAMETERS IN THE PROJECT INIBATHYGEN.
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
  MODULE MR_ERRORS_INIBATHYGEN

    IMPLICIT NONE

    INTEGER , PARAMETER :: ERROR_BISECT_SOLVE_NO_UNIQUE_ROOT_IN_REGION = -2001

    INTEGER , PARAMETER :: ERROR_NEWTON_SOLVE_ZERO_DERIVATIVE          = -2011
    INTEGER , PARAMETER :: ERROR_NEWTON_SOLVE_MAX_NUMBER_OF_ITERATION  = -2012

  END MODULE MR_ERRORS_INIBATHYGEN