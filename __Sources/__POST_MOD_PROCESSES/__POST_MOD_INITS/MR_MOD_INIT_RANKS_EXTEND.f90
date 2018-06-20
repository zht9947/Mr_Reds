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
  MODULE MR_MOD_INIT_RANKS_EXTEND

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_RANKS_

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_INIT_RANKS_EXTEND

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
! DEFREADION OF VARIABLES:
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
  SUBROUTINE MR_INIT_RANKS_EXTEND( NLOOPS )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: NLOOPS

    NI_  = NI * NLOOPS ; NJ_  = NJ

    NND_ = (2*NI_+1) * (2*NJ_+1)
    NEM_ =       NI_ * NJ_

  END SUBROUTINE MR_INIT_RANKS_EXTEND

  END MODULE MR_MOD_INIT_RANKS_EXTEND