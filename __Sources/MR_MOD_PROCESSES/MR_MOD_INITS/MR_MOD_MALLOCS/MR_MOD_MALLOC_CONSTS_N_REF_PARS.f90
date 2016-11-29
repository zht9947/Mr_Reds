#INCLUDE 'MR_H_ALIGN_PADDING.H'
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
  MODULE MR_MOD_MALLOC_CONSTS_N_REF_PARS

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_CONSTS_N_REF_PARS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_MALLOC_KK_CONSTS_N_REF_PARS

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
  SUBROUTINE MR_MALLOC_KK_CONSTS_N_REF_PARS

    IMPLICIT NONE

    ALLOCATE( SIGMA(1:NK) )

  END SUBROUTINE MR_MALLOC_KK_CONSTS_N_REF_PARS

  END MODULE MR_MOD_MALLOC_CONSTS_N_REF_PARS