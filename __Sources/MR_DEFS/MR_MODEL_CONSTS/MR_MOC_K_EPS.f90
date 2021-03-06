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
  MODULE MR_MOC_K_EPS

    USE MR_KINDS

    IMPLICIT NONE

    REAL   (PARD_KIND) , PARAMETER :: CV0 = 0.09

    REAL   (PARD_KIND) , PARAMETER :: CD1 = 1.44
    REAL   (PARD_KIND) , PARAMETER :: CD2 = 1.92

    REAL   (PARD_KIND) , PARAMETER :: SIK = 1.00
    REAL   (PARD_KIND) , PARAMETER :: SID = 1.30

  END MODULE MR_MOC_K_EPS