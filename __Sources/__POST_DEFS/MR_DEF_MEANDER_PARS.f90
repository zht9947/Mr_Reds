!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE) MR_DEF_MEANDER_PARS
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
  MODULE MR_DEF_MEANDER_PARS

    USE MR_KINDS

    IMPLICIT NONE

    REAL   (GJRD_KIND) :: THETA0

    REAL   (GJRD_KIND) :: BTH , LTH , LAMBTH
    REAL   (GJRD_KIND) :: LAMBTH2BTH , SINUOSITY

    INTEGER            :: NBENDS

  END MODULE MR_DEF_MEANDER_PARS