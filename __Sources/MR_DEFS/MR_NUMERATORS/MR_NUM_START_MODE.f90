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
!   2015-03-24    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_NUM_START_MODE

    USE MR_KINDS

    IMPLICIT NONE

    INTEGER(NRID_KIND) :: START_MODE

  ! ENUMERATORS
    INTEGER(NRID_KIND) , PARAMETER :: COLD_MODE = 1_NRID_KIND
    INTEGER(NRID_KIND) , PARAMETER :: HOT_MODE  = 2_NRID_KIND
  ! END ENUMERATORS

  END MODULE MR_NUM_START_MODE