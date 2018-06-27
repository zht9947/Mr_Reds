!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE) MR_DEF_ACTIITY
!
! PURPOSE:
!
!   TO DEFINE THE ACTIVITY, WHICH HELPS DETERMINE WHICH CELL ON THE GRID IS OUGHT TO BE INVOLVED IN THE COMPUTATION. THE ACTIVITY
!   IS AN ENUMERATOR VARIABLE, WHICH HAS ONLY 2 POSSIBLE VALUES: 1 (TO BE INVOLVED IN THE COMPUTATION) AND 0 (NOT TO BE INVOLVED
!   IN THE COMPUTATION).
!
! DEFINITION OF VARIABLES:
!
!                        ACTIVITY    :    FLAG TO DETERMINE WHETHER A CELL IS TO BE INVOLVED IN THE COMPUTATION
!
!   // BELOW ARE ENUMERATORS FOR 'ACTIVITY'
!
!                        BEACTIVE    :    1, TO BE INVOLVED IN THE COMPUTATION
!                        NOACTIVE    :    0, NOT TO BE INVOLVED IN THE COMPUTATION
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_DEF_ACTIVITY

    USE MR_KINDS

    IMPLICIT NONE

    INTEGER(ACID_KIND) , ALLOCATABLE , DIMENSION(:,:) :: ACTIVITY

  ! ENUMERATORS FOR 'ACTIVITY'
    INTEGER(ACID_KIND) , PARAMETER :: NOACTIVE = 0_ACID_KIND
    INTEGER(ACID_KIND) , PARAMETER :: BEACTIVE = 1_ACID_KIND
  ! END ENUMERATORS FOR 'ACTIVITY'

  END MODULE MR_DEF_ACTIVITY