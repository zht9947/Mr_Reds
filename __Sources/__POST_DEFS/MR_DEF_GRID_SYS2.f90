!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE) MR_DEF_GRID_SYS_EXTEND
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
!   2015-03-27    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_DEF_GRID_SYS_EXTEND

    USE MR_KINDS

    IMPLICIT NONE

    INTEGER(EMID_KIND) , ALLOCATABLE , DIMENSION(:,:) :: EMIDW2

    INTEGER(NDID_KIND) , ALLOCATABLE , DIMENSION(:,:) :: NDIDW2
    INTEGER(NDID_KIND) , ALLOCATABLE , DIMENSION(:,:) :: NDIDU2 , NDIDV2
    INTEGER(NDID_KIND) , ALLOCATABLE , DIMENSION(:,:) :: NDIDO2

  END MODULE MR_DEF_GRID_SYS_EXTEND