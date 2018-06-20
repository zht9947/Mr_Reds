!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE) MR_DEF_GRID_SYS_
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
  MODULE MR_DEF_GRID_SYS_

    USE MR_KINDS

    IMPLICIT NONE

    INTEGER(EMID_KIND) , ALLOCATABLE , DIMENSION(:,:) :: EMIDW_

    INTEGER(NDID_KIND) , ALLOCATABLE , DIMENSION(:,:) :: NDIDW_
    INTEGER(NDID_KIND) , ALLOCATABLE , DIMENSION(:,:) :: NDIDU_ , NDIDV_
    INTEGER(NDID_KIND) , ALLOCATABLE , DIMENSION(:,:) :: NDIDO_

  END MODULE MR_DEF_GRID_SYS_