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

    INTEGER(EMID_KIND) , ALLOCATABLE , DIMENSION(:,:) :: EXTEND_EMIDW

    INTEGER(NDID_KIND) , ALLOCATABLE , DIMENSION(:,:) :: EXTEND_NDIDW
    INTEGER(NDID_KIND) , ALLOCATABLE , DIMENSION(:,:) :: EXTEND_NDIDU , EXTEND_NDIDV
    INTEGER(NDID_KIND) , ALLOCATABLE , DIMENSION(:,:) :: EXTEND_NDIDO

  END MODULE MR_DEF_GRID_SYS_EXTEND