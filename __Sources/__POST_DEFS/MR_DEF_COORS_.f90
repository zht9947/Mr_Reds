!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE) MR_DEF_COORS_
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
  MODULE MR_DEF_COORS_

    USE MR_KINDS

    IMPLICIT NONE

    REAL   (XYRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:  ) :: XYUV_
    REAL   (XYRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:  ) :: XYUU_
    REAL   (XYRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:  ) :: XYVV_
    REAL   (XYRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:  ) :: XYOO_

  END MODULE MR_DEF_COORS_