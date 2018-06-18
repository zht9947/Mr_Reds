!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE) MR_DEF_COORS
!
! PURPOSE:
!
!   TO DEFINE THE COORDINATES.
!
! DEFINITION OF VARIABLES:
!
!                            XYUV    :    COORDINATES AT CENTROIDS
!                      XYUU, XYVV    :    COORDINATES AT U- AND V-, RESPECTIVELY, FACIAL NODES
!                            XYOO    :    COORDINATES AT CORNER NODES
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-03-27    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_DEF_COORS

    USE MR_KINDS

    IMPLICIT NONE

    REAL   (XYRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:  ) :: XYUV
    REAL   (XYRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:  ) :: XYUU
    REAL   (XYRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:  ) :: XYVV
    REAL   (XYRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:  ) :: XYOO

  END MODULE MR_DEF_COORS