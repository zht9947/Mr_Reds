!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE) MR_DEF_SLOPE
!
! PURPOSE:
!
!   TO DEFINE THE SLOPE PARAMETER MEASURED ALONG THE CHANNEL CENTERLINE.
!
! DEFINITION OF VARIABLES:
!
!                           SLOPE    :    SLOPE ALONG THE CHANNEL CENTERLINE
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_DEF_SLOPE

    USE MR_KINDS

    IMPLICIT NONE

    REAL   (SPRD_KIND) :: SLOPE = 0.000E-3

  END MODULE MR_DEF_SLOPE