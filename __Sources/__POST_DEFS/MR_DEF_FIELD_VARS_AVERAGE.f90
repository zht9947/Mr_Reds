!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE) MR_DEF_FIELD_VARS_AVERAGE
!
! PURPOSE:
!
!   TO
!
! DEFINITION OF VARIABLES:
!
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-03-26    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_DEF_FIELD_VARS_AVERAGE

    USE MR_KINDS

    IMPLICIT NONE

    REAL   (FDRD_KIND) :: AVERAGE_H
    REAL   (FDRD_KIND) :: AVERAGE_UV(1:2) , AVERAGE_UV_MOD
    REAL   (FDRD_KIND) :: AVERAGE_TBUV(1:2) , AVERAGE_TBUV_MOD
    REAL   (FDRD_KIND) :: AVERAGE_TBFUV(1:2) , AVERAGE_TBFUV_MOD

    REAL   (FDRD_KIND) :: AVERAGE_NUMFR(1:2) , AVERAGE_NUMFR_MOD
    REAL   (FDRD_KIND) :: AVERAGE_NUMFRS(1:2) , AVERAGE_NUMFRS_MOD

    REAL   (FDRD_KIND) :: AVERAGE_QUV(1:2)

  END MODULE MR_DEF_FIELD_VARS_AVERAGE