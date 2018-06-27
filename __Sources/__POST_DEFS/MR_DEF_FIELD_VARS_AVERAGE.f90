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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_DEF_FIELD_VARS_AVERAGE

    USE MR_KINDS

    IMPLICIT NONE

    REAL   (FDRD_KIND)                                  :: AVERAGE_PLAN_H
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:    ) :: AVERAGE_XSEC_H

    REAL   (FDRD_KIND) ,               DIMENSION(  1:2) :: AVERAGE_PLAN_UNIT_FORCES
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:, : ) :: AVERAGE_XSEC_UNIT_FORCES

    REAL   (FDRD_KIND) ,               DIMENSION(  1:2) :: AVERAGE_PLAN_UNIT_MOTION
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:, : ) :: AVERAGE_XSEC_UNIT_MOTION

    REAL   (FDRD_KIND)                                  :: AVERAGE_PLAN_UNIT_ENERGY
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:    ) :: AVERAGE_XSEC_UNIT_ENERGY

    REAL   (FDRD_KIND) ,               DIMENSION(  1:2) :: AVERAGE_PLAN_TBFUV
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:, : ) :: AVERAGE_XSEC_TBFUV
    REAL   (FDRD_KIND)                                  :: AVERAGE_PLAN_TBFUV_MOD
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:    ) :: AVERAGE_XSEC_TBFUV_MOD

    REAL   (FDRD_KIND) ,               DIMENSION(  1:2) :: AVERAGE_PLAN_TBUV
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:, : ) :: AVERAGE_XSEC_TBUV
    REAL   (FDRD_KIND)                                  :: AVERAGE_PLAN_TBUV_MOD
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:    ) :: AVERAGE_XSEC_TBUV_MOD

    REAL   (FDRD_KIND) ,               DIMENSION(  1:2) :: AVERAGE_PLAN_QUV
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:, : ) :: AVERAGE_XSEC_QUV

  END MODULE MR_DEF_FIELD_VARS_AVERAGE