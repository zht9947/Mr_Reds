#INCLUDE 'MR_H_ALIGN_PADDING.H'
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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_MOD_UPDT_FIELD_VARS_AVERAGE

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_FIELD_VARS
    USE MR_DEF_FIELD_VARS_AVERAGE

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_UPDT_FIELD_VARS_AVERAGE

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:  ) :: UNIT_FORCES , UNIT_MOTION
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:  ) :: UNIT_ENERGY

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:  ) :: QUV

!***********************************************************************************************************************************

  CONTAINS

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE)
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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_UPDT_FIELD_VARS_AVERAGE

    USE MR_MOD_OPERATOR_UV
    USE MR_MOD_OPERATOR_SS

    USE MR_MOD_AVERAGE

    USE MR_MOD_CALC_UNIT_FORCES_N_MOTION
    USE MR_MOD_CALC_UNIT_ENERGY

    IMPLICIT NONE

  ! UPDATE AVERAGE_H
    CALL MR_AVERAGE_SS_BY_AREA( NI , NJ , H , AVERAGE_PLAN_H , AVERAGE_XSEC_H )

  ! UPDATE AVERAGE_UNIT_FORCES AND AVERAGE_UNIT_MOTION
    ALLOCATE( UNIT_FORCES(1:NI1(NI,FDRD_KIND),1:NJ,1:2) , UNIT_MOTION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) )
    CALL MR_CALC_UNIT_FORCES_N_MOTION( NI , NJ , NK , UNIT_FORCES , UNIT_MOTION )
    CALL MR_AVERAGE_UV_BY_AREA( NI , NJ , UNIT_FORCES , AVERAGE_PLAN_UNIT_FORCES , AVERAGE_XSEC_UNIT_FORCES  )
    CALL MR_AVERAGE_UV_BY_AREA( NI , NJ , UNIT_MOTION , AVERAGE_PLAN_UNIT_MOTION , AVERAGE_XSEC_UNIT_MOTION )
    DEALLOCATE( UNIT_FORCES , UNIT_MOTION )

  ! UPDATE AVERAGE_UNIT_ENERGY
    ALLOCATE( UNIT_ENERGY(1:NI1(NI,FDRD_KIND),1:NJ,1:2) )
    CALL MR_CALC_UNIT_ENERGY( NI , NJ , NK , UNIT_ENERGY )
    CALL MR_AVERAGE_SS_BY_AREA( NI , NJ , UNIT_ENERGY(:,:,1) , AVERAGE_PLAN_UNIT_ENERGY , AVERAGE_XSEC_UNIT_ENERGY )
    DEALLOCATE( UNIT_ENERGY )

  ! UPDATE AVERAGE_TBFUV
    CALL MR_AVERAGE_UV_BY_AREA( NI , NJ , TBFUV , AVERAGE_PLAN_TBFUV , AVERAGE_XSEC_TBFUV )
    CALL MR_AVERAGE_SS_BY_AREA( NI , NJ ,   &
    & ( .MRSSQRT. ( .MRUVSQR. ( JUV .MRUVTFM. TBFUV ) ) ) , AVERAGE_PLAN_TBFUV_MOD , AVERAGE_XSEC_TBFUV_MOD )

  ! UPDATE AVERAGE_TBUV
    CALL MR_AVERAGE_UV_BY_AREA( NI , NJ , TBUV , AVERAGE_PLAN_TBUV , AVERAGE_XSEC_TBUV )
    CALL MR_AVERAGE_SS_BY_AREA( NI , NJ ,   &
    & ( .MRSSQRT. ( .MRUVSQR. ( JUV .MRUVTFM. TBUV ) ) ) , AVERAGE_PLAN_TBUV_MOD , AVERAGE_XSEC_TBUV_MOD )

  ! UPDATE AVERAGE_QUV
    ALLOCATE( QUV(1:NI1(NI,FDRD_KIND),1:NJ,1:2) )
    QUV = UVA .MRUVMTP. H
    CALL MR_AVERAGE_UV_BY_LINE(NI , NJ , QUV , AVERAGE_PLAN_QUV , AVERAGE_XSEC_QUV )
    DEALLOCATE( QUV )

  END SUBROUTINE MR_UPDT_FIELD_VARS_AVERAGE

  END MODULE MR_MOD_UPDT_FIELD_VARS_AVERAGE