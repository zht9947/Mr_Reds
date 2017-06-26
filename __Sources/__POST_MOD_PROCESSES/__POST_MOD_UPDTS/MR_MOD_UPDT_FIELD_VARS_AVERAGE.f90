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
!   2015-03-26    |     DR. HYDE     |    ORIGINAL CODE.
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
!   2015-03-26    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_UPDT_FIELD_VARS_AVERAGE

    USE MR_MOD_OPERATOR_UV
    USE MR_MOD_OPERATOR_SS

    USE MR_MOD_AVERAGE

    IMPLICIT NONE

  ! UPDATE AVERAGE_H
    CALL MR_AVERAGE_SS( NI , NJ , H , AVERAGE_PLAN_H , AVERAGE_XSEC_H )

  ! UPDATE AVERAGE_TBFUV
    CALL MR_AVERAGE_UV( NI , NJ , TBFUV , AVERAGE_PLAN_TBFUV , AVERAGE_XSEC_TBFUV )

  ! UPDATE AVERAGE_TBUV
    CALL MR_AVERAGE_UV( NI , NJ , TBUV , AVERAGE_PLAN_TBUV , AVERAGE_XSEC_TBUV )

  ! UPDATE AVERAGE_QUV
    CALL MR_AVERAGE_QUV(NI , NJ , ( UVA .MRUVMTP. H ) , AVERAGE_PLAN_QUV , AVERAGE_XSEC_QUV )

  END SUBROUTINE MR_UPDT_FIELD_VARS_AVERAGE

  END MODULE MR_MOD_UPDT_FIELD_VARS_AVERAGE