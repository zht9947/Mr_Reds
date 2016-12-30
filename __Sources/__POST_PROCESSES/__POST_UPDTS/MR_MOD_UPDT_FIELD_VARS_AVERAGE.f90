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
    CALL MR_AVERAGE_SS( NI , NJ , H , AVERAGE_H )

  ! UPDATE AVERAGE_UV
    CALL MR_AVERAGE_UV( NI , NJ , ( UVA .MRUVMTP. H ) , AVERAGE_UV )
    CALL MR_AVERAGE_SS( NI , NJ , ( .MRSSQRT. ( .MRUVSQR. ( JUV .MRUVTFM. ( UVA .MRUVMTP. H ) ) ) ) , AVERAGE_UV_MOD )
    AVERAGE_UV = AVERAGE_UV / AVERAGE_H
    AVERAGE_UV_MOD = AVERAGE_UV_MOD / AVERAGE_H

  ! UPDATE AVERAGE_TBUV
    CALL MR_AVERAGE_UV( NI , NJ , TBUV , AVERAGE_TBUV )
    CALL MR_AVERAGE_SS( NI , NJ , ( .MRSSQRT. ( .MRUVSQR. ( JUV .MRUVTFM. TBUV ) ) ) , AVERAGE_TBUV_MOD )

  ! UPDATE AVERAGE_NUMFR
    CALL MR_AVERAGE_UV( NI , NJ , NUMFR , AVERAGE_NUMFR )
    CALL MR_AVERAGE_SS( NI , NJ , ( .MRSSQRT. ( .MRUVSQR. ( JUV .MRUVTFM. NUMFR ) ) ) , AVERAGE_NUMFR_MOD )

  ! UPDATE AVERAGE_NUMFRS
    CALL MR_AVERAGE_UV( NI , NJ , NUMFRS , AVERAGE_NUMFRS )
    CALL MR_AVERAGE_SS( NI , NJ , ( .MRSSQRT. ( .MRUVSQR. ( JUV .MRUVTFM. NUMFRS ) ) ) , AVERAGE_NUMFRS_MOD )

  ! UPDATE AVERAGE_QUV
    CALL MR_AVERAGE_QUV( NI , NJ , ( UVA .MRUVMTP. H ) , AVERAGE_QUV )

  END SUBROUTINE MR_UPDT_FIELD_VARS_AVERAGE

  END MODULE MR_MOD_UPDT_FIELD_VARS_AVERAGE