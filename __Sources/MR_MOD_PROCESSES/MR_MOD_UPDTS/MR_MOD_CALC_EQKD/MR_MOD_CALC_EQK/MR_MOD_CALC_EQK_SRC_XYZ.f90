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
  MODULE MR_MOD_CALC_EQK_SRC_XYZ

    USE MR_KINDS

    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_FIELD_VARS
    USE MR_DEF_ACTIVITY
    USE MR_DEF_TIMING

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CALC_EQK_SRC_XYZ

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
  SUBROUTINE MR_CALC_EQK_SRC_XYZ( NI , NJ , K , EQKD_PRO_XY_K , EQKD_PRO_Z_K , EQKD_BRO_Z_K , EQK_SRC_XYZ )

    USE MR_MOD_OPERATOR_SS

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    INTEGER(KKID_KIND) , INTENT(IN ) :: K

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) :: EQKD_PRO_XY_K , EQKD_PRO_Z_K , EQKD_BRO_Z_K
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) :: EQK_SRC_XYZ

    EQK_SRC_XYZ = + DT * RBT *   &
    ( MW .MRSSSCL.   &
      ( REAL( ( ( EQKD_PRO_XY_K + EQKD_PRO_Z_K ) - DI(:,:, K ) + EQKD_BRO_Z_K ) , FDRD_KIND ) )   &
    )

  END SUBROUTINE MR_CALC_EQK_SRC_XYZ

  END MODULE MR_MOD_CALC_EQK_SRC_XYZ