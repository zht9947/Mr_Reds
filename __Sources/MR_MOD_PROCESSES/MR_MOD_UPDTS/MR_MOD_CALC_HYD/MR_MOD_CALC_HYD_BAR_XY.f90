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
  MODULE MR_MOD_CALC_HYD_BAR_XY

    USE MR_KINDS

    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_FIELD_VARS
    USE MR_DEF_ACTIVITY
    USE MR_DEF_TIMING

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CALC_HYD_BAR_XY

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
  SUBROUTINE MR_CALC_HYD_BAR_XY( NI , NJ , K , HYD_BAR_XY )

    USE MR_MOD_OPERATOR_UV

    USE MR_MOD_CALC_GRAD_XY

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    INTEGER(KKID_KIND) , INTENT(IN ) :: K

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: HYD_BAR_XY

    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: GRAD_XY_RI
    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: GRAD_XY_H

    CALL MR_CALC_BY_INTERP_GRAD_XY_SS( NI , NJ , RI(:,:, K ) , GRAD_XY_RI )
    CALL MR_CALC_GRAD_XY_SS( NI , NJ , HU , HV , GRAD_XY_H )

    CALL MR_COPY_UV( HYD_BAR_XY ,   &
    & - DT * RB / FRD *   &
      ( MW .MRUVSCL.   &
        ( FUV .MRUVTFM.   &
          ( ( GRAD_XY_RI .MRUVMTP. H )   &
          + ( GRAD_XY_H  .MRUVMTP. ( RI(:,:, K ) + R(:,:, K ) * SIGMA( K ) ) )   &
          )   &
        )   &
      )   &
    )

  END SUBROUTINE MR_CALC_HYD_BAR_XY

  END MODULE MR_MOD_CALC_HYD_BAR_XY