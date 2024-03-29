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
!                   UVT, UUT, VVT    :    COVARIANT HORIZONTAL VELOCITY VECTOR
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_MOD_CALC_HYD_ADV_N_DIF_XY

    USE MR_KINDS

    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_FIELD_VARS
    USE MR_DEF_ACTIVITY
    USE MR_DEF_TIMING

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CALC_HYD_ADV_N_DIF_XY

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:    ) :: UVT , UUT , VVT

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
  SUBROUTINE MR_CALC_HYD_ADV_N_DIF_XY( NI , NJ , K , HYD_ADV_XY , HYD_DIF_XY )

    USE MR_MOD_INTERP_XY

    USE MR_MOD_CALC_QADV_N_QDIF_XY
    USE MR_MOD_CALC_GRAD_XY

    USE MR_MOD_OPERATOR_SS
    USE MR_MOD_OPERATOR_UV

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    INTEGER(KKID_KIND) , INTENT(IN ) :: K

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: HYD_ADV_XY , HYD_DIF_XY

    REAL   (FDRD_KIND) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ,1:2) :: HYD_QADV_XY_U , HYD_QDIF_XY_U
    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ,1:2) :: HYD_QADV_XY_V , HYD_QDIF_XY_V

    ALLOCATE( UVT(1:NI1(NI,FDRD_KIND),1:NJ,1:2) )

      CALL MR_COPY_UV( UVT , JUV .MRUVTFM. UV(:,:,1:2, K ) )

      ALLOCATE( VVT(1:NI1(NI,FDRD_KIND),0:NJ,1:2) )
        CALL MR_INTERP_XY_UV_V_TO_GET_U_AT_V( NI , NJ , UV(:,:,1:2, K ) , VVT(:,:,1) )
       !CALL MR_INTERP_XY_UV_V_BY_LINEAR( NI , NJ , UV(:,:,1:2, K ) , VVT(:,:,2) )
        CALL MR_COPY_SS( VVT(:,:,2) , V(:,:, K ) )

        CALL MR_COPY_UV( VVT , JVV .MRUVTFM. VVT )

        CALL MR_CALC_QADV_N_QDIF_XY_UV_U( NI , NJ ,   &
        & UVT , VVT , RB , U(:,:, K ) , HYD_QADV_XY_U , EKXY , VXYU(:,:, K ) , HYD_QDIF_XY_U )

      DEALLOCATE( VVT )

      ALLOCATE( UUT(0:NI0(NI,FDRD_KIND),1:NJ,1:2) )
        CALL MR_INTERP_XY_UV_U_TO_GET_V_AT_U( NI , NJ , UV(:,:,1:2, K ) , UUT(:,:,2) )
       !CALL MR_INTERP_XY_UV_U_BY_LINEAR( NI , NJ , UV(:,:,1:2, K ) , UUT(:,:,1) )
        CALL MR_COPY_SS( UUT(:,:,1) , U(:,:, K ) )

        CALL MR_COPY_UV( UUT , JUU .MRUVTFM. UUT )

        CALL MR_CALC_QADV_N_QDIF_XY_UV_V( NI , NJ ,   &
        & UVT , UUT , RB , V(:,:, K ) , HYD_QADV_XY_V , EKXY , VXYV(:,:, K ) , HYD_QDIF_XY_V )

      DEALLOCATE( UUT )

    DEALLOCATE( UVT )

  ! CALCULATE ADVECTION
  ! REVISE ADVECTION BY THE DEPTH H
    CALL MR_COPY_UV( HYD_QADV_XY_U , HYD_QADV_XY_U .MRUVMTP. HU )
    CALL MR_COPY_UV( HYD_QADV_XY_V , HYD_QADV_XY_V .MRUVMTP. HV )
    CALL MR_CALC_REDC_GRAD_XY_UV( NI , NJ , HYD_QADV_XY_U , HYD_QADV_XY_V , HYD_ADV_XY )
  ! CONTRAVARIANTLY TRANSFORM
    CALL MR_COPY_UV( HYD_ADV_XY , IUV .MRUVTFM. ( HYD_ADV_XY .MRUVDIV. H ) )

  ! CALCULATE DIFFUSION
  ! REVISE DIFFUSION BY THE DEPTH H
    CALL MR_COPY_UV( HYD_QDIF_XY_U , HYD_QDIF_XY_U .MRUVMTP. HU )
    CALL MR_COPY_UV( HYD_QDIF_XY_V , HYD_QDIF_XY_V .MRUVMTP. HV )
    CALL MR_CALC_REDC_GRAD_XY_UV( NI , NJ , HYD_QDIF_XY_U , HYD_QDIF_XY_V , HYD_DIF_XY )
  ! CONTRAVARIANTLY TRANSFORM
    CALL MR_COPY_UV( HYD_DIF_XY , IUV .MRUVTFM. ( HYD_DIF_XY .MRUVDIV. H ) )

  END SUBROUTINE MR_CALC_HYD_ADV_N_DIF_XY

  END MODULE MR_MOD_CALC_HYD_ADV_N_DIF_XY