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
  MODULE MR_MOD_CALC_UNIT_FORCES_N_MOTION

    USE MR_KINDS

    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_FIELD_VARS
    USE MR_DEF_SLOPE
    USE MR_DEF_ACTIVITY
    USE MR_DEF_TIMING

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CALC_UNIT_FORCES_N_MOTION

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
  SUBROUTINE MR_CALC_UNIT_FORCES_N_MOTION( NI , NJ , NK , UNIT_FORCES , UNIT_MOTION )

    USE MR_MOD_INTERP_XY

    USE MR_MOD_CALC_QADV_N_QDIF_XY
    USE MR_MOD_CALC_GRAD_XY

    USE MR_MOD_OPERATOR_UV

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: UNIT_FORCES , UNIT_MOTION

    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: HYD_ADV_XY , HYD_DIF_XY

    REAL   (FDRD_KIND) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ,1:2) :: HYD_QADV_XY_U , HYD_QDIF_XY_U
    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ,1:2) :: HYD_QADV_XY_V , HYD_QDIF_XY_V

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM
    INTEGER(KKID_KIND) :: K

    CALL MR_CALC_UNIT_INITIATION

    DO K = 1 , NK

      ALLOCATE( UVT(1:NI1(NI,FDRD_KIND),1:NJ,1:2) )

        UVT = ( JUV .MRUVTFM. UV(:,:,1:2, K ) )

        ALLOCATE( VVT(1:NI1(NI,FDRD_KIND),0:NJ,1:2) )
          CALL MR_INTERP_XY_UV_V_TO_GET_U_AT_V( NI , NJ , UV(:,:,1:2, K ) , VVT(:,:,1) )
         !CALL MR_INTERP_XY_UV_V_BY_LINEAR( NI , NJ , UV(:,:,1:2, K ) , VVT(:,:,2) )
          VVT(:,:,2) = V(:,:, K )

          VVT = ( JVV .MRUVTFM. VVT )

          CALL MR_CALC_QADV_N_QDIF_XY_UV_U( NI , NJ ,   &
          & UVT , VVT , RB , U(:,:, K ) , HYD_QADV_XY_U , EKXY , VXYU(:,:, K ) , HYD_QDIF_XY_U )

        DEALLOCATE( VVT )

        ALLOCATE( UUT(0:NI0(NI,FDRD_KIND),1:NJ,1:2) )
          CALL MR_INTERP_XY_UV_U_TO_GET_V_AT_U( NI , NJ , UV(:,:,1:2, K ) , UUT(:,:,2) )
         !CALL MR_INTERP_XY_UV_U_BY_LINEAR( NI , NJ , UV(:,:,1:2, K ) , UUT(:,:,1) )
          UUT(:,:,1) = U(:,:, K )

          UUT = ( JUU .MRUVTFM. UUT )

          CALL MR_CALC_QADV_N_QDIF_XY_UV_V( NI , NJ ,   &
          & UVT , UUT , RB , V(:,:, K ) , HYD_QADV_XY_V , EKXY , VXYV(:,:, K ) , HYD_QDIF_XY_V )

        DEALLOCATE( UUT )

      DEALLOCATE( UVT )

    ! CALCULATE ADVECTION
    ! REVISE ADVECTION BY THE DEPTH H
      HYD_QADV_XY_U = ( HYD_QADV_XY_U .MRUVMTP. HU )
      HYD_QADV_XY_V = ( HYD_QADV_XY_V .MRUVMTP. HV )
      CALL MR_CALC_REDC_GRAD_XY_UV( NI , NJ , HYD_QADV_XY_U , HYD_QADV_XY_V , HYD_ADV_XY )
    ! CONTRAVARIANTLY TRANSFORM
      HYD_ADV_XY = ( IUV .MRUVTFM. ( HYD_ADV_XY .MRUVDIV. H ) )

    ! CALCULATE DIFFUSION
      CALL MR_CALC_REDC_GRAD_XY_UV( NI , NJ , HYD_QDIF_XY_U , HYD_QDIF_XY_V , HYD_DIF_XY )
    ! CONTRAVARIANTLY TRANSFORM
      HYD_DIF_XY = ( IUV .MRUVTFM. ( HYD_DIF_XY ) )

      DO DIM = 1 , 2

        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            UNIT_MOTION( I , J ,DIM) = UNIT_MOTION( I , J ,DIM)   &
            & - DSIGMA * H( I , J ) * HYD_ADV_XY( I , J ,DIM) / ( DT * MW( I , J ) )
            UNIT_FORCES( I , J ,DIM) = UNIT_FORCES( I , J ,DIM)   &
            & + DSIGMA * H( I , J ) * HYD_DIF_XY( I , J ,DIM) / ( DT * MW( I , J ) )
          END DO
        END DO

      END DO

    END DO

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
  SUBROUTINE MR_CALC_UNIT_INITIATION

    USE MR_MOD_CALC_GRAD_XY

    IMPLICIT NONE

    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: GRAD_XY_ZS

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    CALL MR_CALC_GRAD_XY_SS( NI , NJ , ZSU , ZSV , GRAD_XY_ZS )

    DO DIM = 1 , 2

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          IF( ACTIVITY( I , J ) == NOACTIVE ) THEN
            UNIT_MOTION( I , J ,DIM) = 0.0
            UNIT_FORCES( I , J ,DIM) = 0.0
          ELSE
            UNIT_MOTION( I , J ,DIM) = 0.0
            UNIT_FORCES( I , J ,DIM) =   &
            - TBUV( I , J ,DIM)   &
            + H( I , J ) *   &
              ( XYR/ZR / ALPAR * FUV( I , J ,DIM,1) * SQRT(  GUV( I , J ,1,1)  ) * SLOPE   &   ! ADDITIONAL SLOPE TERM
              - (                FUV( I , J ,DIM,1) * GRAD_XY_ZS( I , J ,1)   &
                +                FUV( I , J ,DIM,2) * GRAD_XY_ZS( I , J ,2)   &
                )   &
            & )
          END IF
        END DO
      END DO

    END DO

  END SUBROUTINE MR_CALC_UNIT_INITIATION

  END SUBROUTINE MR_CALC_UNIT_FORCES_N_MOTION

  END MODULE MR_MOD_CALC_UNIT_FORCES_N_MOTION