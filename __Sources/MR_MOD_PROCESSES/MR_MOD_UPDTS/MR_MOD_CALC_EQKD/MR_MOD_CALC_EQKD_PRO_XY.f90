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
  MODULE MR_MOD_CALC_EQKD_PRO_XY

    USE MR_KINDS

    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_FIELD_VARS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CALC_EQKD_PRO_XY

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
!   2015-06-10    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_CALC_EQKD_PRO_XY( NI , NJ , NK , EQKD_PRO_XY )

    USE MR_MOD_OPERATOR_UV
    USE MR_MOD_OPERATOR_XUV

    USE MR_MOD_CALC_QADV_N_QDIF_XY
    USE MR_MOD_INTERP_XY

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:NK) :: EQKD_PRO_XY

    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2,1:2) :: HYD_T_XY , HYD_T_XY_EXT
    REAL   (FDRD_KIND) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ,1:2    ) :: HYD_T_XY_U
    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ,1:2    ) :: HYD_T_XY_V

    INTEGER(IJID_KIND) :: I , J
    INTEGER(KKID_KIND) :: K

    DO K = 1 , NK

      ALLOCATE( UVT(1:NI1(NI,FDRD_KIND),1:NJ,1:2) )

        UVT = ( JUV .MRUVTFM. UV(:,:,1:2, K ) )

        ALLOCATE( VVT(1:NI1(NI,FDRD_KIND),0:NJ,1:2) )
          CALL MR_INTERP_XY_UV_V_TO_GET_U_AT_V( NI , NJ , UV(:,:,1:2, K ) , VVT(:,:,1) )
         !CALL MR_INTERP_XY_UV_V_BY_LINEAR( NI , NJ , UV(:,:,1:2, K ) , VVT(:,:,2) )
          VVT(:,:,2) = V(:,:, K )

          VVT = ( JVV .MRUVTFM. VVT )

          CALL MR_CALC_T_XY_UV_U( NI , NJ , UVT , VVT , RB , U(:,:, K ) , EKXY , VXYU(:,:, K ) , HYD_T_XY_U )

        DEALLOCATE( VVT )

        ALLOCATE( UUT(0:NI0(NI,FDRD_KIND),1:NJ,1:2) )
          CALL MR_INTERP_XY_UV_U_TO_GET_V_AT_U( NI , NJ , UV(:,:,1:2, K ) , UUT(:,:,2) )
         !CALL MR_INTERP_XY_UV_U_BY_LINEAR( NI , NJ , UV(:,:,1:2, K ) , UUT(:,:,1) )
          UUT(:,:,1) = U(:,:, K )

          UUT = ( JUU .MRUVTFM. UUT )

          CALL MR_CALC_T_XY_UV_V( NI , NJ , UVT , UUT , RB , V(:,:, K ) , EKXY , VXYV(:,:, K ) , HYD_T_XY_V )

        DEALLOCATE( UUT )

      DEALLOCATE( UVT )

    ! CALCULATE MECHANICAL PRODUCTION
      CALL MR_INV_INTERP_XY_UV_FROM_UU( NI , NJ , HYD_T_XY_U , HYD_T_XY(:,:,1:2,1) )
      CALL MR_INV_INTERP_XY_UV_FROM_VV( NI , NJ , HYD_T_XY_V , HYD_T_XY(:,:,1:2,2) )
      HYD_T_XY = ( HYD_T_XY .MRXUVMAT. ( .MRXUVTPS. JUV ) )
      HYD_T_XY_EXT = ( HYD_T_XY + ( .MRXUVTPS. HYD_T_XY ) )
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          EQKD_PRO_XY( I , J , K ) = SUM( HYD_T_XY_EXT( I , J ,1:2,1:2) * HYD_T_XY( I , J ,1:2,1:2) ) / ( EKXY * VZWW( I , J , K ) )
        END DO
      END DO

    END DO

  END SUBROUTINE MR_CALC_EQKD_PRO_XY

  END MODULE MR_MOD_CALC_EQKD_PRO_XY