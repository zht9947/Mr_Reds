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
  MODULE MR_MOD_CALC_EQKD_PRO_Z

    USE MR_KINDS

    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_FIELD_VARS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CALC_EQKD_PRO_Z

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
  SUBROUTINE MR_CALC_EQKD_PRO_Z( NI , NJ , NK , EQKD_PRO_Z )

    USE MR_MOD_OPERATOR_UV

    USE MR_MOD_CALC_QADV_N_QDIF_Z
    USE MR_MOD_INTERP_Z

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:NK) :: EQKD_PRO_Z

    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2,1:NK) :: HYD_T_Z
    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2,0:NK) :: HYD_T_Z_W

    INTEGER(IJID_KIND) :: I , J
    INTEGER(KKID_KIND) :: K

    CALL MR_CALC_T_Z_UV_W( NI , NJ , NK , UV , RB , W , EKZ , VZW , HYD_T_Z_W , TUV0=TBUV )

  ! CALCULATE MECHANICAL PRODUCTION
    CALL MR_INV_INTERP_Z_UV_FROM_UVW( NI , NJ , NK , HYD_T_Z_W , HYD_T_Z )
    DO K = 1 , NK
      HYD_T_Z(:,:,1:2, K ) = ( JUV .MRUVTFM. HYD_T_Z(:,:,1:2, K ) )
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          EQKD_PRO_Z( I , J , K ) = SUM( HYD_T_Z( I , J ,1:2, K ) * HYD_T_Z( I , J ,1:2, K ) ) / ( EKZ * VZWW( I , J , K ) )
        END DO
      END DO

    END DO

  END SUBROUTINE MR_CALC_EQKD_PRO_Z

  END MODULE MR_MOD_CALC_EQKD_PRO_Z