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
  MODULE MR_MOD_CALC_EQKD_PRO

    USE MR_KINDS

    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_FIELD_VARS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CALC_EQKD_PRO

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:    ) :: TSUV

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
  SUBROUTINE MR_CALC_EQKD_PRO( NI , NJ , NK , EQKD_PRO )

    USE MR_MOD_OPERATOR_UV

    USE MR_MOD_CALC_QADV_N_QDIF_Z
    USE MR_MOD_INTERP_Z

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,    1:NK) :: EQKD_PRO

    REAL   (FDRD_KIND) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,    0:NK) :: EQKD_PRO_W
    REAL   (FDRD_KIND) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,1:2,0:NK) :: TUV_W
    REAL   (FDRD_KIND) , DIMENSION(1:NI1(FDRD_KIND),1:NJ         ) :: TUV_SQR_W

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM
    INTEGER(KKID_KIND) :: K

    ALLOCATE( TSUV(1:NI1(FDRD_KIND),1:NJ,1:2) ) ; TSUV(1:NI,1:NJ,1:2) = 0.0
      CALL MR_CALC_T_Z_UV_W( NI , NJ , NK , UV , EKZ , VZW , TUV_W , TBUV , TSUV )
    DEALLOCATE( TSUV )

    DO K = 0 , NK
      TUV_SQR_W = ( .MRUVSQR. ( JUV .MRUVTFM. TUV_W(:,:,1:2, K ) ) )
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          EQKD_PRO_W( I , J , K ) = TUV_SQR_W( I , J ) / ( EKZ * VZW( I , J , K ) )
        END DO
      END DO
    END DO

    CALL MR_ANTI_INTERP_Z_SS_W( NI , NJ , NK , EQKD_PRO_W , EQKD_PRO )

  END SUBROUTINE MR_CALC_EQKD_PRO

  END MODULE MR_MOD_CALC_EQKD_PRO