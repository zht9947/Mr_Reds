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
  MODULE MR_MOD_UPDT_KI_N_DI

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_FIELD_VARS

    USE MR_MCS_K_EPS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_UPDT_KI_N_DI

    PUBLIC :: MR_CALC_KIB_N_DIB , MR_CALC_DIS

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,  :  ) :: EQKD_PRO , EQKD_GRO

    REAL   (CARD_KIND) , ALLOCATABLE , DIMENSION(:,:,  :  ) :: EQK_A3 , EQK_B3 , EQK_C3
    REAL   (CARD_KIND) , ALLOCATABLE , DIMENSION(:,:,  :  ) :: EQK_D3
    REAL   (CARD_KIND) , ALLOCATABLE , DIMENSION(:,:,  :  ) :: EQD_A3 , EQD_B3 , EQD_C3
    REAL   (CARD_KIND) , ALLOCATABLE , DIMENSION(:,:,  :  ) :: EQD_D3

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
  SUBROUTINE MR_UPDT_KI_N_DI

    USE MR_MOD_CALC_EQKD_PRO
    USE MR_MOD_CALC_EQKD_GRO

    USE MR_MOD_CALC_EQK_A3_B3_C3_D3
    USE MR_MOD_CALC_EQD_A3_B3_C3_D3

    USE MR_MOD_TDMA3

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( EQKD_PRO(1:NI1(FDRD_KIND),1:NJ,1:NK) , EQKD_GRO(1:NI1(FDRD_KIND),1:NJ,1:NK) )

      CALL MR_CALC_EQKD_PRO ( NI , NJ , NK , EQKD_PRO )
      CALL MR_CALC_EQKD_GRO ( NI , NJ , NK , EQKD_GRO )

      CALL MR_CALC_KIB_N_DIB( NI , NJ , TBUV , KIB , DIB )

      ALLOCATE( EQK_D3(1:NI1(CARD_KIND),1:NJ,1:NK) )
        CALL MR_CALC_EQK_D3( NI , NJ , NK , EQKD_PRO , EQKD_GRO , EQK_D3 )
        ALLOCATE( EQK_A3(1:NI1(CARD_KIND),1:NJ,2:NK), EQK_B3(1:NI1(CARD_KIND),1:NJ,1:NK), EQK_C3(1:NI1(CARD_KIND),1:NJ,1:NK-1) )
          CALL MR_CALC_EQK_A3_B3_C3( NI , NJ , NK , EQK_A3 , EQK_B3 , EQK_C3 )
          !BLOCK
            KI = MAX( MR_TDMA3_SS( NI , NJ , NK , EQK_A3 , EQK_B3 , EQK_C3 , EQK_D3 ) , EPSILON(KI) )
          !END BLOCK
        DEALLOCATE( EQK_A3 , EQK_B3 , EQK_C3 )
      DEALLOCATE( EQK_D3 )

      CALL MR_CALC_DIS( NI , NJ , KI(:,:,NK ) , DIS )

      ALLOCATE( EQD_D3(1:NI1(CARD_KIND),1:NJ,1:NK) )
        CALL MR_CALC_EQD_D3( NI , NJ , NK , EQKD_PRO , EQD_D3 )
        ALLOCATE( EQD_A3(1:NI1(CARD_KIND),1:NJ,2:NK), EQD_B3(1:NI1(CARD_KIND),1:NJ,1:NK), EQD_C3(1:NI1(CARD_KIND),1:NJ,1:NK-1) )
          CALL MR_CALC_EQD_A3_B3_C3( NI , NJ , NK , EQD_A3 , EQD_B3 , EQD_C3 )
          !BLOCK
            DI = MAX( MR_TDMA3_SS( NI , NJ , NK , EQD_A3 , EQD_B3 , EQD_C3 , EQD_D3 ) , EPSILON(DI) )
          !END BLOCK
        DEALLOCATE( EQD_A3 , EQD_B3 , EQD_C3 )
      DEALLOCATE( EQD_D3 )

    DEALLOCATE( EQKD_PRO , EQKD_GRO )

  END SUBROUTINE MR_UPDT_KI_N_DI

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
  SUBROUTINE MR_CALC_KIB_N_DIB( NI , NJ , TBUV , KIB , DIB )

    USE MR_MOD_OPERATOR_UV
    USE MR_MOD_OPERATOR_SS

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,1:2) :: TBUV

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(FDRD_KIND),1:NJ    ) :: KIB , DIB

    REAL   (FDRD_KIND) , DIMENSION(1:NI1(FDRD_KIND),1:NJ) :: TBUV_SQR
    REAL   (FDRD_KIND) , DIMENSION(1:NI1(FDRD_KIND),1:NJ) :: TBUV_MOD

    INTEGER(IJID_KIND) :: I , J

    TBUV_SQR = .MRUVSQR. ( JUV .MRUVTFM. TBUV )
    TBUV_MOD = .MRSSQRT. ( TBUV_SQR )

    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        KIB( I , J ) = TBUV_MOD( I , J ) / ( SQRT( CV0 ) )
        DIB( I , J ) = TBUV_SQR( I , J ) / ( KAR * EKZ * (70.0*V0/VZR) )
      END DO
    END DO

  END SUBROUTINE MR_CALC_KIB_N_DIB

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
  SUBROUTINE MR_CALC_DIS( NI , NJ , KIS , DIS )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(FDRD_KIND),1:NJ    ) :: KIS
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(FDRD_KIND),1:NJ    ) :: DIS

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        DIS( I , J ) = 2.33 / ( SQRT( RBT ) ) * ( KIS( I , J )**1.5 ) / H( I , J )
      END DO
    END DO

  END SUBROUTINE MR_CALC_DIS

  END MODULE MR_MOD_UPDT_KI_N_DI