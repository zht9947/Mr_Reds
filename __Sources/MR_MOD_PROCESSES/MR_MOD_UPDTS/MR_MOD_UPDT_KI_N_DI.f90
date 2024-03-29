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
  MODULE MR_MOD_UPDT_KI_N_DI

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_FIELD_VARS

    USE MR_MOC_K_EPS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_UPDT_KI_N_DI

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:      ) :: TBUV_MOD

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,  :  ) :: EQKD_PRO_XY , EQKD_PRO_Z , EQKD_BRO_Z

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_UPDT_KI_N_DI

    USE MR_MOD_OPERATOR_SS
    USE MR_MOD_OPERATOR_UV

    USE MR_MOD_CALC_EQKD_PRO_XY
    USE MR_MOD_CALC_EQKD_PRO_Z
    USE MR_MOD_CALC_EQKD_BRO_Z

    USE MR_MOD_CALC_EQK_A3_B3_C3_D3
    USE MR_MOD_CALC_EQD_A3_B3_C3_D3

    USE MR_MOD_TDMA3

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER(KKID_KIND) :: K

    ALLOCATE( EQKD_PRO_XY(1:NI1(NI,FDRD_KIND),1:NJ,1:NK) ,   &
    & EQKD_PRO_Z(1:NI1(NI,FDRD_KIND),1:NJ,1:NK) ,   &
    & EQKD_BRO_Z(1:NI1(NI,FDRD_KIND),1:NJ,1:NK) )

      CALL MR_CALC_EQKD_PRO_XY( NI , NJ , NK , EQKD_PRO_XY )
      CALL MR_CALC_EQKD_PRO_Z(  NI , NJ , NK , EQKD_PRO_Z  )
      CALL MR_CALC_EQKD_BRO_Z(  NI , NJ , NK , EQKD_BRO_Z  )

     !BLOCK
    ! CALCULATE KI(:,:, 1 )
      ALLOCATE( TBUV_MOD(1:NI1(NI,FDRD_KIND),1:NJ) )
        CALL MR_COPY_SS( TBUV_MOD , .MRSSQRT. ( .MRUVSQR. ( JUV .MRUVTFM. TBUV ) ) )
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            KI( I , J , 1 ) = TBUV_MOD( I , J ) / ( SQRT(CV0) )
          END DO
        END DO
      DEALLOCATE( TBUV_MOD )
     !END BLOCK

      ALLOCATE( EQK_D3(1:NI1(NI,CARD_KIND),1:NJ,1:NK) )
        CALL MR_CALC_EQK_D3( NI , NJ , NK , EQKD_PRO_XY , EQKD_PRO_Z , EQKD_BRO_Z , EQK_D3 )
        ALLOCATE( EQK_A3(1:NI1(NI,CARD_KIND),1:NJ,2:NK) ,   &
        & EQK_B3(1:NI1(NI,CARD_KIND),1:NJ,1:NK  ) ,   &
        & EQK_C3(1:NI1(NI,CARD_KIND),1:NJ,1:NK-1) )
          CALL MR_CALC_EQK_A3_B3_C3( NI , NJ , NK , EQK_A3 , EQK_B3 , EQK_C3 )
          !BLOCK
            CALL MR_TDMA3_SS( NI , NJ , NK , EQK_A3 , EQK_B3 , EQK_C3 , EQK_D3 , KI , EPSILON(KI) )
          !END BLOCK
        DEALLOCATE( EQK_A3 , EQK_B3 , EQK_C3 )
      DEALLOCATE( EQK_D3 )

     !BLOCK
    ! CALCULATE DI(:,:, 1 ), DI(:,:,NK )
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          DI( I , J , 1 ) = ( ( KI( I , J , 1 ) * SQRT(CV0) )**1.5 ) / ( SQRT(RBT) * KAR * H( I , J ) * ( 0.5 * DSIGMA ) )
          DI( I , J ,NK ) = ( ( KI( I , J ,NK )             )**1.5 ) / ( SQRT(RBT) *0.430* H( I , J )                    )
        END DO
      END DO
     !END BLOCK

      ALLOCATE( EQD_D3(1:NI1(NI,CARD_KIND),1:NJ,1:NK) )
        CALL MR_CALC_EQD_D3( NI , NJ , NK , EQKD_PRO_XY , EQKD_PRO_Z , EQD_D3 )
        ALLOCATE( EQD_A3(1:NI1(NI,CARD_KIND),1:NJ,2:NK) ,   &
        & EQD_B3(1:NI1(NI,CARD_KIND),1:NJ,1:NK  ) ,   &
        & EQD_C3(1:NI1(NI,CARD_KIND),1:NJ,1:NK-1) )
          CALL MR_CALC_EQD_A3_B3_C3( NI , NJ , NK , EQD_A3 , EQD_B3 , EQD_C3 )
          !BLOCK
            CALL MR_TDMA3_SS( NI , NJ , NK , EQD_A3 , EQD_B3 , EQD_C3 , EQD_D3 , DI , EPSILON(DI) )
          !END BLOCK
        DEALLOCATE( EQD_A3 , EQD_B3 , EQD_C3 )
      DEALLOCATE( EQD_D3 )

    DEALLOCATE( EQKD_PRO_XY , EQKD_PRO_Z , EQKD_BRO_Z )

  END SUBROUTINE MR_UPDT_KI_N_DI

  END MODULE MR_MOD_UPDT_KI_N_DI