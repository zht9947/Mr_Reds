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
    USE MR_DEF_FIELD_VARS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_UPDT_KI_N_DI

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

      CALL MR_CALC_EQKD_PRO( NI , NJ , NK , EQKD_PRO )
      CALL MR_CALC_EQKD_GRO( NI , NJ , NK , EQKD_GRO )

      ALLOCATE( EQK_D3(1:NI1(CARD_KIND),1:NJ,1:NK) )
        CALL MR_CALC_EQK_D3( NI , NJ , NK , EQKD_PRO , EQKD_GRO , EQK_D3 )
        ALLOCATE( EQK_A3(1:NI1(CARD_KIND),1:NJ,2:NK), EQK_B3(1:NI1(CARD_KIND),1:NJ,1:NK), EQK_C3(1:NI1(CARD_KIND),1:NJ,1:NK-1) )
          CALL MR_CALC_EQK_A3_B3_C3( NI , NJ , NK , EQK_A3 , EQK_B3 , EQK_C3 )
          !BLOCK
            KI = MAX( MR_TDMA3_SS( NI , NJ , NK , EQK_A3 , EQK_B3 , EQK_C3 , EQK_D3 ) , EPSILON(KI) )
          !END BLOCK
        DEALLOCATE( EQK_A3 , EQK_B3 , EQK_C3 )
      DEALLOCATE( EQK_D3 )

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

  END MODULE MR_MOD_UPDT_KI_N_DI