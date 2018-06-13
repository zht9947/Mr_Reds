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
  MODULE MR_MOD_UPDT_HYD

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_FIELD_VARS
    USE MR_DEF_ACTIVITY

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_UPDT_HYD
    PUBLIC :: MR_CALC_UVA , MR_CALC_W   !JUST FOR TEST

    REAL   (CARD_KIND) , ALLOCATABLE , DIMENSION(:,:,:    ) :: ALFA , BETA

    REAL   (CARD_KIND) , ALLOCATABLE , DIMENSION(:,:      ) :: A1 , B1 , C1 , D1
    REAL   (CARD_KIND) , ALLOCATABLE , DIMENSION(:,:      ) :: A2 , B2 , C2 , D2

    REAL   (CARD_KIND) , ALLOCATABLE , DIMENSION(:,:,  :  ) :: HYD_A3 , HYD_B3 , HYD_C3
    REAL   (CARD_KIND) , ALLOCATABLE , DIMENSION(:,:,:,:  ) :: HYD_D3

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
  SUBROUTINE MR_UPDT_HYD

    USE MR_MOD_INTERP_XY

    USE MR_MOD_CALC_ALFA_N_BETA
    USE MR_MOD_CALC_A_B_C_D
    USE MR_MOD_CALC_HYD_A3_B3_C3_D3

    USE MR_MOD_TDMA
    USE MR_MOD_TDMA3

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM
    INTEGER(KKID_KIND) :: K

    ALLOCATE( HYD_D3(1:NI1(NI,CARD_KIND),1:NJ,1:2,1:NK) )

      CALL MR_CALC_RAW_HYD_D3( NI , NJ , NK , HYD_D3 )

      ALLOCATE( ALFA(1:NI1(NI,CARD_KIND),1:NJ,1:2) , BETA(1:NI1(NI,CARD_KIND),1:NJ,1:2) )

       !BLOCK
        CALL MR_CALC_ALFA_N_BETA( NI , NJ , NK , HYD_D3 , ALFA , BETA )
      ! HYD_D3 WILL BE REVISED IN THIS CALL
       !END BLOCK

        ALLOCATE( D1(1:NI1(NI,CARD_KIND),1:NJ) )
          CALL MR_CALC_D1( NI , NJ , ZS , ALFA , BETA , D1 )
          ALLOCATE( A1(2:NI2(NI,CARD_KIND),1:NJ) ,   &
          & B1(1:NI1(NI,CARD_KIND),1:NJ  ) ,   &
          & C1(1:NI2(NI,CARD_KIND)-1,1:NJ) )
            CALL MR_CALC_A1_B1_C1( NI , NJ , ALFA , A1 , B1 , C1 )
            !BLOCK
              ZS = MR_TDMA1( NI , NJ , A1 , B1 , C1 , D1 )
            !END BLOCK
          DEALLOCATE( A1 , B1 , C1 )
        DEALLOCATE( D1 )

        ALLOCATE( D2(1:NI1(NI,CARD_KIND),1:NJ) )
          CALL MR_CALC_D2( NI , NJ , ZS , BETA , D2 )
          ALLOCATE( A2(1:NI1(NI,CARD_KIND),2:NJ) ,   &
          & B2(1:NI1(NI,CARD_KIND),1:NJ  ) ,   &
          & C2(1:NI1(NI,CARD_KIND),1:NJ-1) )
            CALL MR_CALC_A2_B2_C2( NI , NJ , ALFA , A2 , B2 , C2 )
            !BLOCK
              ZS = MR_TDMA2( NI , NJ , A2 , B2 , C2 , D2 )
            !END BLOCK
          DEALLOCATE( A2 , B2 , C2 )
        DEALLOCATE( D2 )

       !BLOCK
        CALL MR_INTERP_XY_SS_U( NI , NJ , ZS , ZSU )
        CALL MR_INTERP_XY_SS_V( NI , NJ , ZS , ZSV )
      ! A DIFFERENT INTERPOLATION PRACTICE FOR ZS MAY BE CONSIDERED
      ! CALL MR_INTERP_XY_ZS_U( NI , NJ , ZS , ZSU )
      ! CALL MR_INTERP_XY_ZS_V( NI , NJ , ZS , ZSV )
       !END BLOCK

        CALL MR_INTERP_XY_UV_U_BY_RCPRAC( NI , NJ , ZS , ALFA , BETA , UA )
        CALL MR_INTERP_XY_UV_V_BY_RCPRAC( NI , NJ , ZS , ALFA , BETA , VA )

        CALL MR_CALC_UVA( NI , NJ , ZSU , ZSV , ALFA , BETA , UVA )

      DEALLOCATE( ALFA , BETA )

      ALLOCATE( HYD_A3(1:NI1(NI,CARD_KIND),1:NJ,2:NK) ,   &
      & HYD_B3(1:NI1(NI,CARD_KIND),1:NJ,1:NK  ) ,   &
      & HYD_C3(1:NI1(NI,CARD_KIND),1:NJ,1:NK-1) )
        CALL MR_CALC_HYD_A3_B3_C3( NI , NJ , NK , HYD_A3 , HYD_B3 , HYD_C3 )
        !BLOCK
          UV = MR_TDMA3_UV( NI , NJ , NK , HYD_A3 , HYD_B3 , HYD_C3 , HYD_D3 )
        !END BLOCK
      DEALLOCATE( HYD_A3 , HYD_B3 , HYD_C3 )

    DEALLOCATE( HYD_D3 )

    DO K = 1 , NK
      CALL MR_INTERP_XY_UV_U_BY_LINEAR( NI , NJ , UV(:,:,1:2, K ) , U(:,:, K ) )
      CALL MR_INTERP_XY_UV_V_BY_LINEAR( NI , NJ , UV(:,:,1:2, K ) , V(:,:, K ) )
    END DO

  ! CALCULATE VERTICAL VELOCITY
    CALL MR_CALC_W( NI , NJ , NK , U , V , W )

  ! MERGE EXTERNAL AND INTERNAL
  ! COMPONENTS OF HORIZONTAL VELOCITY TOGETHER
    DO K = 1 , NK

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 0 , NI
          U( I , J , K ) = U( I , J , K ) + UA( I , J )
        END DO
      END DO

      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          V( I , J , K ) = V( I , J , K ) + VA( I , J )
        END DO
      END DO

      DO DIM = 1 , 2
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            UV( I , J ,DIM, K ) = UV( I , J ,DIM, K ) + UVA( I , J ,DIM)
          END DO
        END DO
      END DO

    END DO

  END SUBROUTINE MR_UPDT_HYD

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
  SUBROUTINE MR_CALC_UVA( NI , NJ , ZSU , ZSV , ALFA , BETA , UVA )

    USE MR_MOD_CALC_GRAD_XY

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ    ) :: ZSU
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ    ) :: ZSV

    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND),1:NJ,1:2) :: ALFA , BETA

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: UVA

    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: GRAD_XY_ZS

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    CALL MR_CALC_GRAD_XY_SS( NI , NJ , ZSU , ZSV , GRAD_XY_ZS )

    DO DIM = 1 , 2

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          UVA( I , J ,DIM) = ( ALFA( I , J ,DIM) * GRAD_XY_ZS( I , J ,DIM) + BETA( I , J ,DIM) ) / MW( I , J ) / H( I , J )
        END DO
      END DO

    END DO

  END SUBROUTINE MR_CALC_UVA

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
  SUBROUTINE MR_CALC_W( NI , NJ , NK , UD , VD , W )

    USE MR_MOD_OPERATOR_SS
    USE MR_MOD_CALC_GRAD_XY

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ,1:NK) :: UD
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ,1:NK) :: VD

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,0:NK) :: W

    REAL   (FDRD_KIND) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) :: REDC_GRAD_UVD

    INTEGER(IJID_KIND) :: I , J
    INTEGER(KKID_KIND) :: K

    W(1:NI,1:NJ, 0 ) = 0.0

    DO K = 1 , NK

      CALL MR_CALC_REDC_GRAD_XY_SS( NI , NJ ,   &
      & ( MU .MRSSSCL. ( UD(:,:, K ) .MRSSMTP. HU ) ) , ( MV .MRSSSCL. ( VD(:,:, K ) .MRSSMTP. HV ) ) , REDC_GRAD_UVD )

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          W( I , J , K ) = W( I , J ,K-1) -   &
          & DSIGMA * REDC_GRAD_UVD( I , J ) / MW( I , J ) / H( I , J )
        END DO
      END DO

    END DO

  END SUBROUTINE MR_CALC_W

  END MODULE MR_MOD_UPDT_HYD