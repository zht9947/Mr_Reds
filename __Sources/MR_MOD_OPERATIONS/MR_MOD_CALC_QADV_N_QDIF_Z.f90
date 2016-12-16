#INCLUDE 'MR_H_ALIGN_PADDING.H'
!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE) MOD_CALC_QADV_N_QDIF_Z
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
  MODULE MR_MOD_CALC_QADV_N_QDIF_Z

    USE MR_KINDS

    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_CONSTS_N_REF_PARS , ONLY : DSIGMA
    USE MR_DEF_FIELD_VARS , ONLY : H
    USE MR_DEF_ACTIVITY
    USE MR_DEF_TIMING

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CALC_QADV_N_QDIF_Z_UV_W , MR_CALC_T_Z_UV_W
    PUBLIC :: MR_CALC_QADV_N_QDIF_Z_SS_W , MR_CALC_T_Z_SS_W

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_Z_UV_W( NI , NJ , NK , UV , RB , W , QADV_Z_UV_W , EKZ , VZW , QDIF_Z_UV_W , TUV0 , TUVN )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,1:2,1:NK) :: UV

    REAL   (PARD_KIND) , INTENT(IN ) :: RB , EKZ
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,    0:NK) :: W , VZW
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,1:2,0:NK) :: QADV_Z_UV_W , QDIF_Z_UV_W

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,1:2     ) , OPTIONAL :: TUV0 , TUVN

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM
    INTEGER(KKID_KIND) :: K

    K = 0
      DO DIM = 1 , 2
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
           !DIR$ FORCEINLINE
            CALL MR_CALC_QADV_N_QDIF_Z_UV_W_II_JJ_K0
          END DO
        END DO
      END DO
    !END K = 0

    K = 1
      DO DIM = 1 , 2
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
           !DIR$ FORCEINLINE
            CALL MR_CALC_QADV_N_QDIF_Z_UV_W_II_JJ_K1
          END DO
        END DO
      END DO
    !END K = 1

    DO K = 2 , NK-2
      DO DIM = 1 , 2
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
           !DIR$ FORCEINLINE
            CALL MR_CALC_QADV_N_QDIF_Z_UV_W_II_JJ_KK
          END DO
        END DO
      END DO
    END DO

    K = NK-1
      DO DIM = 1 , 2
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
           !DIR$ FORCEINLINE
            CALL MR_CALC_QADV_N_QDIF_Z_UV_W_II_JJ_KM
          END DO
        END DO
      END DO
    !END K = NK-1

    K = NK
      DO DIM = 1 , 2
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
           !DIR$ FORCEINLINE
            CALL MR_CALC_QADV_N_QDIF_Z_UV_W_II_JJ_KN
          END DO
        END DO
      END DO
    !END K = NK

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_Z_UV_W_II_JJ_K0

    IMPLICIT NONE

    !BLOCK
      !BLOCK
        QADV_Z_UV_W( I , J ,DIM, K ) = 0.0
      !END BLOCK
    !END BLOCK

    IF( ACTIVITY( I , J ) == NOACTIVE ) THEN
      !BLOCK
        QDIF_Z_UV_W( I , J ,DIM, K ) = 0.0
      !END BLOCK
    ELSE
      IF( PRESENT(TUV0) ) THEN
        QDIF_Z_UV_W( I , J ,DIM, K ) = DT * MW( I , J ) * TUV0( I , J ,DIM) / ( H( I , J ) * DSIGMA )
      ELSE
        QDIF_Z_UV_W( I , J ,DIM, K ) = 0.0
      END IF
    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_Z_UV_W_II_JJ_K0

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_Z_UV_W_II_JJ_K1

    IMPLICIT NONE

    REAL   (CARD_KIND)               :: CRW , DRW
    REAL   (CARD_KIND)               :: DDW , D2W

    IF( ACTIVITY( I , J ) == NOACTIVE ) THEN

      QADV_Z_UV_W( I , J ,DIM, K ) = 0.0
      QDIF_Z_UV_W( I , J ,DIM, K ) = 0.0

    ELSE

      CRW = DT * RB * W( I , J , K ) / DSIGMA
      DRW = DT * (1.0-PHI) * EKZ * VZW( I , J , K ) / ( H( I , J ) * DSIGMA )**2

    ! DDW
      !BLOCK
        !BLOCK
          DDW = ( UV( I , J ,DIM,K+1) - UV( I , J ,DIM, K ) )
        !END BLOCK
      !END BLOCK

    ! D2W
      IF( CRW >= 0.0 ) THEN
        IF( PRESENT(TUV0) ) THEN
          D2W = ( UV( I , J ,DIM,K+1) - UV( I , J ,DIM, K ) )   &
          &   - TUV0( I , J ,DIM) / ( EKZ * VZW( I , J ,K-1) ) * ( H( I , J ) * DSIGMA )
        ELSE
          D2W = ( UV( I , J ,DIM,K+1) - UV( I , J ,DIM, K ) )   !\\\
        END IF
      ELSE
        !BLOCK
          D2W = ( UV( I , J ,DIM,K+2) - UV( I , J ,DIM,K+1) )   &
          &   - ( UV( I , J ,DIM,K+1) - UV( I , J ,DIM, K ) )   !\\\
        !END BLOCK
      END IF

    ! CALCULATE ADVECTION
      QADV_Z_UV_W( I , J ,DIM, K ) = - CRW * MW( I , J ) *   &
      ( (     ( UV( I , J ,DIM,K+1) + UV( I , J ,DIM, K ) ) / 2.0   &
        -         CRW         * DDW                         / 2.0   &
        - ( 1.0 - CRW * CRW ) * D2W                         / 6.0   &
        )   &
      )
    ! CALCULATE DIFFUSION
      QDIF_Z_UV_W( I , J ,DIM, K ) = + DRW * MW( I , J ) *   &
                                DDW

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_Z_UV_W_II_JJ_K1

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_Z_UV_W_II_JJ_KK

    IMPLICIT NONE

    REAL   (CARD_KIND)               :: CRW , DRW
    REAL   (CARD_KIND)               :: DDW , D2W

    IF( ACTIVITY( I , J ) == NOACTIVE ) THEN

      QADV_Z_UV_W( I , J ,DIM, K ) = 0.0
      QDIF_Z_UV_W( I , J ,DIM, K ) = 0.0

    ELSE

      CRW = DT * RB * W( I , J , K ) / DSIGMA
      DRW = DT * (1.0-PHI) * EKZ * VZW( I , J , K ) / ( H( I , J ) * DSIGMA )**2

    ! DDW
      !BLOCK
        !BLOCK
          DDW = ( UV( I , J ,DIM,K+1) - UV( I , J ,DIM, K ) )   !\\\
        !END BLOCK
      !END BLOCK

    ! D2W
      IF( CRW >= 0.0 ) THEN
        !BLOCK
          D2W = ( UV( I , J ,DIM,K+1) - UV( I , J ,DIM, K ) )   &
          &   - ( UV( I , J ,DIM, K ) - UV( I , J ,DIM,K-1) )   !\\\
        !END BLOCK
      ELSE
        !BLOCK
          D2W = ( UV( I , J ,DIM,K+2) - UV( I , J ,DIM,K+1) )   &
          &   - ( UV( I , J ,DIM,K+1) - UV( I , J ,DIM, K ) )   !\\\
        !END BLOCK
      END IF

    ! CALCULATE ADVECTION
      QADV_Z_UV_W( I , J ,DIM, K ) = - CRW * MW( I , J ) *   &
      ( (     ( UV( I , J ,DIM,K+1) + UV( I , J ,DIM, K ) ) / 2.0   &
        -         CRW         * DDW                         / 2.0   &
        - ( 1.0 - CRW * CRW ) * D2W                         / 6.0   &
        )   &
      )
    ! CALCULATE DIFFUSION
      QDIF_Z_UV_W( I , J ,DIM, K ) = + DRW * MW( I , J ) *   &
                                DDW

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_Z_UV_W_II_JJ_KK

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_Z_UV_W_II_JJ_KM

    IMPLICIT NONE

    REAL   (CARD_KIND)               :: CRW , DRW
    REAL   (CARD_KIND)               :: DDW , D2W

    IF( ACTIVITY( I , J ) == NOACTIVE ) THEN

      QADV_Z_UV_W( I , J ,DIM, K ) = 0.0
      QDIF_Z_UV_W( I , J ,DIM, K ) = 0.0

    ELSE

      CRW = DT * RB * W( I , J , K ) / DSIGMA
      DRW = DT * (1.0-PHI) * EKZ * VZW( I , J , K ) / ( H( I , J ) * DSIGMA )**2

    ! DDW
      !BLOCK
        !BLOCK
          DDW = ( UV( I , J ,DIM,K+1) - UV( I , J ,DIM, K ) )
        !END BLOCK
      !END BLOCK

    ! D2W
      IF( CRW >= 0.0 ) THEN
        !BLOCK
          D2W = ( UV( I , J ,DIM,K+1) - UV( I , J ,DIM, K ) )   &
          &   - ( UV( I , J ,DIM, K ) - UV( I , J ,DIM,K-1) )   !\\\
        !END BLOCK
      ELSE
        IF( PRESENT(TUVN) ) THEN
          D2W = TUVN( I , J ,DIM) / ( EKZ * VZW( I , J ,K+1) ) * ( H( I , J ) * DSIGMA )   &
          &   - ( UV( I , J ,DIM,K+1) - UV( I , J ,DIM, K ) )   !\\\
        ELSE
          D2W =-( UV( I , J ,DIM,K+1) - UV( I , J ,DIM, K ) )   !\\\
        END IF
      END IF

    ! CALCULATE ADVECTION
      QADV_Z_UV_W( I , J ,DIM, K ) = - CRW * MW( I , J ) *   &
      ( (     ( UV( I , J ,DIM,K+1) + UV( I , J ,DIM, K ) ) / 2.0   &
        -         CRW         * DDW                         / 2.0   &
        - ( 1.0 - CRW * CRW ) * D2W                         / 6.0   &
        )   &
      )
    ! CALCULATE DIFFUSION
      QDIF_Z_UV_W( I , J ,DIM, K ) = + DRW * MW( I , J ) *   &
                                DDW

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_Z_UV_W_II_JJ_KM

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_Z_UV_W_II_JJ_KN

    IMPLICIT NONE

    !BLOCK
      !BLOCK
        QADV_Z_UV_W( I , J ,DIM, K ) = 0.0
      !END BLOCK
    !END BLOCK

    IF( ACTIVITY( I , J ) == NOACTIVE ) THEN
      !BLOCK
        QDIF_Z_UV_W( I , J ,DIM, K ) = 0.0
      !END BLOCK
    ELSE
      IF( PRESENT(TUVN) ) THEN
        QDIF_Z_UV_W( I , J ,DIM, K ) = DT * MW( I , J ) * TUVN( I , J ,DIM) / ( H( I , J ) * DSIGMA )
      ELSE
        QDIF_Z_UV_W( I , J ,DIM, K ) = 0.0
      END IF
    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_Z_UV_W_II_JJ_KN

  END SUBROUTINE MR_CALC_QADV_N_QDIF_Z_UV_W

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
  SUBROUTINE MR_CALC_T_Z_UV_W( NI , NJ , NK , UV , EKZ , VZW , T_Z_UV_W , TUV0 , TUVN )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,1:2,1:NK) :: UV

    REAL   (PARD_KIND) , INTENT(IN ) :: EKZ
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,    0:NK) :: VZW
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,1:2,0:NK) :: T_Z_UV_W

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,1:2     ) , OPTIONAL :: TUV0 , TUVN

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM
    INTEGER(KKID_KIND) :: K

    K = 0
      DO DIM = 1 , 2
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            IF( ACTIVITY( I , J ) == NOACTIVE ) THEN
              !BLOCK
                T_Z_UV_W( I , J ,DIM, K ) = 0.0
              !END BLOCK
            ELSE
              IF( PRESENT(TUV0) ) THEN
                T_Z_UV_W( I , J ,DIM, K ) = TUV0( I , J ,DIM)
              ELSE
                T_Z_UV_W( I , J ,DIM, K ) = 0.0
              END IF
            END IF
          END DO
        END DO
      END DO
    !END K = 0

    DO K = 1 , NK-1
      DO DIM = 1 , 2
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            IF( ACTIVITY( I , J ) == NOACTIVE ) THEN
              !BLOCK
                T_Z_UV_W( I , J ,DIM, K ) = 0.0
              !END BLOCK
            ELSE
              !BLOCK
                T_Z_UV_W( I , J ,DIM, K ) = EKZ * VZW( I , J , K ) / ( H( I , J ) * DSIGMA ) *   &
                & ( UV( I , J ,DIM,K+1) - UV( I , J ,DIM, K ) )
              !END BLOCK
            END IF
          END DO
        END DO
      END DO
    END DO

    K = NK
      DO DIM = 1 , 2
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            IF( ACTIVITY( I , J ) == NOACTIVE ) THEN
              !BLOCK
                T_Z_UV_W( I , J ,DIM, K ) = 0.0
              !END BLOCK
            ELSE
              IF( PRESENT(TUVN) ) THEN
                T_Z_UV_W( I , J ,DIM, K ) = TUVN( I , J ,DIM)
              ELSE
                T_Z_UV_W( I , J ,DIM, K ) = 0.0
              END IF
            END IF
          END DO
        END DO
      END DO
    !END K = NK

  END SUBROUTINE MR_CALC_T_Z_UV_W

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_Z_SS_W( NI , NJ , NK , SS , RB , W , QADV_Z_SS_W , SCZ , DZW , QDIF_Z_SS_W , TSS0 , TSSN )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,1:NK) :: SS

    REAL   (PARD_KIND) , INTENT(IN ) :: RB , SCZ
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,0:NK) :: W , DZW
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,0:NK) :: QADV_Z_SS_W , QDIF_Z_SS_W

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(FDRD_KIND),1:NJ     ) , OPTIONAL :: TSS0 , TSSN

    INTEGER(IJID_KIND) :: I , J
    INTEGER(KKID_KIND) :: K

    K = 0
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
         !DIR$ FORCEINLINE
          CALL MR_CALC_QADV_N_QDIF_Z_SS_W_II_JJ_K0
        END DO
      END DO
    !END K = 0

    K = 1
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
         !DIR$ FORCEINLINE
          CALL MR_CALC_QADV_N_QDIF_Z_SS_W_II_JJ_K1
        END DO
      END DO
    !END K = 1

    DO K = 2 , NK-2
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
         !DIR$ FORCEINLINE
          CALL MR_CALC_QADV_N_QDIF_Z_SS_W_II_JJ_KK
        END DO
      END DO
    END DO

    K = NK-1
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
         !DIR$ FORCEINLINE
          CALL MR_CALC_QADV_N_QDIF_Z_SS_W_II_JJ_KM
        END DO
      END DO
    !END K = NK-1

    K = NK
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
         !DIR$ FORCEINLINE
          CALL MR_CALC_QADV_N_QDIF_Z_SS_W_II_JJ_KN
        END DO
      END DO
    !END K = NK

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_Z_SS_W_II_JJ_K0

    IMPLICIT NONE

    !BLOCK
      !BLOCK
        QADV_Z_SS_W( I , J , K ) = 0.0
      !END BLOCK
    !END BLOCK

    IF( ACTIVITY( I , J ) == NOACTIVE ) THEN
      !BLOCK
        QDIF_Z_SS_W( I , J , K ) = 0.0
      !END BLOCK
    ELSE
      IF( PRESENT(TSS0) ) THEN
        QDIF_Z_SS_W( I , J , K ) = DT * MW( I , J ) * TSS0( I , J ) / ( H( I , J ) * DSIGMA )
      ELSE
        QDIF_Z_SS_W( I , J , K ) = 0.0
      END IF
    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_Z_SS_W_II_JJ_K0

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_Z_SS_W_II_JJ_K1

    IMPLICIT NONE

    REAL   (CARD_KIND)               :: CRW , DRW
    REAL   (CARD_KIND)               :: DDW , D2W

    IF( ACTIVITY( I , J ) == NOACTIVE ) THEN

      QADV_Z_SS_W( I , J , K ) = 0.0
      QDIF_Z_SS_W( I , J , K ) = 0.0

    ELSE

      CRW = DT * RB * W( I , J , K ) / DSIGMA
      DRW = DT * (1.0-PHI) * SCZ * DZW( I , J , K ) / ( H( I , J ) * DSIGMA )**2

    ! DDW
      !BLOCK
        !BLOCK
          DDW = ( SS( I , J ,K+1) - SS( I , J , K ) )
        !END BLOCK
      !END BLOCK

    ! D2W
      IF( CRW >= 0.0 ) THEN
        IF( PRESENT(TSS0) ) THEN
          D2W = ( SS( I , J ,K+1) - SS( I , J , K ) )   &
          &   - TSS0( I , J ) / ( SCZ * DZW( I , J ,K-1) ) * ( H( I , J ) * DSIGMA )
        ELSE
          D2W = ( SS( I , J ,K+1) - SS( I , J , K ) )   !\\\
        END IF
      ELSE
        !BLOCK
          D2W = ( SS( I , J ,K+2) - SS( I , J ,K+1) )   &
          &   - ( SS( I , J ,K+1) - SS( I , J , K ) )   !\\\
        !END BLOCK
      END IF

    ! CALCULATE ADVECTION
      QADV_Z_SS_W( I , J , K ) = - CRW * MW( I , J ) *   &
      ( (     ( SS( I , J ,K+1) + SS( I , J , K ) ) / 2.0   &
        -         CRW         * DDW                 / 2.0   &
        - ( 1.0 - CRW * CRW ) * D2W                 / 6.0   &
        )   &
      )
    ! CALCULATE DIFFUSION
      QDIF_Z_SS_W( I , J , K ) = + DRW * MW( I , J ) *   &
                                DDW

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_Z_SS_W_II_JJ_K1

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_Z_SS_W_II_JJ_KK

    IMPLICIT NONE

    REAL   (CARD_KIND)               :: CRW , DRW
    REAL   (CARD_KIND)               :: DDW , D2W

    IF( ACTIVITY( I , J ) == NOACTIVE ) THEN

      QADV_Z_SS_W( I , J , K ) = 0.0
      QDIF_Z_SS_W( I , J , K ) = 0.0

    ELSE

      CRW = DT * RB * W( I , J , K ) / DSIGMA
      DRW = DT * (1.0-PHI) * SCZ * DZW( I , J , K ) / ( H( I , J ) * DSIGMA )**2

    ! DDW
      !BLOCK
        !BLOCK
          DDW = ( SS( I , J ,K+1) - SS( I , J , K ) )   !\\\
        !END BLOCK
      !END BLOCK

    ! D2W
      IF( CRW >= 0.0 ) THEN
        !BLOCK
          D2W = ( SS( I , J ,K+1) - SS( I , J , K ) )   &
          &   - ( SS( I , J , K ) - SS( I , J ,K-1) )   !\\\
        !END BLOCK
      ELSE
        !BLOCK
          D2W = ( SS( I , J ,K+2) - SS( I , J ,K+1) )   &
          &   - ( SS( I , J ,K+1) - SS( I , J , K ) )   !\\\
        !END BLOCK
      END IF

    ! CALCULATE ADVECTION
      QADV_Z_SS_W( I , J , K ) = - CRW * MW( I , J ) *   &
      ( (     ( SS( I , J ,K+1) + SS( I , J , K ) ) / 2.0   &
        -         CRW         * DDW                 / 2.0   &
        - ( 1.0 - CRW * CRW ) * D2W                 / 6.0   &
        )   &
      )
    ! CALCULATE DIFFUSION
      QDIF_Z_SS_W( I , J , K ) = + DRW * MW( I , J ) *   &
                                DDW

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_Z_SS_W_II_JJ_KK

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_Z_SS_W_II_JJ_KM

    IMPLICIT NONE

    REAL   (CARD_KIND)               :: CRW , DRW
    REAL   (CARD_KIND)               :: DDW , D2W

    IF( ACTIVITY( I , J ) == NOACTIVE ) THEN

      QADV_Z_SS_W( I , J , K ) = 0.0
      QDIF_Z_SS_W( I , J , K ) = 0.0

    ELSE

      CRW = DT * RB * W( I , J , K ) / DSIGMA
      DRW = DT * (1.0-PHI) * SCZ * DZW( I , J , K ) / ( H( I , J ) * DSIGMA )**2

    ! DDW
      !BLOCK
        !BLOCK
          DDW = ( SS( I , J ,K+1) - SS( I , J , K ) )
        !END BLOCK
      !END BLOCK

    ! D2W
      IF( CRW >= 0.0 ) THEN
        !BLOCK
          D2W = ( SS( I , J ,K+1) - SS( I , J , K ) )   &
          &   - ( SS( I , J , K ) - SS( I , J ,K-1) )   !\\\
        !END BLOCK
      ELSE
        IF( PRESENT(TSSN) ) THEN
          D2W = TSSN( I , J ) / ( SCZ * DZW( I , J ,K+1) ) * ( H( I , J ) * DSIGMA )   &
          &   - ( SS( I , J ,K+1) - SS( I , J , K ) )   !\\\
        ELSE
          D2W =-( SS( I , J ,K+1) - SS( I , J , K ) )   !\\\
        END IF
      END IF

    ! CALCULATE ADVECTION
      QADV_Z_SS_W( I , J , K ) = - CRW * MW( I , J ) *   &
      ( (     ( SS( I , J ,K+1) + SS( I , J , K ) ) / 2.0   &
        -         CRW         * DDW                 / 2.0   &
        - ( 1.0 - CRW * CRW ) * D2W                 / 6.0   &
        )   &
      )
    ! CALCULATE DIFFUSION
      QDIF_Z_SS_W( I , J , K ) = + DRW * MW( I , J ) *   &
                                DDW

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_Z_SS_W_II_JJ_KM

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_Z_SS_W_II_JJ_KN

    IMPLICIT NONE

    !BLOCK
      !BLOCK
        QADV_Z_SS_W( I , J , K ) = 0.0
      !END BLOCK
    !END BLOCK

    IF( ACTIVITY( I , J ) == NOACTIVE ) THEN
      !BLOCK
        QDIF_Z_SS_W( I , J , K ) = 0.0
      !END BLOCK
    ELSE
      IF( PRESENT(TSSN) ) THEN
        QDIF_Z_SS_W( I , J , K ) = DT * MW( I , J ) * TSSN( I , J ) / ( H( I , J ) * DSIGMA )
      ELSE
        QDIF_Z_SS_W( I , J , K ) = 0.0
      END IF
    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_Z_SS_W_II_JJ_KN

  END SUBROUTINE MR_CALC_QADV_N_QDIF_Z_SS_W

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
  SUBROUTINE MR_CALC_T_Z_SS_W( NI , NJ , NK , SS , SCZ , DZW , T_Z_SS_W , TSS0 , TSSN )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,1:NK) :: SS

    REAL   (PARD_KIND) , INTENT(IN ) :: SCZ
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,0:NK) :: DZW
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,0:NK) :: T_Z_SS_W

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(FDRD_KIND),1:NJ     ) , OPTIONAL :: TSS0 , TSSN

    INTEGER(IJID_KIND) :: I , J
    INTEGER(KKID_KIND) :: K

    K = 0
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          IF( ACTIVITY( I , J ) == NOACTIVE ) THEN
            !BLOCK
              T_Z_SS_W( I , J , K ) = 0.0
            !END BLOCK
          ELSE
            IF( PRESENT(TSS0) ) THEN
              T_Z_SS_W( I , J , K ) = TSS0( I , J )
            ELSE
              T_Z_SS_W( I , J , K ) = 0.0
            END IF
          END IF
        END DO
      END DO
    !END K = 0

    DO K = 1 , NK-1
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          IF( ACTIVITY( I , J ) == NOACTIVE ) THEN
            !BLOCK
              T_Z_SS_W( I , J , K ) = 0.0
            !END BLOCK
          ELSE
            !BLOCK
              T_Z_SS_W( I , J , K ) = SCZ * DZW( I , J , K ) / ( H( I , J ) * DSIGMA ) *   &
              & ( SS( I , J ,K+1) - SS( I , J , K ) )
            !END BLOCK
          END IF
        END DO
      END DO
    END DO

    K = NK
      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          IF( ACTIVITY( I , J ) == NOACTIVE ) THEN
            !BLOCK
              T_Z_SS_W( I , J , K ) = 0.0
            !END BLOCK
          ELSE
            IF( PRESENT(TSSN) ) THEN
              T_Z_SS_W( I , J , K ) = TSSN( I , J )
            ELSE
              T_Z_SS_W( I , J , K ) = 0.0
            END IF
          END IF
        END DO
      END DO
    !END K = NK

  END SUBROUTINE MR_CALC_T_Z_SS_W

  END MODULE MR_MOD_CALC_QADV_N_QDIF_Z