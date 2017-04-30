#INCLUDE 'MR_H_ALIGN_PADDING.H'
!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE) MOD_CALC_QADV_N_QDIF_XY
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
  MODULE MR_MOD_CALC_QADV_N_QDIF_XY

    USE MR_KINDS

    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_ACTIVITY
    USE MR_DEF_TIMING

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CALC_QADV_N_QDIF_XY_UV_U
    PUBLIC :: MR_CALC_QADV_N_QDIF_XY_SS_U

    PUBLIC :: MR_CALC_QADV_N_QDIF_XY_UV_V
    PUBLIC :: MR_CALC_QADV_N_QDIF_XY_SS_V

!***********************************************************************************************************************************

  CONTAINS

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE) CALC_QADV_N_QDIF_XY_UV_U
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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_U( NI , NJ , UV , VV , RB , U , QADV_XY_UV_U , EKXY , VXYU , QDIF_XY_UV_U )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: UV
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ,1:2) :: VV

    REAL   (PARD_KIND) , INTENT(IN ) :: RB , EKXY
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ    ) :: U , VXYU
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ,1:2) :: QADV_XY_UV_U , QDIF_XY_UV_U

    REAL   (FDRD_KIND) , PARAMETER   , DIMENSION(                      1:2) :: FACTOR = (/+1.0,-1.0/)

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    DO DIM = 1 , 2

      DO J = 1 , NJ

        I = 0
         !DIR$ FORCEINLINE
          CALL MR_CALC_QADV_N_QDIF_XY_UV_U_I0_JJ
        !END I = 0

        I = 1
         !DIR$ FORCEINLINE
          CALL MR_CALC_QADV_N_QDIF_XY_UV_U_I1_JJ
        !END I = 1

       !DIR$ VECTOR ALIGNED
        DO I = 2 , NI-2
         !DIR$ FORCEINLINE
          CALL MR_CALC_QADV_N_QDIF_XY_UV_U_II_JJ
        END DO

        I = NI-1
         !DIR$ FORCEINLINE
          CALL MR_CALC_QADV_N_QDIF_XY_UV_U_IM_JJ
        !END I = NI-1

        I = NI
         !DIR$ FORCEINLINE
          CALL MR_CALC_QADV_N_QDIF_XY_UV_U_IN_JJ
        !END I = NI

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
!   2015-06-10    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_U_I0_JJ

    IMPLICIT NONE

    REAL   (CARD_KIND)               :: CRU , DRU
    REAL   (CARD_KIND)               :: DDU , D2U , DCU

    INTEGER(IJID_KIND)               :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

  ! AT IMPERMEABLE INTERNAL INTERFACE
    IF( ACTIVITY(I+1, J ) == NOACTIVE .OR. ACTIVITY( P , Q ) == NOACTIVE ) THEN

      QADV_XY_UV_U( I , J ,DIM) = 0.0
      QDIF_XY_UV_U( I , J ,DIM) = 0.0

  ! AT FREE INTERNAL INTERFACE
    ELSE

      CRU = DT * RB * U( I , J )
      DRU = DT * EKXY * VXYU( I , J )

    ! DDU
      !BLOCK
        !BLOCK
          DDU = ( UV(I+1, J ,DIM)               - UV( P , Q ,DIM) * FACTOR(DIM) )
        !END BLOCK
      !END BLOCK

    ! D2U
      IF( CRU >= 0.0 ) THEN
        IF( ACTIVITY(P-1, Q ) == NOACTIVE ) THEN
          D2U = ( UV(I+1, J ,DIM)               - UV( P , Q ,DIM) * FACTOR(DIM) )   !
        ELSE
          D2U = ( UV(I+1, J ,DIM)               - UV( P , Q ,DIM) * FACTOR(DIM) )   &
          &   - ( UV( P , Q ,DIM) * FACTOR(DIM) - UV(P-1, Q ,DIM) * FACTOR(DIM) )   !\\\
        END IF
      ELSE
        IF( ACTIVITY(I+2, J ) == NOACTIVE ) THEN
          D2U = ( UV( P , Q ,DIM) * FACTOR(DIM) - UV(I+1, J ,DIM)               )   !
        ELSE
          D2U = ( UV( P , Q ,DIM) * FACTOR(DIM) - UV(I+1, J ,DIM)               )   &
          &   - ( UV(I+1, J ,DIM)               - UV(I+2, J ,DIM)               )   !\\\
        END IF
      END IF

    ! DCU
      IF( CRU >= -1.0 .AND. CRU <= +1.0 ) THEN
        !BLOCK
          DCU = ( VV(I+1, J ,DIM)               - VV(I+1,J-1,DIM)               ) * ( 1.0 - CRU ) / 2.0   &
          &   + ( VV( P ,Q-1,DIM) * FACTOR(DIM) - VV( P , Q ,DIM) * FACTOR(DIM) ) * ( 1.0 + CRU ) / 2.0   !\\\
        !END BLOCK
      ELSE IF( CRU > +1.0 ) THEN
        IF( ACTIVITY(P-1, Q ) == NOACTIVE ) THEN
          DCU = ( VV( P ,Q-1,DIM) * FACTOR(DIM) - VV( P , Q ,DIM) * FACTOR(DIM) )                         !
        ELSE
          DCU = ( VV( P ,Q-1,DIM) * FACTOR(DIM) - VV( P , Q ,DIM) * FACTOR(DIM) ) * ( 3.0 - CRU ) / 2.0   &
          &   - ( VV(P-1,Q-1,DIM) * FACTOR(DIM) - VV(P-1, Q ,DIM) * FACTOR(DIM) ) * ( 1.0 - CRU ) / 2.0   !\\\
        END IF
      ELSE
        IF( ACTIVITY(I+2, J ) == NOACTIVE ) THEN
          DCU = ( VV(I+1, J ,DIM)               - VV(I+1,J-1,DIM)               )                         !
        ELSE
          DCU = ( VV(I+1, J ,DIM)               - VV(I+1,J-1,DIM)               ) * ( 3.0 + CRU ) / 2.0   &
          &   - ( VV(I+2, J ,DIM)               - VV(I+2,J-1,DIM)               ) * ( 1.0 + CRU ) / 2.0   !\\\
        END IF
      END IF

    ! CALCULATE ADVECTION
      QADV_XY_UV_U( I , J ,DIM) = - CRU * MU( I , J ) *   &
      ( (       ( UV(I+1, J ,DIM)               + UV( P , Q ,DIM) * FACTOR(DIM) ) / 2.0   &
        -         CRU         * DDU                                               / 2.0   &
        - ( 1.0 - CRU * CRU ) * D2U                                               / 6.0   &
        )   &
      )
    ! CALCULATE DIFFUSION
      QDIF_XY_UV_U( I , J ,DIM) = + DRU * MU( I , J ) *   &
      ( FUU( I , J ,1,1) *   &
        (                       DDU                                                       &
        -         CRU         * D2U                                               / 2.0   &
        )   &
      + FUU( I , J ,1,2) *   &
                                DCU                                                       &
      )

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_U_I0_JJ

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_U_I1_JJ

    IMPLICIT NONE

    REAL   (CARD_KIND)               :: CRU , DRU
    REAL   (CARD_KIND)               :: DDU , D2U , DCU

    INTEGER(IJID_KIND)               :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

  ! AT IMPERMEABLE INTERNAL INTERFACE
    IF( ACTIVITY(I+1, J ) == NOACTIVE .OR. ACTIVITY( I , J ) == NOACTIVE ) THEN

      QADV_XY_UV_U( I , J ,DIM) = 0.0
      QDIF_XY_UV_U( I , J ,DIM) = 0.0

  ! AT FREE INTERNAL INTERFACE
    ELSE

      CRU = DT * RB * U( I , J )
      DRU = DT * EKXY * VXYU( I , J )

    ! DDU
      !BLOCK
        !BLOCK
          DDU = ( UV(I+1, J ,DIM) - UV( I , J ,DIM) )
        !END BLOCK
      !END BLOCK

    ! D2U
      IF( CRU >= 0.0 ) THEN
        IF( ACTIVITY(P-1, Q ) == NOACTIVE ) THEN
          D2U = ( UV(I+1, J ,DIM) - UV( I , J ,DIM) )   !
        ELSE
          D2U = ( UV(I+1, J ,DIM) - UV( I , J ,DIM) )   &
          &   - ( UV( I , J ,DIM) - UV(P-1, Q ,DIM) * FACTOR(DIM) )   !!!!!!
        END IF
      ELSE
        IF( ACTIVITY(I+2, J ) == NOACTIVE ) THEN
          D2U = ( UV( I , J ,DIM) - UV(I+1, J ,DIM) )   !
        ELSE
          D2U = ( UV( I , J ,DIM) - UV(I+1, J ,DIM) )   &
          &   - ( UV(I+1, J ,DIM) - UV(I+2, J ,DIM) )   !\\\
        END IF
      END IF

    ! DCU
      IF( CRU >= -1.0 .AND. CRU <= +1.0 ) THEN
        !BLOCK
          DCU = ( VV(I+1, J ,DIM) - VV(I+1,J-1,DIM) ) * ( 1.0 - CRU ) / 2.0   &
          &   + ( VV( I , J ,DIM) - VV( I ,J-1,DIM) ) * ( 1.0 + CRU ) / 2.0   !\\\
        !END BLOCK
      ELSE IF( CRU > +1.0 ) THEN
        IF( ACTIVITY(P-1, Q ) == NOACTIVE ) THEN
          DCU = ( VV( I , J ,DIM) - VV( I ,J-1,DIM) )                         !
        ELSE
          DCU = ( VV( I , J ,DIM) - VV( I ,J-1,DIM) ) * ( 3.0 - CRU ) / 2.0   &
          &   - ( VV(P-1,Q-1,DIM) * FACTOR(DIM) - VV(P-1, Q ,DIM) * FACTOR(DIM) ) * ( 1.0 - CRU ) / 2.0   !!!!!!
        END IF
      ELSE
        IF( ACTIVITY(I+2, J ) == NOACTIVE ) THEN
          DCU = ( VV(I+1, J ,DIM) - VV(I+1,J-1,DIM) )                         !
        ELSE
          DCU = ( VV(I+1, J ,DIM) - VV(I+1,J-1,DIM) ) * ( 3.0 + CRU ) / 2.0   &
          &   - ( VV(I+2, J ,DIM) - VV(I+2,J-1,DIM) ) * ( 1.0 + CRU ) / 2.0   !\\\
        END IF
      END IF

    ! CALCULATE ADVECTION
      QADV_XY_UV_U( I , J ,DIM) = - CRU * MU( I , J ) *   &
      ( (       ( UV(I+1, J ,DIM) + UV( I , J ,DIM) ) / 2.0   &
        -         CRU         * DDU                   / 2.0   &
        - ( 1.0 - CRU * CRU ) * D2U                   / 6.0   &
        )   &
      )
    ! CALCULATE DIFFUSION
      QDIF_XY_UV_U( I , J ,DIM) = + DRU * MU( I , J ) *   &
      ( FUU( I , J ,1,1) *   &
        (                       DDU                           &
        -         CRU         * D2U                   / 2.0   &
        )   &
      + FUU( I , J ,1,2) *   &
                                DCU                           &
      )

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_U_I1_JJ

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_U_II_JJ

    IMPLICIT NONE

    REAL   (CARD_KIND)               :: CRU , DRU
    REAL   (CARD_KIND)               :: DDU , D2U , DCU

  ! AT IMPERMEABLE INTERNAL INTERFACE
    IF( ACTIVITY(I+1, J ) == NOACTIVE .OR. ACTIVITY( I , J ) == NOACTIVE ) THEN

      QADV_XY_UV_U( I , J ,DIM) = 0.0
      QDIF_XY_UV_U( I , J ,DIM) = 0.0

  ! AT FREE INTERNAL INTERFACE
    ELSE

      CRU = DT * RB * U( I , J )
      DRU = DT * EKXY * VXYU( I , J )

    ! DDU
      !BLOCK
        !BLOCK
          DDU = ( UV(I+1, J ,DIM) - UV( I , J ,DIM) )
        !END BLOCK
      !END BLOCK

    ! D2U
      IF( CRU >= 0.0 ) THEN
        IF( ACTIVITY(I-1, J ) == NOACTIVE ) THEN
          D2U = ( UV(I+1, J ,DIM) - UV( I , J ,DIM) )   !
        ELSE
          D2U = ( UV(I+1, J ,DIM) - UV( I , J ,DIM) )   &
          &   - ( UV( I , J ,DIM) - UV(I-1, J ,DIM) )   !\\\
        END IF
      ELSE
        IF( ACTIVITY(I+2, J ) == NOACTIVE ) THEN
          D2U = ( UV( I , J ,DIM) - UV(I+1, J ,DIM) )   !
        ELSE
          D2U = ( UV( I , J ,DIM) - UV(I+1, J ,DIM) )   &
          &   - ( UV(I+1, J ,DIM) - UV(I+2, J ,DIM) )   !\\\
        END IF
      END IF

    ! DCU
      IF( CRU >= -1.0 .AND. CRU <= +1.0 ) THEN
        !BLOCK
          DCU = ( VV(I+1, J ,DIM) - VV(I+1,J-1,DIM) ) * ( 1.0 - CRU ) / 2.0   &
          &   + ( VV( I , J ,DIM) - VV( I ,J-1,DIM) ) * ( 1.0 + CRU ) / 2.0   !\\\
        !END BLOCK
      ELSE IF( CRU > +1.0 ) THEN
        IF( ACTIVITY(I-1, J ) == NOACTIVE ) THEN
          DCU = ( VV( I , J ,DIM) - VV( I ,J-1,DIM) )                         !
        ELSE
          DCU = ( VV( I , J ,DIM) - VV( I ,J-1,DIM) ) * ( 3.0 - CRU ) / 2.0   &
          &   - ( VV(I-1, J ,DIM) - VV(I-1,J-1,DIM) ) * ( 1.0 - CRU ) / 2.0   !\\\
        END IF
      ELSE
        IF( ACTIVITY(I+2, J ) == NOACTIVE ) THEN
          DCU = ( VV(I+1, J ,DIM) - VV(I+1,J-1,DIM) )                         !
        ELSE
          DCU = ( VV(I+1, J ,DIM) - VV(I+1,J-1,DIM) ) * ( 3.0 + CRU ) / 2.0   &
          &   - ( VV(I+2, J ,DIM) - VV(I+2,J-1,DIM) ) * ( 1.0 + CRU ) / 2.0   !\\\
        END IF
      END IF

    ! CALCULATE ADVECTION
      QADV_XY_UV_U( I , J ,DIM) = - CRU * MU( I , J ) *   &
      ( (       ( UV(I+1, J ,DIM) + UV( I , J ,DIM) ) / 2.0   &
        -         CRU         * DDU                   / 2.0   &
        - ( 1.0 - CRU * CRU ) * D2U                   / 6.0   &
        )   &
      )
    ! CALCULATE DIFFUSION
      QDIF_XY_UV_U( I , J ,DIM) = + DRU * MU( I , J ) *   &
      ( FUU( I , J ,1,1) *   &
        (                       DDU                           &
        -         CRU         * D2U                   / 2.0   &
        )   &
      + FUU( I , J ,1,2) *   &
                                DCU                           &
      )

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_U_II_JJ

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_U_IM_JJ

    IMPLICIT NONE

    REAL   (CARD_KIND)               :: CRU , DRU
    REAL   (CARD_KIND)               :: DDU , D2U , DCU

    INTEGER(IJID_KIND)               :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

  ! AT IMPERMEABLE INTERNAL INTERFACE
    IF( ACTIVITY(I+1, J ) == NOACTIVE .OR. ACTIVITY( I , J ) == NOACTIVE ) THEN

      QADV_XY_UV_U( I , J ,DIM) = 0.0
      QDIF_XY_UV_U( I , J ,DIM) = 0.0

  ! AT FREE INTERNAL INTERFACE
    ELSE

      CRU = DT * RB * U( I , J )
      DRU = DT * EKXY * VXYU( I , J )

    ! DDU
      !BLOCK
        !BLOCK
          DDU = ( UV(I+1, J ,DIM) - UV( I , J ,DIM) )
        !END BLOCK
      !END BLOCK

    ! D2U
      IF( CRU >= 0.0 ) THEN
        IF( ACTIVITY(I-1, J ) == NOACTIVE ) THEN
          D2U = ( UV(I+1, J ,DIM) - UV( I , J ,DIM) )   !
        ELSE
          D2U = ( UV(I+1, J ,DIM) - UV( I , J ,DIM) )   &
          &   - ( UV( I , J ,DIM) - UV(I-1, J ,DIM) )   !\\\
        END IF
      ELSE
        IF( ACTIVITY(P+2, Q ) == NOACTIVE ) THEN
          D2U = ( UV( I , J ,DIM) - UV(I+1, J ,DIM) )   !
        ELSE
          D2U = ( UV( I , J ,DIM) - UV(I+1, J ,DIM) )   &
          &   - ( UV(I+1, J ,DIM) - UV(P+2, Q ,DIM) * FACTOR(DIM) )   !!!!!!
        END IF
      END IF

    ! DCU
      IF( CRU >= -1.0 .AND. CRU <= +1.0 ) THEN
        !BLOCK
          DCU = ( VV(I+1, J ,DIM) - VV(I+1,J-1,DIM) ) * ( 1.0 - CRU ) / 2.0   &
          &   + ( VV( I , J ,DIM) - VV( I ,J-1,DIM) ) * ( 1.0 + CRU ) / 2.0   !\\\
        !END BLOCK
      ELSE IF( CRU > +1.0 ) THEN
        IF( ACTIVITY(I-1, J ) == NOACTIVE ) THEN
          DCU = ( VV( I , J ,DIM) - VV( I ,J-1,DIM) )                         !
        ELSE
          DCU = ( VV( I , J ,DIM) - VV( I ,J-1,DIM) ) * ( 3.0 - CRU ) / 2.0   &
          &   - ( VV(I-1, J ,DIM) - VV(I-1,J-1,DIM) ) * ( 1.0 - CRU ) / 2.0   !\\\
        END IF
      ELSE
        IF( ACTIVITY(P+2, Q ) == NOACTIVE ) THEN
          DCU = ( VV(I+1, J ,DIM) - VV(I+1,J-1,DIM) )                         !
        ELSE
          DCU = ( VV(I+1, J ,DIM) - VV(I+1,J-1,DIM) ) * ( 3.0 + CRU ) / 2.0   &
          &   - ( VV(P+2,Q-1,DIM) * FACTOR(DIM) - VV(P+2, Q ,DIM) * FACTOR(DIM) ) * ( 1.0 + CRU ) / 2.0   !!!!!!
        END IF
      END IF

    ! CALCULATE ADVECTION
      QADV_XY_UV_U( I , J ,DIM) = - CRU * MU( I , J ) *   &
      ( (       ( UV(I+1, J ,DIM) + UV( I , J ,DIM) ) / 2.0   &
        -         CRU         * DDU                   / 2.0   &
        - ( 1.0 - CRU * CRU ) * D2U                   / 6.0   &
        )   &
      )
    ! CALCULATE DIFFUSION
      QDIF_XY_UV_U( I , J ,DIM) = + DRU * MU( I , J ) *   &
      ( FUU( I , J ,1,1) *   &
        (                       DDU                           &
        -         CRU         * D2U                   / 2.0   &
        )   &
      + FUU( I , J ,1,2) *   &
                                DCU                           &
      )

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_U_IM_JJ

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_U_IN_JJ

    IMPLICIT NONE

    REAL   (CARD_KIND)               :: CRU , DRU
    REAL   (CARD_KIND)               :: DDU , D2U , DCU

    INTEGER(IJID_KIND)               :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

  ! AT IMPERMEABLE INTERNAL INTERFACE
    IF( ACTIVITY(P+1, Q ) == NOACTIVE .OR. ACTIVITY( I , J ) == NOACTIVE ) THEN

      QADV_XY_UV_U( I , J ,DIM) = 0.0
      QDIF_XY_UV_U( I , J ,DIM) = 0.0

  ! AT FREE INTERNAL INTERFACE
    ELSE

      CRU = DT * RB * U( I , J )
      DRU = DT * EKXY * VXYU( I , J )

    ! DDU
      !BLOCK
        !BLOCK
          DDU = ( UV(P+1, Q ,DIM) * FACTOR(DIM) - UV( I , J ,DIM)               )
        !END BLOCK
      !END BLOCK

    ! D2U
      IF( CRU >= 0.0 ) THEN
        IF( ACTIVITY(I-1, J ) == NOACTIVE ) THEN
          D2U = ( UV(P+1, Q ,DIM) * FACTOR(DIM) - UV( I , J ,DIM)               )   !
        ELSE
          D2U = ( UV(P+1, Q ,DIM) * FACTOR(DIM) - UV( I , J ,DIM)               )   &
          &   - ( UV( I , J ,DIM)               - UV(I-1, J ,DIM)               )   !\\\
        END IF
      ELSE
        IF( ACTIVITY(P+2, Q ) == NOACTIVE ) THEN
          D2U = ( UV( I , J ,DIM)               - UV(P+1, Q ,DIM) * FACTOR(DIM) )   !
        ELSE
          D2U = ( UV( I , J ,DIM)               - UV(P+1, Q ,DIM) * FACTOR(DIM) )   &
          &   - ( UV(P+1, Q ,DIM) * FACTOR(DIM) - UV(P+2, Q ,DIM) * FACTOR(DIM) )   !\\\
        END IF
      END IF

    ! DCU
      IF( CRU >= -1.0 .AND. CRU <= +1.0 ) THEN
        !BLOCK
          DCU = ( VV(P+1,Q-1,DIM) * FACTOR(DIM) - VV(P+1, Q ,DIM) * FACTOR(DIM) ) * ( 1.0 - CRU ) / 2.0   &
          &   + ( VV( I , J ,DIM)               - VV( I ,J-1,DIM)               ) * ( 1.0 + CRU ) / 2.0   !\\\
        !END BLOCK
      ELSE IF( CRU > +1.0 ) THEN
        IF( ACTIVITY(I-1, J ) == NOACTIVE ) THEN
          DCU = ( VV( I , J ,DIM)               - VV( I ,J-1,DIM)               )                         !
        ELSE
          DCU = ( VV( I , J ,DIM)               - VV( I ,J-1,DIM)               ) * ( 3.0 - CRU ) / 2.0   &
          &   - ( VV(I-1, J ,DIM)               - VV(I-1,J-1,DIM)               ) * ( 1.0 - CRU ) / 2.0   !\\\
        END IF
      ELSE
        IF( ACTIVITY(P+2, Q ) == NOACTIVE ) THEN
          DCU = ( VV(P+1,Q-1,DIM) * FACTOR(DIM) - VV(P+1, Q ,DIM) * FACTOR(DIM) )                         !
        ELSE
          DCU = ( VV(P+1,Q-1,DIM) * FACTOR(DIM) - VV(P+1, Q ,DIM) * FACTOR(DIM) ) * ( 3.0 + CRU ) / 2.0   &
          &   - ( VV(P+2,Q-1,DIM) * FACTOR(DIM) - VV(P+2, Q ,DIM) * FACTOR(DIM) ) * ( 1.0 + CRU ) / 2.0   !\\\
        END IF
      END IF

    ! CALCULATE ADVECTION
      QADV_XY_UV_U( I , J ,DIM) = - CRU * MU( I , J ) *   &
      ( (       ( UV(P+1, Q ,DIM) * FACTOR(DIM) + UV( I , J ,DIM)               ) / 2.0   &
        -         CRU         * DDU                                               / 2.0   &
        - ( 1.0 - CRU * CRU ) * D2U                                               / 6.0   &
        )   &
      )
    ! CALCULATE DIFFUSION
      QDIF_XY_UV_U( I , J ,DIM) = + DRU * MU( I , J ) *   &
      ( FUU( I , J ,1,1) *   &
        (                       DDU                                                       &
        -         CRU         * D2U                                               / 2.0   &
        )   &
      + FUU( I , J ,1,2) *   &
                                DCU                                                       &
      )

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_U_IN_JJ

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_U

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE) CALC_QADV_N_QDIF_XY_SS_U
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
!   2015-06-04    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_U( NI , NJ , SS , SV , RB , U , QADV_XY_SS_U , SCXY , DXYU , QDIF_XY_SS_U )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) :: SS
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ) :: SV

    REAL   (PARD_KIND) , INTENT(IN ) :: RB , SCXY
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ) :: U , DXYU
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ) :: QADV_XY_SS_U , QDIF_XY_SS_U

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ

      I = 0
       !DIR$ FORCEINLINE
        CALL MR_CALC_QADV_N_QDIF_XY_SS_U_I0_JJ
      !END I = 0

      I = 1
       !DIR$ FORCEINLINE
        CALL MR_CALC_QADV_N_QDIF_XY_SS_U_I1_JJ
      !END I = 1

     !DIR$ VECTOR ALIGNED
      DO I = 2 , NI-2
       !DIR$ FORCEINLINE
        CALL MR_CALC_QADV_N_QDIF_XY_SS_U_II_JJ
      END DO

      I = NI-1
       !DIR$ FORCEINLINE
        CALL MR_CALC_QADV_N_QDIF_XY_SS_U_IM_JJ
      !END I = NI-1

      I = NI
       !DIR$ FORCEINLINE
        CALL MR_CALC_QADV_N_QDIF_XY_SS_U_IN_JJ
      !END I = NI

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
!   2015-06-04    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_U_I0_JJ

    IMPLICIT NONE

    REAL   (CARD_KIND)               :: CRU , DRU
    REAL   (CARD_KIND)               :: DDU , D2U , DCU

    INTEGER(IJID_KIND)               :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

  ! AT IMPERMEABLE INTERNAL INTERFACE
    IF( ACTIVITY(I+1, J ) == NOACTIVE .OR. ACTIVITY( P , Q ) == NOACTIVE ) THEN

      QADV_XY_SS_U( I , J ) = 0.0
      QDIF_XY_SS_U( I , J ) = 0.0

  ! AT FREE INTERNAL INTERFACE
    ELSE

      CRU = DT * RB * U( I , J )
      DRU = DT * SCXY * DXYU( I , J )

    ! DDU
      !BLOCK
        !BLOCK
          DDU = ( SS(I+1, J ) - SS( P , Q ) )
        !END BLOCK
      !END BLOCK

    ! D2U
      IF( CRU >= 0.0 ) THEN
        IF( ACTIVITY(P-1, Q ) == NOACTIVE ) THEN
          D2U = ( SS(I+1, J ) - SS( P , Q ) )   !
        ELSE
          D2U = ( SS(I+1, J ) - SS( P , Q ) )   &
          &   - ( SS( P , Q ) - SS(P-1, Q ) )   !\\\
        END IF
      ELSE
        IF( ACTIVITY(I+2, J ) == NOACTIVE ) THEN
          D2U = ( SS( P , Q ) - SS(I+1, J ) )   !
        ELSE
          D2U = ( SS( P , Q ) - SS(I+1, J ) )   &
          &   - ( SS(I+1, J ) - SS(I+2, J ) )   !\\\
        END IF
      END IF

    ! DCU
      IF( CRU >= -1.0 .AND. CRU <= +1.0 ) THEN
        !BLOCK
          DCU = ( SV(I+1, J ) - SV(I+1,J-1) ) * ( 1.0 - CRU ) / 2.0   &
          &   + ( SV( P ,Q-1) - SV( P , Q ) ) * ( 1.0 + CRU ) / 2.0   !\\\
        !END BLOCK
      ELSE IF( CRU > +1.0 ) THEN
        IF( ACTIVITY(P-1, Q ) == NOACTIVE ) THEN
          DCU = ( SV( P ,Q-1) - SV( P , Q ) )                         !
        ELSE
          DCU = ( SV( P ,Q-1) - SV( P , Q ) ) * ( 3.0 - CRU ) / 2.0   &
          &   - ( SV(P-1,Q-1) - SV(P-1, Q ) ) * ( 1.0 - CRU ) / 2.0   !\\\
        END IF
      ELSE
        IF( ACTIVITY(I+2, J ) == NOACTIVE ) THEN
          DCU = ( SV(I+1, J ) - SV(I+1,J-1) )                         !
        ELSE
          DCU = ( SV(I+1, J ) - SV(I+1,J-1) ) * ( 3.0 + CRU ) / 2.0   &
          &   - ( SV(I+2, J ) - SV(I+2,J-1) ) * ( 1.0 + CRU ) / 2.0   !\\\
        END IF
      END IF

    ! CALCULATE ADVECTION
      QADV_XY_SS_U( I , J ) = - CRU * MU( I , J ) *   &
      ( (       ( SS(I+1, J ) + SS( P , Q ) ) / 2.0   &
        -         CRU         * DDU           / 2.0   &
        - ( 1.0 - CRU * CRU ) * D2U           / 6.0   &
        )   &
      )
    ! CALCULATE DIFFUSION
      QDIF_XY_SS_U( I , J ) = + DRU * MU( I , J ) *   &
      ( FUU( I , J ,1,1) *   &
        (                       DDU                   &
        -         CRU         * D2U           / 2.0   &
        )   &
      + FUU( I , J ,1,2) *   &
                                DCU                   &
      )

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_U_I0_JJ

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
!   2015-06-04    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_U_I1_JJ

    IMPLICIT NONE

    REAL   (CARD_KIND)               :: CRU , DRU
    REAL   (CARD_KIND)               :: DDU , D2U , DCU

    INTEGER(IJID_KIND)               :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

  ! AT IMPERMEABLE INTERNAL INTERFACE
    IF( ACTIVITY(I+1, J ) == NOACTIVE .OR. ACTIVITY( I , J ) == NOACTIVE ) THEN

      QADV_XY_SS_U( I , J ) = 0.0
      QDIF_XY_SS_U( I , J ) = 0.0

  ! AT FREE INTERNAL INTERFACE
    ELSE

      CRU = DT * RB * U( I , J )
      DRU = DT * SCXY * DXYU( I , J )

    ! DDU
      !BLOCK
        !BLOCK
          DDU = ( SS(I+1, J ) - SS( I , J ) )
        !END BLOCK
      !END BLOCK

    ! D2U
      IF( CRU >= 0.0 ) THEN
        IF( ACTIVITY(P-1, Q ) == NOACTIVE ) THEN
          D2U = ( SS(I+1, J ) - SS( I , J ) )   !
        ELSE
          D2U = ( SS(I+1, J ) - SS( I , J ) )   &
          &   - ( SS( I , J ) - SS(P-1, Q ) )   !!!!!!
        END IF
      ELSE
        IF( ACTIVITY(I+2, J ) == NOACTIVE ) THEN
          D2U = ( SS( I , J ) - SS(I+1, J ) )   !
        ELSE
          D2U = ( SS( I , J ) - SS(I+1, J ) )   &
          &   - ( SS(I+1, J ) - SS(I+2, J ) )   !\\\
        END IF
      END IF

    ! DCU
      IF( CRU >= -1.0 .AND. CRU <= +1.0 ) THEN
        !BLOCK
          DCU = ( SV(I+1, J ) - SV(I+1,J-1) ) * ( 1.0 - CRU ) / 2.0   &
          &   + ( SV( I , J ) - SV( I ,J-1) ) * ( 1.0 + CRU ) / 2.0   !\\\
        !END BLOCK
      ELSE IF( CRU > +1.0 ) THEN
        IF( ACTIVITY(P-1, Q ) == NOACTIVE ) THEN
          DCU = ( SV( I , J ) - SV( I ,J-1) )                         !
        ELSE
          DCU = ( SV( I , J ) - SV( I ,J-1) ) * ( 3.0 - CRU ) / 2.0   &
          &   - ( SV(P-1,Q-1) - SV(P-1, Q ) ) * ( 1.0 - CRU ) / 2.0   !!!!!!
        END IF
      ELSE
        IF( ACTIVITY(I+2, J ) == NOACTIVE ) THEN
          DCU = ( SV(I+1, J ) - SV(I+1,J-1) )                         !
        ELSE
          DCU = ( SV(I+1, J ) - SV(I+1,J-1) ) * ( 3.0 + CRU ) / 2.0   &
          &   - ( SV(I+2, J ) - SV(I+2,J-1) ) * ( 1.0 + CRU ) / 2.0   !\\\
        END IF
      END IF

    ! CALCULATE ADVECTION
      QADV_XY_SS_U( I , J ) = - CRU * MU( I , J ) *   &
      ( (       ( SS(I+1, J ) + SS( I , J ) ) / 2.0   &
        -         CRU         * DDU           / 2.0   &
        - ( 1.0 - CRU * CRU ) * D2U           / 6.0   &
        )   &
      )
    ! CALCULATE DIFFUSION
      QDIF_XY_SS_U( I , J ) = + DRU * MU( I , J ) *   &
      ( FUU( I , J ,1,1) *   &
        (                       DDU                   &
        -         CRU         * D2U           / 2.0   &
        )   &
      + FUU( I , J ,1,2) *   &
                                DCU                   &
      )

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_U_I1_JJ

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
!   2015-06-04    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_U_II_JJ

    IMPLICIT NONE

    REAL   (CARD_KIND)               :: CRU , DRU
    REAL   (CARD_KIND)               :: DDU , D2U , DCU

  ! AT IMPERMEABLE INTERNAL INTERFACE
    IF( ACTIVITY(I+1, J ) == NOACTIVE .OR. ACTIVITY( I , J ) == NOACTIVE ) THEN

      QADV_XY_SS_U( I , J ) = 0.0
      QDIF_XY_SS_U( I , J ) = 0.0

  ! AT FREE INTERNAL INTERFACE
    ELSE

      CRU = DT * RB * U( I , J )
      DRU = DT * SCXY * DXYU( I , J )

    ! DDU
      !BLOCK
        !BLOCK
          DDU = ( SS(I+1, J ) - SS( I , J ) )
        !END BLOCK
      !END BLOCK

    ! D2U
      IF( CRU >= 0.0 ) THEN
        IF( ACTIVITY(I-1, J ) == NOACTIVE ) THEN
          D2U = ( SS(I+1, J ) - SS( I , J ) )   !
        ELSE
          D2U = ( SS(I+1, J ) - SS( I , J ) )   &
          &   - ( SS( I , J ) - SS(I-1, J ) )   !\\\
        END IF
      ELSE
        IF( ACTIVITY(I+2, J ) == NOACTIVE ) THEN
          D2U = ( SS( I , J ) - SS(I+1, J ) )   !
        ELSE
          D2U = ( SS( I , J ) - SS(I+1, J ) )   &
          &   - ( SS(I+1, J ) - SS(I+2, J ) )   !\\\
        END IF
      END IF

    ! DCU
      IF( CRU >= -1.0 .AND. CRU <= +1.0 ) THEN
        !BLOCK
          DCU = ( SV(I+1, J ) - SV(I+1,J-1) ) * ( 1.0 - CRU ) / 2.0   &
          &   + ( SV( I , J ) - SV( I ,J-1) ) * ( 1.0 + CRU ) / 2.0   !\\\
        !END BLOCK
      ELSE IF( CRU > +1.0 ) THEN
        IF( ACTIVITY(I-1, J ) == NOACTIVE ) THEN
          DCU = ( SV( I , J ) - SV( I ,J-1) )                         !
        ELSE
          DCU = ( SV( I , J ) - SV( I ,J-1) ) * ( 3.0 - CRU ) / 2.0   &
          &   - ( SV(I-1, J ) - SV(I-1,J-1) ) * ( 1.0 - CRU ) / 2.0   !\\\
        END IF
      ELSE
        IF( ACTIVITY(I+2, J ) == NOACTIVE ) THEN
          DCU = ( SV(I+1, J ) - SV(I+1,J-1) )                         !
        ELSE
          DCU = ( SV(I+1, J ) - SV(I+1,J-1) ) * ( 3.0 + CRU ) / 2.0   &
          &   - ( SV(I+2, J ) - SV(I+2,J-1) ) * ( 1.0 + CRU ) / 2.0   !\\\
        END IF
      END IF

    ! CALCULATE ADVECTION
      QADV_XY_SS_U( I , J ) = - CRU * MU( I , J ) *   &
      ( (       ( SS(I+1, J ) + SS( I , J ) ) / 2.0   &
        -         CRU         * DDU           / 2.0   &
        - ( 1.0 - CRU * CRU ) * D2U           / 6.0   &
        )   &
      )
    ! CALCULATE DIFFUSION
      QDIF_XY_SS_U( I , J ) = + DRU * MU( I , J ) *   &
      ( FUU( I , J ,1,1) *   &
        (                       DDU                   &
        -         CRU         * D2U           / 2.0   &
        )   &
      + FUU( I , J ,1,2) *   &
                                DCU                   &
      )

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_U_II_JJ

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
!   2015-06-04    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_U_IM_JJ

    IMPLICIT NONE

    REAL   (CARD_KIND)               :: CRU , DRU
    REAL   (CARD_KIND)               :: DDU , D2U , DCU

    INTEGER(IJID_KIND)               :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

  ! AT IMPERMEABLE INTERNAL INTERFACE
    IF( ACTIVITY(I+1, J ) == NOACTIVE .OR. ACTIVITY( I , J ) == NOACTIVE ) THEN

      QADV_XY_SS_U( I , J ) = 0.0
      QDIF_XY_SS_U( I , J ) = 0.0

  ! AT FREE INTERNAL INTERFACE
    ELSE

      CRU = DT * RB * U( I , J )
      DRU = DT * SCXY * DXYU( I , J )

    ! DDU
      !BLOCK
        !BLOCK
          DDU = ( SS(I+1, J ) - SS( I , J ) )
        !END BLOCK
      !END BLOCK

    ! D2U
      IF( CRU >= 0.0 ) THEN
        IF( ACTIVITY(I-1, J ) == NOACTIVE ) THEN
          D2U = ( SS(I+1, J ) - SS( I , J ) )   !
        ELSE
          D2U = ( SS(I+1, J ) - SS( I , J ) )   &
          &   - ( SS( I , J ) - SS(I-1, J ) )   !\\\
        END IF
      ELSE
        IF( ACTIVITY(P+2, Q ) == NOACTIVE ) THEN
          D2U = ( SS( I , J ) - SS(I+1, J ) )   !
        ELSE
          D2U = ( SS( I , J ) - SS(I+1, J ) )   &
          &   - ( SS(I+1, J ) - SS(P+2, Q ) )   !!!!!!
        END IF
      END IF

    ! DCU
      IF( CRU >= -1.0 .AND. CRU <= +1.0 ) THEN
        !BLOCK
          DCU = ( SV(I+1, J ) - SV(I+1,J-1) ) * ( 1.0 - CRU ) / 2.0   &
          &   + ( SV( I , J ) - SV( I ,J-1) ) * ( 1.0 + CRU ) / 2.0   !\\\
        !END BLOCK
      ELSE IF( CRU > +1.0 ) THEN
        IF( ACTIVITY(I-1, J ) == NOACTIVE ) THEN
          DCU = ( SV( I , J ) - SV( I ,J-1) )                         !
        ELSE
          DCU = ( SV( I , J ) - SV( I ,J-1) ) * ( 3.0 - CRU ) / 2.0   &
          &   - ( SV(I-1, J ) - SV(I-1,J-1) ) * ( 1.0 - CRU ) / 2.0   !\\\
        END IF
      ELSE
        IF( ACTIVITY(P+2, Q ) == NOACTIVE ) THEN
          DCU = ( SV(I+1, J ) - SV(I+1,J-1) )                         !
        ELSE
          DCU = ( SV(I+1, J ) - SV(I+1,J-1) ) * ( 3.0 + CRU ) / 2.0   &
          &   - ( SV(P+2,Q-1) - SV(P+2, Q ) ) * ( 1.0 + CRU ) / 2.0   !!!!!!
        END IF
      END IF

    ! CALCULATE ADVECTION
      QADV_XY_SS_U( I , J ) = - CRU * MU( I , J ) *   &
      ( (       ( SS(I+1, J ) + SS( I , J ) ) / 2.0   &
        -         CRU         * DDU           / 2.0   &
        - ( 1.0 - CRU * CRU ) * D2U           / 6.0   &
        )   &
      )
    ! CALCULATE DIFFUSION
      QDIF_XY_SS_U( I , J ) = + DRU * MU( I , J ) *   &
      ( FUU( I , J ,1,1) *   &
        (                       DDU                   &
        -         CRU         * D2U           / 2.0   &
        )   &
      + FUU( I , J ,1,2) *   &
                                DCU                   &
      )

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_U_IM_JJ

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
!   2015-06-04    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_U_IN_JJ

    IMPLICIT NONE

    REAL   (CARD_KIND)               :: CRU , DRU
    REAL   (CARD_KIND)               :: DDU , D2U , DCU

    INTEGER(IJID_KIND)               :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

  ! AT IMPERMEABLE INTERNAL INTERFACE
    IF( ACTIVITY(P+1, Q ) == NOACTIVE .OR. ACTIVITY( I , J ) == NOACTIVE ) THEN

      QADV_XY_SS_U( I , J ) = 0.0
      QDIF_XY_SS_U( I , J ) = 0.0

  ! AT FREE INTERNAL INTERFACE
    ELSE

      CRU = DT * RB * U( I , J )
      DRU = DT * SCXY * DXYU( I , J )

    ! DDU
      !BLOCK
        !BLOCK
          DDU = ( SS(P+1, Q ) - SS( I , J ) )
        !END BLOCK
      !END BLOCK

    ! D2U
      IF( CRU >= 0.0 ) THEN
        IF( ACTIVITY(I-1, J ) == NOACTIVE ) THEN
          D2U = ( SS(P+1, Q ) - SS( I , J ) )   !
        ELSE
          D2U = ( SS(P+1, Q ) - SS( I , J ) )   &
          &   - ( SS( I , J ) - SS(I-1, J ) )   !\\\
        END IF
      ELSE
        IF( ACTIVITY(P+2, Q ) == NOACTIVE ) THEN
          D2U = ( SS( I , J ) - SS(P+1, Q ) )   !
        ELSE
          D2U = ( SS( I , J ) - SS(P+1, Q ) )   &
          &   - ( SS(P+1, Q ) - SS(P+2, Q ) )   !\\\
        END IF
      END IF

    ! DCU
      IF( CRU >= -1.0 .AND. CRU <= +1.0 ) THEN
        !BLOCK
          DCU = ( SV(P+1,Q-1) - SV(P+1, Q ) ) * ( 1.0 - CRU ) / 2.0   &
          &   + ( SV( I , J ) - SV( I ,J-1) ) * ( 1.0 + CRU ) / 2.0   !\\\
        !END BLOCK
      ELSE IF( CRU > +1.0 ) THEN
        IF( ACTIVITY(I-1, J ) == NOACTIVE ) THEN
          DCU = ( SV( I , J ) - SV( I ,J-1) )                         !
        ELSE
          DCU = ( SV( I , J ) - SV( I ,J-1) ) * ( 3.0 - CRU ) / 2.0   &
          &   - ( SV(I-1, J ) - SV(I-1,J-1) ) * ( 1.0 - CRU ) / 2.0   !\\\
        END IF
      ELSE
        IF( ACTIVITY(P+2, Q ) == NOACTIVE ) THEN
          DCU = ( SV(P+1,Q-1) - SV(I+1, Q ) )                         !
        ELSE
          DCU = ( SV(P+1,Q-1) - SV(P+1, Q ) ) * ( 3.0 + CRU ) / 2.0   &
          &   - ( SV(P+2,Q-1) - SV(P+2, Q ) ) * ( 1.0 + CRU ) / 2.0   !\\\
        END IF
      END IF

    ! CALCULATE ADVECTION
      QADV_XY_SS_U( I , J ) = - CRU * MU( I , J ) *   &
      ( (       ( SS(P+1, Q ) + SS( I , J ) ) / 2.0   &
        -         CRU         * DDU           / 2.0   &
        - ( 1.0 - CRU * CRU ) * D2U           / 6.0   &
        )   &
      )
    ! CALCULATE DIFFUSION
      QDIF_XY_SS_U( I , J ) = + DRU * MU( I , J ) *   &
      ( FUU( I , J ,1,1) *   &
        (                       DDU                   &
        -         CRU         * D2U           / 2.0   &
        )   &
      + FUU( I , J ,1,2) *   &
                                DCU                   &
      )

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_U_IN_JJ

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_U

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE) CALC_QADV_N_QDIF_XY_UV_V
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
!   2015-06-24    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_V( NI , NJ , UV , UU , RB , V , QADV_XY_UV_V , EKXY , VXYV , QDIF_XY_UV_V )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: UV
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ,1:2) :: UU

    REAL   (PARD_KIND) , INTENT(IN ) :: RB , EKXY
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ    ) :: V , VXYV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ,1:2) :: QADV_XY_UV_V , QDIF_XY_UV_V

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    DO DIM = 1 , 2

      J = 0
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
         !DIR$ FORCEINLINE
          CALL MR_CALC_QADV_N_QDIF_XY_UV_V_II_J0
        END DO
      !END J = 0

      J = 1
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
         !DIR$ FORCEINLINE
          CALL MR_CALC_QADV_N_QDIF_XY_UV_V_II_J1
        END DO
      !END J = 1

      DO J = 2 , NJ-2
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
         !DIR$ FORCEINLINE
          CALL MR_CALC_QADV_N_QDIF_XY_UV_V_II_JJ
        END DO
      END DO

      J = NJ-1
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
         !DIR$ FORCEINLINE
          CALL MR_CALC_QADV_N_QDIF_XY_UV_V_II_JM
        END DO
      !END J = NJ-1

      J = NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
         !DIR$ FORCEINLINE
          CALL MR_CALC_QADV_N_QDIF_XY_UV_V_II_JN
        END DO
      !END J = NJ

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
!   2015-06-10    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_V_II_J0

    IMPLICIT NONE

    QADV_XY_UV_V( I , J ,DIM) = 0.0
    QDIF_XY_UV_V( I , J ,DIM) = 0.0

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_V_II_J0

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_V_II_J1

    IMPLICIT NONE

    REAL   (CARD_KIND)               :: CRV , DRV
    REAL   (CARD_KIND)               :: DDV , D2V , DCV

  ! AT IMPERMEABLE INTERNAL INTERFACE
    IF( ACTIVITY( I , J ) == NOACTIVE .OR. ACTIVITY( I ,J+1) == NOACTIVE ) THEN

      QADV_XY_UV_V( I , J ,DIM) = 0.0
      QDIF_XY_UV_V( I , J ,DIM) = 0.0

  ! AT FREE INTERNAL INTERFACE
    ELSE

      CRV = DT * RB * V( I , J )
      DRV = DT * EKXY * VXYV( I , J )

    ! DDV
      !BLOCK
        !BLOCK
          DDV = ( UV( I ,J+1,DIM) - UV( I , J ,DIM) )
        !END BLOCK
      !END BLOCK

    ! D2V
      IF( CRV >= 0.0 ) THEN
        !BLOCK
          D2V = ( UV( I ,J+1,DIM) - UV( I , J ,DIM) )   !!!!!!
        !END BLOCK
      ELSE
        IF( ACTIVITY( I ,J+2) == NOACTIVE ) THEN
          D2V = ( UV( I , J ,DIM) - UV( I ,J+1,DIM) )   !
        ELSE
          D2V = ( UV( I , J ,DIM) - UV( I ,J+1,DIM) )   &
          &   - ( UV( I ,J+1,DIM) - UV( I ,J+2,DIM) )   !\\\
        END IF
      END IF

    ! DCV
      IF( CRV >= -1.0 .AND. CRV <= +1.0 ) THEN
        !BLOCK
          DCV = ( UU( I ,J+1,DIM) - UU(I-1,J+1,DIM) ) * ( 1.0 - CRV ) / 2.0   &
          &   + ( UU( I , J ,DIM) - UU(I-1, J ,DIM) ) * ( 1.0 + CRV ) / 2.0   !\\\
        !END BLOCK
      ELSE IF( CRV > +1.0 ) THEN
        !BLOCK
          DCV = ( UU( I , J ,DIM) - UU(I-1, J ,DIM) )                         !!!!!!
        !END BLOCK
      ELSE
        IF( ACTIVITY( I ,J+2) == NOACTIVE ) THEN
          DCV = ( UU( I ,J+1,DIM) - UU(I-1,J+1,DIM) )                         !
        ELSE
          DCV = ( UU( I ,J+1,DIM) - UU(I-1,J+1,DIM) ) * ( 3.0 + CRV ) / 2.0   &
          &   - ( UU( I ,J+2,DIM) - UU(I-1,J+2,DIM) ) * ( 1.0 + CRV ) / 2.0   !\\\
        END IF
      END IF

    ! CALCULATE ADVECTION
      QADV_XY_UV_V( I , J ,DIM) = - CRV * MV( I , J ) *   &
      ( (       ( UV( I ,J+1,DIM) + UV( I , J ,DIM) ) / 2.0   &
        -         CRV         * DDV                   / 2.0   &
        - ( 1.0 - CRV * CRV ) * D2V                   / 6.0   &
        )   &
      )
    ! CALCULATE DIFFUSION
      QDIF_XY_UV_V( I , J ,DIM) = + DRV * MV( I , J ) *   &
      ( FVV( I , J ,2,2) *   &
        (                       DDV                           &
        -         CRV         * D2V                   / 2.0   &
        )   &
      + FVV( I , J ,2,1) *   &
                                DCV                           &
      )

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_V_II_J1

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_V_II_JJ

    IMPLICIT NONE

    REAL   (CARD_KIND)               :: CRV , DRV
    REAL   (CARD_KIND)               :: DDV , D2V , DCV

  ! AT IMPERMEABLE INTERNAL INTERFACE
    IF( ACTIVITY( I , J ) == NOACTIVE .OR. ACTIVITY( I ,J+1) == NOACTIVE ) THEN

      QADV_XY_UV_V( I , J ,DIM) = 0.0
      QDIF_XY_UV_V( I , J ,DIM) = 0.0

  ! AT FREE INTERNAL INTERFACE
    ELSE

      CRV = DT * RB * V( I , J )
      DRV = DT * EKXY * VXYV( I , J )

    ! DDV
      !BLOCK
        !BLOCK
          DDV = ( UV( I ,J+1,DIM) - UV( I , J ,DIM) )
        !END BLOCK
      !END BLOCK

    ! D2V
      IF( CRV >= 0.0 ) THEN
        IF( ACTIVITY( I ,J-1) == NOACTIVE ) THEN
          D2V = ( UV( I ,J+1,DIM) - UV( I , J ,DIM) )   !
        ELSE
          D2V = ( UV( I ,J+1,DIM) - UV( I , J ,DIM) )   &
          &   - ( UV( I , J ,DIM) - UV( I ,J-1,DIM) )   !\\\
        END IF
      ELSE
        IF( ACTIVITY( I ,J+2) == NOACTIVE ) THEN
          D2V = ( UV( I , J ,DIM) - UV( I ,J+1,DIM) )   !
        ELSE
          D2V = ( UV( I , J ,DIM) - UV( I ,J+1,DIM) )   &
          &   - ( UV( I ,J+1,DIM) - UV( I ,J+2,DIM) )   !\\\
        END IF
      END IF

    ! DCV
      IF( CRV >= -1.0 .AND. CRV <= +1.0 ) THEN
        !BLOCK
          DCV = ( UU( I ,J+1,DIM) - UU(I-1,J+1,DIM) ) * ( 1.0 - CRV ) / 2.0   &
          &   + ( UU( I , J ,DIM) - UU(I-1, J ,DIM) ) * ( 1.0 + CRV ) / 2.0   !\\\
        !END BLOCK
      ELSE IF( CRV > +1.0 ) THEN
        IF( ACTIVITY( I ,J-1) == NOACTIVE ) THEN
          DCV = ( UU( I , J ,DIM) - UU(I-1, J ,DIM) )                         !
        ELSE
          DCV = ( UU( I , J ,DIM) - UU(I-1, J ,DIM) ) * ( 3.0 - CRV ) / 2.0   &
          &   - ( UU( I ,J-1,DIM) - UU(I-1,J-1,DIM) ) * ( 1.0 - CRV ) / 2.0   !\\\
        END IF
      ELSE
        IF( ACTIVITY( I ,J+2) == NOACTIVE ) THEN
          DCV = ( UU( I ,J+1,DIM) - UU(I-1,J+1,DIM) )                         !
        ELSE
          DCV = ( UU( I ,J+1,DIM) - UU(I-1,J+1,DIM) ) * ( 3.0 + CRV ) / 2.0   &
          &   - ( UU( I ,J+2,DIM) - UU(I-1,J+2,DIM) ) * ( 1.0 + CRV ) / 2.0   !\\\
        END IF
      END IF

    ! CALCULATE ADVECTION
      QADV_XY_UV_V( I , J ,DIM) = - CRV * MV( I , J ) *   &
      ( (       ( UV( I ,J+1,DIM) + UV( I , J ,DIM) ) / 2.0   &
        -         CRV         * DDV                   / 2.0   &
        - ( 1.0 - CRV * CRV ) * D2V                   / 6.0   &
        )   &
      )
    ! CALCULATE DIFFUSION
      QDIF_XY_UV_V( I , J ,DIM) = + DRV * MV( I , J ) *   &
      ( FVV( I , J ,2,2) *   &
        (                       DDV                           &
        -         CRV         * D2V                   / 2.0   &
        )   &
      + FVV( I , J ,2,1) *   &
                                DCV                           &
      )

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_V_II_JJ

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_V_II_JM

    IMPLICIT NONE

    REAL   (CARD_KIND)               :: CRV , DRV
    REAL   (CARD_KIND)               :: DDV , D2V , DCV

  ! AT IMPERMEABLE INTERNAL INTERFACE
    IF( ACTIVITY( I , J ) == NOACTIVE .OR. ACTIVITY( I ,J+1) == NOACTIVE ) THEN

      QADV_XY_UV_V( I , J ,DIM) = 0.0
      QDIF_XY_UV_V( I , J ,DIM) = 0.0

  ! AT FREE INTERNAL INTERFACE
    ELSE

      CRV = DT * RB * V( I , J )
      DRV = DT * EKXY * VXYV( I , J )

    ! DDV
      !BLOCK
        !BLOCK
          DDV = ( UV( I ,J+1,DIM) - UV( I , J ,DIM) )
        !END BLOCK
      !END BLOCK

    ! D2V
      IF( CRV >= 0.0 ) THEN
        IF( ACTIVITY( I ,J-1) == NOACTIVE ) THEN
          D2V = ( UV( I ,J+1,DIM) - UV( I , J ,DIM) )   !
        ELSE
          D2V = ( UV( I ,J+1,DIM) - UV( I , J ,DIM) )   &
          &   - ( UV( I , J ,DIM) - UV( I ,J-1,DIM) )   !\\\
        END IF
      ELSE
        !BLOCK
          D2V = ( UV( I , J ,DIM) - UV( I ,J+1,DIM) )   !!!!!!
        !END BLOCK
      END IF

    ! DCV
      IF( CRV >= -1.0 .AND. CRV <= +1.0 ) THEN
        !BLOCK
          DCV = ( UU( I ,J+1,DIM) - UU(I-1,J+1,DIM) ) * ( 1.0 - CRV ) / 2.0   &
          &   + ( UU( I , J ,DIM) - UU(I-1, J ,DIM) ) * ( 1.0 + CRV ) / 2.0   !\\\
        !END BLOCK
      ELSE IF( CRV > +1.0 ) THEN
        IF( ACTIVITY( I ,J-1) == NOACTIVE ) THEN
          DCV = ( UU( I , J ,DIM) - UU(I-1, J ,DIM) )                         !
        ELSE
          DCV = ( UU( I , J ,DIM) - UU(I-1, J ,DIM) ) * ( 3.0 - CRV ) / 2.0   &
          &   - ( UU( I ,J-1,DIM) - UU(I-1,J-1,DIM) ) * ( 1.0 - CRV ) / 2.0   !\\\
        END IF
      ELSE
        !BLOCK
          DCV = ( UU( I ,J+1,DIM) - UU(I-1,J+1,DIM) )                         !!!!!!
        !END BLOCK
      END IF

    ! CALCULATE ADVECTION
      QADV_XY_UV_V( I , J ,DIM) = - CRV * MV( I , J ) *   &
      ( (       ( UV( I ,J+1,DIM) + UV( I , J ,DIM) ) / 2.0   &
        -         CRV         * DDV                   / 2.0   &
        - ( 1.0 - CRV * CRV ) * D2V                   / 6.0   &
        )   &
      )
    ! CALCULATE DIFFUSION
      QDIF_XY_UV_V( I , J ,DIM) = + DRV * MV( I , J ) *   &
      ( FVV( I , J ,2,2) *   &
        (                       DDV                           &
        -         CRV         * D2V                   / 2.0   &
        )   &
      + FVV( I , J ,2,1) *   &
                                DCV                           &
      )

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_V_II_JM

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_V_II_JN

    IMPLICIT NONE

    QADV_XY_UV_V( I , J ,DIM) = 0.0
    QDIF_XY_UV_V( I , J ,DIM) = 0.0

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_V_II_JN

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_V

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE) CALC_QADV_N_QDIF_XY_SS_V
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
!   2015-06-24    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_V( NI , NJ , SS , SU , RB , V , QADV_XY_SS_V , SCXY , DXYV , QDIF_XY_SS_V )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) :: SS
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ) :: SU

    REAL   (PARD_KIND) , INTENT(IN ) :: RB , SCXY
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ) :: V , DXYV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ) :: QADV_XY_SS_V , QDIF_XY_SS_V

    INTEGER(IJID_KIND) :: I , J

    J = 0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
       !DIR$ FORCEINLINE
        CALL MR_CALC_QADV_N_QDIF_XY_SS_V_II_J0
      END DO
    !END J = 0

    J = 1
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
       !DIR$ FORCEINLINE
        CALL MR_CALC_QADV_N_QDIF_XY_SS_V_II_J1
      END DO
    !END J = 1

    DO J = 2 , NJ-2
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
       !DIR$ FORCEINLINE
        CALL MR_CALC_QADV_N_QDIF_XY_SS_V_II_JJ
      END DO
    END DO

    J = NJ-1
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
       !DIR$ FORCEINLINE
        CALL MR_CALC_QADV_N_QDIF_XY_SS_V_II_JM
      END DO
    !END J = NJ-1

    J = NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
       !DIR$ FORCEINLINE
        CALL MR_CALC_QADV_N_QDIF_XY_SS_V_II_JN
      END DO
    !END J = NJ

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_V_II_J0

    IMPLICIT NONE

    QADV_XY_SS_V( I , J ) = 0.0
    QDIF_XY_SS_V( I , J ) = 0.0

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_V_II_J0

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_V_II_J1

    IMPLICIT NONE

    REAL   (CARD_KIND)               :: CRV , DRV
    REAL   (CARD_KIND)               :: DDV , D2V , DCV

  ! AT IMPERMEABLE BOUNDARY
    IF( ACTIVITY( I , J ) == NOACTIVE .OR. ACTIVITY( I ,J+1) == NOACTIVE ) THEN

      QADV_XY_SS_V( I , J ) = 0.0
      QDIF_XY_SS_V( I , J ) = 0.0

  ! AT FREE INTERFACE
    ELSE

      CRV = DT * RB * V( I , J )
      DRV = DT * SCXY * DXYV( I , J )

    ! DDV
      !BLOCK
        !BLOCK
          DDV = ( SS( I ,J+1) - SS( I , J ) )
        !END BLOCK
      !END BLOCK

    ! D2V
      IF( CRV >= 0.0 ) THEN
        !BLOCK
          D2V = ( SS( I ,J+1) - SS( I , J ) )   !!!!!!
        !END BLOCK
      ELSE
        IF( ACTIVITY( I ,J+2) == NOACTIVE ) THEN
          D2V = ( SS( I , J ) - SS( I ,J+1) )   !
        ELSE
          D2V = ( SS( I , J ) - SS( I ,J+1) )   &
          &   - ( SS( I ,J+1) - SS( I ,J+2) )   !\\\
        END IF
      END IF

    ! DCV
      IF( CRV >= -1.0 .AND. CRV <= +1.0 ) THEN
        !BLOCK
          DCV = ( SU( I ,J+1) - SU(I-1,J+1) ) * ( 1.0 - CRV ) / 2.0   &
          &   + ( SU( I , J ) - SU(I-1, J ) ) * ( 1.0 + CRV ) / 2.0   !\\\
        !END BLOCK
      ELSE IF( CRV > +1.0 ) THEN
        !BLOCK
          DCV = ( SU( I , J ) - SU(I-1, J ) )                         !!!!!!
        !END BLOCK
      ELSE
        IF( ACTIVITY( I ,J+2) == NOACTIVE ) THEN
          DCV = ( SU( I ,J+1) - SU(I-1,J+1) )                         !
        ELSE
          DCV = ( SU( I ,J+1) - SU(I-1,J+1) ) * ( 3.0 + CRV ) / 2.0   &
          &   - ( SU( I ,J+2) - SU(I-1,J+2) ) * ( 1.0 + CRV ) / 2.0   !\\\
        END IF
      END IF

    ! CALCULATE ADVECTION
      QADV_XY_SS_V( I , J ) = - CRV * MV( I , J ) *   &
      ( (       ( SS( I ,J+1) + SS( I , J ) )     / 2.0   &
        -         CRV         * DDV               / 2.0   &
        - ( 1.0 - CRV * CRV ) * D2V               / 6.0   &
        )   &
      )
    ! CALCULATE DIFFUSION
      QDIF_XY_SS_V( I , J ) = + DRV * MV( I , J ) *   &
      ( FVV( I , J ,2,2) *   &
        (                       DDV                       &
        -         CRV         * D2V               / 2.0   &
        )   &
      + FVV( I , J ,2,1) *   &
                                DCV                       &
      )

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_V_II_J1

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_V_II_JJ

    IMPLICIT NONE

    REAL   (CARD_KIND)               :: CRV , DRV
    REAL   (CARD_KIND)               :: DDV , D2V , DCV

  ! AT IMPERMEABLE BOUNDARY
    IF( ACTIVITY( I , J ) == NOACTIVE .OR. ACTIVITY( I ,J+1) == NOACTIVE ) THEN

      QADV_XY_SS_V( I , J ) = 0.0
      QDIF_XY_SS_V( I , J ) = 0.0

  ! AT FREE INTERFACE
    ELSE

      CRV = DT * RB * V( I , J )
      DRV = DT * SCXY * DXYV( I , J )

    ! DDV
      !BLOCK
        !BLOCK
          DDV = ( SS( I ,J+1) - SS( I , J ) )
        !END BLOCK
      !END BLOCK

    ! D2V
      IF( CRV >= 0.0 ) THEN
        IF( ACTIVITY( I ,J-1) == NOACTIVE ) THEN
          D2V = ( SS( I ,J+1) - SS( I , J ) )   !
        ELSE
          D2V = ( SS( I ,J+1) - SS( I , J ) )   &
          &   - ( SS( I , J ) - SS( I ,J-1) )   !\\\
        END IF
      ELSE
        IF( ACTIVITY( I ,J+2) == NOACTIVE ) THEN
          D2V = ( SS( I , J ) - SS( I ,J+1) )   !
        ELSE
          D2V = ( SS( I , J ) - SS( I ,J+1) )   &
          &   - ( SS( I ,J+1) - SS( I ,J+2) )   !\\\
        END IF
      END IF

    ! DCV
      IF( CRV >= -1.0 .AND. CRV <= +1.0 ) THEN
        !BLOCK
          DCV = ( SU( I ,J+1) - SU(I-1,J+1) ) * ( 1.0 - CRV ) / 2.0   &
          &   + ( SU( I , J ) - SU(I-1, J ) ) * ( 1.0 + CRV ) / 2.0   !\\\
        !END BLOCK
      ELSE IF( CRV > +1.0 ) THEN
        IF( ACTIVITY( I ,J-1) == NOACTIVE ) THEN
          DCV = ( SU( I , J ) - SU(I-1, J ) )                         !
        ELSE
          DCV = ( SU( I , J ) - SU(I-1, J ) ) * ( 3.0 - CRV ) / 2.0   &
          &   - ( SU( I ,J-1) - SU(I-1,J-1) ) * ( 1.0 - CRV ) / 2.0   !\\\
        END IF
      ELSE
        IF( ACTIVITY( I ,J+2) == NOACTIVE ) THEN
          DCV = ( SU( I ,J+1) - SU(I-1,J+1) )                         !
        ELSE
          DCV = ( SU( I ,J+1) - SU(I-1,J+1) ) * ( 3.0 + CRV ) / 2.0   &
          &   - ( SU( I ,J+2) - SU(I-1,J+2) ) * ( 1.0 + CRV ) / 2.0   !\\\
        END IF
      END IF

    ! CALCULATE ADVECTION
      QADV_XY_SS_V( I , J ) = - CRV * MV( I , J ) *   &
      ( (       ( SS( I ,J+1) + SS( I , J ) )     / 2.0   &
        -         CRV         * DDV               / 2.0   &
        - ( 1.0 - CRV * CRV ) * D2V               / 6.0   &
        )   &
      )
    ! CALCULATE DIFFUSION
      QDIF_XY_SS_V( I , J ) = + DRV * MV( I , J ) *   &
      ( FVV( I , J ,2,2) *   &
        (                       DDV                       &
        -         CRV         * D2V               / 2.0   &
        )   &
      + FVV( I , J ,2,1) *   &
                                DCV                       &
      )

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_V_II_JJ

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_V_II_JM

    IMPLICIT NONE

    REAL   (CARD_KIND)               :: CRV , DRV
    REAL   (CARD_KIND)               :: DDV , D2V , DCV

  ! AT IMPERMEABLE BOUNDARY
    IF( ACTIVITY( I , J ) == NOACTIVE .OR. ACTIVITY( I ,J+1) == NOACTIVE ) THEN

      QADV_XY_SS_V( I , J ) = 0.0
      QDIF_XY_SS_V( I , J ) = 0.0

  ! AT FREE INTERFACE
    ELSE

      CRV = DT * RB * V( I , J )
      DRV = DT * SCXY * DXYV( I , J )

    ! DDV
      !BLOCK
        !BLOCK
          DDV = ( SS( I ,J+1) - SS( I , J ) )
        !END BLOCK
      !END BLOCK

    ! D2V
      IF( CRV >= 0.0 ) THEN
        IF( ACTIVITY( I ,J-1) == NOACTIVE ) THEN
          D2V = ( SS( I ,J+1) - SS( I , J ) )   !
        ELSE
          D2V = ( SS( I ,J+1) - SS( I , J ) )   &
          &   - ( SS( I , J ) - SS( I ,J-1) )   !\\\
        END IF
      ELSE
        !BLOCK
          D2V = ( SS( I , J ) - SS( I ,J+1) )   !!!!!!
        !END BLOCK
      END IF

    ! DCV
      IF( CRV >= -1.0 .AND. CRV <= +1.0 ) THEN
        !BLOCK
          DCV = ( SU( I ,J+1) - SU(I-1,J+1) ) * ( 1.0 - CRV ) / 2.0   &
          &   + ( SU( I , J ) - SU(I-1, J ) ) * ( 1.0 + CRV ) / 2.0   !\\\
        !END BLOCK
      ELSE IF( CRV > +1.0 ) THEN
        IF( ACTIVITY( I ,J-1) == NOACTIVE ) THEN
          DCV = ( SU( I , J ) - SU(I-1, J ) )                         !
        ELSE
          DCV = ( SU( I , J ) - SU(I-1, J ) ) * ( 3.0 - CRV ) / 2.0   &
          &   - ( SU( I ,J-1) - SU(I-1,J-1) ) * ( 1.0 - CRV ) / 2.0   !\\\
        END IF
      ELSE
        !BLOCK
          DCV = ( SU( I ,J+1) - SU(I-1,J+1) )                         !!!!!!
        !END BLOCK
      END IF

    ! CALCULATE ADVECTION
      QADV_XY_SS_V( I , J ) = - CRV * MV( I , J ) *   &
      ( (       ( SS( I ,J+1) + SS( I , J ) )     / 2.0   &
        -         CRV         * DDV               / 2.0   &
        - ( 1.0 - CRV * CRV ) * D2V               / 6.0   &
        )   &
      )
    ! CALCULATE DIFFUSION
      QDIF_XY_SS_V( I , J ) = + DRV * MV( I , J ) *   &
      ( FVV( I , J ,2,2) *   &
        (                       DDV                       &
        -         CRV         * D2V               / 2.0   &
        )   &
      + FVV( I , J ,2,1) *   &
                                DCV                       &
      )

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_V_II_JM

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_V_II_JN

    IMPLICIT NONE

    QADV_XY_SS_V( I , J ) = 0.0
    QDIF_XY_SS_V( I , J ) = 0.0

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_V_II_JN

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_V

  END MODULE MR_MOD_CALC_QADV_N_QDIF_XY
