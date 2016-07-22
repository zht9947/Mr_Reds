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

    PUBLIC :: CALC_QADV_N_QDIF_XY_UV_U
    PUBLIC :: CALC_QADV_N_QDIF_XY_SS_U

    PUBLIC :: CALC_QADV_N_QDIF_XY_UV_V
    PUBLIC :: CALC_QADV_N_QDIF_XY_SS_V

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_U( NI , NJ , UV , VV , RB , U , QADV_XY_UV_U , EKXY , VXYU , QDIF_XY_UV_U ,   &
  & UU_AT_I0 , DC_UU_AT_I0 )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ,1:2) :: UV
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI,0:NJ,1:2) :: VV

    REAL   (PARD_KIND) , INTENT(IN ) :: RB , EKXY
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI,1:NJ    ) :: U , VXYU
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI,1:NJ,1:2) :: QADV_XY_UV_U , QDIF_XY_UV_U

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(     1:NJ,1:2) :: UU_AT_I0 , DC_UU_AT_I0


    INTEGER(IJID_KIND) :: J

    INTEGER :: DIM

    DO DIM = 1 , 2

      DO J = 1 , NJ

        CALL MR_CALC_QADV_N_QDIF_XY_UV_U_AT_I0
        CALL MR_CALC_QADV_N_QDIF_XY_UV_U_AT_I1
        CALL MR_CALC_QADV_N_QDIF_XY_UV_U_AT_II
        CALL MR_CALC_QADV_N_QDIF_XY_UV_U_AT_IM
        CALL MR_CALC_QADV_N_QDIF_XY_UV_U_AT_IN

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_U_AT_I0

    IMPLICIT NONE

    REAL   (CARD_KIND) :: CRU , DRU
    REAL   (CARD_KIND) :: DDU , D2U , DCU

    INTEGER(IJID_KIND) :: I

    I = 0

  ! AT IMPERMEABLE BOUNDARY
    IF( ACTIVITY(I+1, J ) == NOACTIVE ) THEN

      QADV_XY_UV_U( I , J ,DIM) = 0.0
      QDIF_XY_UV_U( I , J ,DIM) = 0.0

  ! AT FREE BOUNDARY
    ELSE

      CRU = DT * RB * U( I , J )                                                !///
      DRU = DT * EKXY * VXYU( I , J )                                           !///

    ! DDU
      IF( ACTIVITY(I+2, J ) == NOACTIVE ) THEN
        DDU = ( UV(I+1, J ,DIM) - UU_AT_I0( J ,DIM) ) * 8.0 / 3.0               !
      ELSE
        DDU = ( UV(I+1, J ,DIM) - UU_AT_I0( J ,DIM) ) * 8.0 / 3.0               &
        &   - ( UV(I+2, J ,DIM) - UV(I+1, J ,DIM) ) * 1.0 / 3.0                 !\\\
      END IF

    ! D2U
      IF( ACTIVITY(I+2, J ) == NOACTIVE ) THEN
        D2U =                                       - DDU                       !
      ELSE
        D2U = ( UV(I+2, J ,DIM) - UV(I+1, J ,DIM) ) - DDU                       !\\\
      END IF

    ! FOR INFLOW BOUNDARY
      IF( CRU >= 0.0 ) THEN

      ! DCU
        DCU = DC_UU_AT_I0( J ,DIM)                                              !\\\

      ! CALCULATE ADVECTION
        QADV_XY_UV_U( I , J ,DIM) = - CRU * MU( I , J ) *   &
        ( (                         UU_AT_I0( J ,DIM)            &
          -                             D2U             / 24.0   &
          )   &
        )
      ! CALCULATE DIFFUSION
        QDIF_XY_UV_U( I , J ,DIM) = + DRU * MU( I , J ) *   &
        ( FU( I , J ,1,1) *   &
                                        DDU                      &
        + FU( I , J ,1,2) *   &
                                        DCU                      &
        )

    ! FOR OUTFLOW BOUNDARY
      ELSE

      ! DCU
        IF( CRU >= -1.0 ) THEN
          !BLOCK
            DCU = (              DC_UU_AT_I0( J ,DIM) ) * ( 1.0 + CRU )         &
            &   - ( VV(I+1, J ,DIM) - VV(I+1,J-1,DIM) ) *         CRU           !\\\
          !END BLOCK
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
        ( (                         UU_AT_I0( J ,DIM)            &
          -               CRU         * DDU             /  2.0   &
          - ( 1.0 - 4.0 * CRU * CRU ) * D2U             / 24.0   &
          )   &
        )
      ! CALCULATE DIFFUSION
        QDIF_XY_UV_U( I , J ,DIM) = + DRU * MU( I , J ) *   &
        ( FU( I , J ,1,1) *   &
          (                             DDU                      &
          -               CRU         * D2U             /  2.0   &
          )   &
        + FU( I , J ,1,2) *   &
                                        DCU                      &
        )

      END IF

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_U_AT_I0

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_U_AT_I1

    IMPLICIT NONE

    REAL   (CARD_KIND) :: CRU , DRU
    REAL   (CARD_KIND) :: DDU , D2U , DCU

    INTEGER(IJID_KIND) :: I

    I = 1

  ! AT IMPERMEABLE INTERNAL INTERFACE
    IF( ACTIVITY(I+1, J ) == NOACTIVE .OR. ACTIVITY( I , J ) == NOACTIVE ) THEN

      QADV_XY_UV_U( I , J ,DIM) = 0.0
      QDIF_XY_UV_U( I , J ,DIM) = 0.0

  ! AT FREE INTERNAL INTERFACE
    ELSE

      CRU = DT * RB * U( I , J )                                              !///
      DRU = DT * EKXY * VXYU( I , J )                                         !///

    ! DDU
      !BLOCK
        !BLOCK
          DDU = ( UV(I+1, J ,DIM) - UV( I , J ,DIM) )                         !\\\
        !END BLOCK
      !END BLOCK

    ! D2U
      IF( CRU >= 0.0 ) THEN
        !BLOCK
          D2U = ( UV(I+1, J ,DIM) - UV( I , J ,DIM) ) * 4.0 / 3.0             &
          &   - ( UV( I , J ,DIM) - UU_AT_I0( J ,DIM) ) * 8.0 / 3.0           !!!!!!
        !END BLOCK
      ELSE
        IF( ACTIVITY(I+2, J ) == NOACTIVE ) THEN
          D2U = ( UV( I , J ,DIM) - UV(I+1, J ,DIM) )                         !
        ELSE
          D2U = ( UV( I , J ,DIM) - UV(I+1, J ,DIM) )                         &
          &   - ( UV(I+1, J ,DIM) - UV(I+2, J ,DIM) )                         !\\\
        END IF
      END IF

    ! DCU
      IF( CRU >= -1.0 .AND. CRU <= +1.0 ) THEN
        !BLOCK
          DCU = ( VV(I+1, J ,DIM) - VV(I+1,J-1,DIM) ) * ( 1.0 - CRU ) / 2.0   &
          &   + ( VV( I , J ,DIM) - VV( I ,J-1,DIM) ) * ( 1.0 + CRU ) / 2.0   !\\\
        !END BLOCK
      ELSE IF( CRU > +1.0 ) THEN
        !BLOCK
          DCU = ( VV( I , J ,DIM) - VV( I ,J-1,DIM) ) * ( 2.0 - CRU )         &
          &   - (                DC_UU_AT_I0( J ,DIM) ) * ( 1.0 - CRU )       !!!!!!
        !END BLOCK
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
      ( FU( I , J ,1,1) *   &
        (                       DDU                           &
        -         CRU         * D2U                   / 2.0   &
        )   &
      + FU( I , J ,1,2) *   &
                                DCU                           &
      )

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_U_AT_I1

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_U_AT_II

    IMPLICIT NONE

    REAL   (CARD_KIND) :: CRU , DRU
    REAL   (CARD_KIND) :: DDU , D2U , DCU

    INTEGER(IJID_KIND) :: I

    DO I = 2 , NI-2

    ! AT IMPERMEABLE INTERNAL INTERFACE
      IF( ACTIVITY(I+1, J ) == NOACTIVE .OR. ACTIVITY( I , J ) == NOACTIVE ) THEN

        QADV_XY_UV_U( I , J ,DIM) = 0.0
        QDIF_XY_UV_U( I , J ,DIM) = 0.0

    ! AT FREE INTERNAL INTERFACE
      ELSE

        CRU = DT * RB * U( I , J )                                              !///
        DRU = DT * EKXY * VXYU( I , J )                                         !///

      ! DDU
        !BLOCK
          !BLOCK
            DDU = ( UV(I+1, J ,DIM) - UV( I , J ,DIM) )                         !\\\
          !END BLOCK
        !END BLOCK

      ! D2U
        IF( CRU >= 0.0 ) THEN
          IF( ACTIVITY(I-1, J ) == NOACTIVE ) THEN
            D2U = ( UV(I+1, J ,DIM) - UV( I , J ,DIM) )                         !
          ELSE
            D2U = ( UV(I+1, J ,DIM) - UV( I , J ,DIM) )                         &
            &   - ( UV( I , J ,DIM) - UV(I-1, J ,DIM) )                         !\\\
          END IF
        ELSE
          IF( ACTIVITY(I+2, J ) == NOACTIVE ) THEN
            D2U = ( UV( I , J ,DIM) - UV(I+1, J ,DIM) )                         !
          ELSE
            D2U = ( UV( I , J ,DIM) - UV(I+1, J ,DIM) )                         &
            &   - ( UV(I+1, J ,DIM) - UV(I+2, J ,DIM) )                         !\\\
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
        ( FU( I , J ,1,1) *   &
          (                       DDU                           &
          -         CRU         * D2U                   / 2.0   &
          )   &
        + FU( I , J ,1,2) *   &
                                  DCU                           &
        )

      END IF

    END DO

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_U_AT_II

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_U_AT_IM

    IMPLICIT NONE

    REAL   (CARD_KIND) :: CRU , DRU
    REAL   (CARD_KIND) :: DDU , D2U , DCU

    INTEGER(IJID_KIND) :: I

    I = NI-1

  ! AT IMPERMEABLE INTERNAL INTERFACE
    IF( ACTIVITY(I+1, J ) == NOACTIVE .OR. ACTIVITY( I , J ) == NOACTIVE ) THEN

      QADV_XY_UV_U( I , J ,DIM) = 0.0
      QDIF_XY_UV_U( I , J ,DIM) = 0.0

  ! AT FREE INTERNAL INTERFACE
    ELSE

      CRU = DT * RB * U( I , J )                                              !///
      DRU = DT * EKXY * VXYU( I , J )                                         !///

    ! DDU
      !BLOCK
        !BLOCK
          DDU = ( UV(I+1, J ,DIM) - UV( I , J ,DIM) )                         !\\\
        !END BLOCK
      !END BLOCK

    ! D2U
      IF( CRU >= 0.0 ) THEN
        IF( ACTIVITY(I-1, J ) == NOACTIVE ) THEN
          D2U = ( UV(I+1, J ,DIM) - UV( I , J ,DIM) )                         !
        ELSE
          D2U = ( UV(I+1, J ,DIM) - UV( I , J ,DIM) )                         &
          &   - ( UV( I , J ,DIM) - UV(I-1, J ,DIM) )                         !\\\
        END IF
      ELSE
        !BLOCK
          D2U = ( UV( I , J ,DIM) - UV(I+1, J ,DIM) )                         !!!!!!
        !END BLOCK
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
        !BLOCK
          DCU = ( VV(I+1, J ,DIM) - VV(I+1,J-1,DIM) )                         !!!!!!
        !END BLOCK
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
      ( FU( I , J ,1,1) *   &
        (                       DDU                           &
        -         CRU         * D2U                   / 2.0   &
        )   &
      + FU( I , J ,1,2) *   &
                                DCU                           &
      )

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_U_AT_IM

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_U_AT_IN

    IMPLICIT NONE

    REAL   (CARD_KIND) :: CRU , DRU
    REAL   (CARD_KIND) :: D2U , DCU

    INTEGER(IJID_KIND) :: I

    I = NI

  ! AT IMPERMEABLE BOUNDARY
    IF( ACTIVITY( I , J ) == NOACTIVE ) THEN

      QADV_XY_UV_U( I , J ,DIM) = 0.0
      QDIF_XY_UV_U( I , J ,DIM) = 0.0

  ! AT FREE BOUNDARY
    ELSE

      CRU = DT * RB * U( I , J )                                              !///
      DRU = DT * EKXY * VXYU( I , J )                                         !///

      IF( CRU >= 0.0 ) THEN

      ! D2U
        IF( ACTIVITY(I-1, J ) == NOACTIVE ) THEN
          D2U =  0.0                                                          !
        ELSE
          D2U = ( UV(I-1, J ,DIM) - UV( I , J ,DIM) )                         !\\\
        END IF

      ! CALCULATE ADVECTION
        IF( CRU <= 0.5 ) THEN
          QADV_XY_UV_U( I , J ,DIM) = - CRU * MU( I , J ) *   &
          ( (                       UV( I , J ,DIM)                &
            -                                         D2U / 24.0   &
            )   &
          )
        ELSE
          QADV_XY_UV_U( I , J ,DIM) = - CRU * MU( I , J ) *   &
          ( (                       UV( I , J ,DIM)                &
            + ( 5.0 - 12.0 * CRU + 8.0 * CRU * CRU) * D2U / 24.0   &
            )   &
          )   &
                                      +       MU( I , J ) *   &
                                                      D2U / 24.0
        END IF

      ! CALCULATE DIFFUSION
        IF( CRU <= 1.0 ) THEN
        ! DCU
          DCU = ( VV( I , J ,DIM) - VV( I ,J-1,DIM) )                         !\\\

          QDIF_XY_UV_U( I , J ,DIM) = + DRU * MU( I , J ) *   &
          ( FU( I , J ,1,2) *   &
                                                      DCU          &
          )
        ELSE
        ! DCU
          DCU = ( VV( I , J ,DIM) - VV( I ,J-1,DIM) ) * ( 3.0 - CRU ) / 2.0   &
          &   - ( VV(I-1, J ,DIM) - VV(I-1,J-1,DIM) ) * ( 1.0 - CRU ) / 2.0   !\\\

          QDIF_XY_UV_U( I , J ,DIM) = + DRU * MU( I , J ) *   &
          ( FU( I , J ,1,1) *   &
                                       ( 1.0 - CRU) * D2U /  2.0   &
          + FU( I , J ,1,2) *   &
                                                      DCU          &
          )
        END IF

      ELSE
        !BLOCK

        ! DCU
          DCU = ( VV( I , J ,DIM) - VV( I ,J-1,DIM) )                         !\\\

        ! CALCULATE ADVECTION
          QADV_XY_UV_U( I , J ,DIM) = - CRU * MU( I , J ) *   &
                                    UV( I , J ,DIM)
        ! CALCULATE DIFFUSION
          QDIF_XY_UV_U( I , J ,DIM) = + DRU * MU( I , J ) *   &
          ( FU( I , J ,1,2) *   &
                                                      DCU         &
          )

        !END BLOCK
      END IF

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_U_AT_IN

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_U( NI , NJ , SS , SV , RB , U , QADV_XY_SS_U , SCXY , DXYU , QDIF_XY_SS_U ,   &
  SU_AT_I0 , DC_SU_AT_I0 )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ) :: SS
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI,0:NJ) :: SV

    REAL   (PARD_KIND) , INTENT(IN ) :: RB , SCXY
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI,1:NJ) :: U , DXYU
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI,1:NJ) :: QADV_XY_SS_U , QDIF_XY_SS_U

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(     1:NJ) :: SU_AT_I0 , DC_SU_AT_I0

    INTEGER(IJID_KIND) :: J

    DO J = 1 , NJ

      CALL MR_CALC_QADV_N_QDIF_XY_SS_U_AT_I0
      CALL MR_CALC_QADV_N_QDIF_XY_SS_U_AT_I1
      CALL MR_CALC_QADV_N_QDIF_XY_SS_U_AT_II
      CALL MR_CALC_QADV_N_QDIF_XY_SS_U_AT_IM
      CALL MR_CALC_QADV_N_QDIF_XY_SS_U_AT_IN

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_U_AT_I0

    IMPLICIT NONE

    REAL   (CARD_KIND) :: CRU , DRU
    REAL   (CARD_KIND) :: DDU , D2U , DCU

    INTEGER(IJID_KIND) :: I

    I = 0

  ! AT IMPERMEABLE BOUNDARY
    IF( ACTIVITY(I+1, J ) == NOACTIVE ) THEN

      QADV_XY_SS_U( I , J ) = 0.0
      QDIF_XY_SS_U( I , J ) = 0.0

  ! AT FREE BOUNDARY
    ELSE

      CRU = DT * RB * U( I , J )                                        !///
      DRU = DT * SCXY * DXYU( I , J )                                   !///

    ! DDU
      IF( ACTIVITY(I+2, J ) == NOACTIVE ) THEN
        DDU = ( SS(I+1, J ) - SU_AT_I0( J ) ) * 8.0 / 3.0               !
      ELSE
        DDU = ( SS(I+1, J ) - SU_AT_I0( J ) ) * 8.0 / 3.0               &
        &   - ( SS(I+2, J ) - SS(I+1, J ) ) * 1.0 / 3.0                 !\\\
      END IF

    ! D2U
      IF( ACTIVITY(I+2, J ) == NOACTIVE ) THEN
        D2U =                               - DDU                       !
      ELSE
        D2U = ( SS(I+2, J ) - SS(I+1, J ) ) - DDU                       !\\\
      END IF

    ! FOR INFLOW BOUNDARY
      IF( CRU >= 0.0 ) THEN

      ! DCU
        DCU = DC_SU_AT_I0( J )                                          !\\\

      ! CALCULATE ADVECTION
        QADV_XY_SS_U( I , J ) = - CRU * MU( I , J ) *   &
        ( (                     SU_AT_I0( J )                &
          -                         D2U             / 24.0   &
          )   &
        )
      ! CALCULATE DIFFUSION
        QDIF_XY_SS_U( I , J ) = + DRU * MU( I , J ) *   &
        ( FU( I , J ,1,1) *   &
                                    DDU                      &
        + FU( I , J ,1,2) *   &
                                    DCU                      &
        )

    ! FOR OUTFLOW BOUNDARY
      ELSE

      ! DCU
        IF( CRU >= -1.0 ) THEN
          !BLOCK
            DCU = (          DC_SU_AT_I0( J ) ) * ( 1.0 + CRU )         &
            &   - ( SV(I+1, J ) - SV(I+1,J-1) ) *         CRU           !\\\
          !END BLOCK
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
        ( (                         SU_AT_I0( J )            &
          -               CRU         * DDU         /  2.0   &
          - ( 1.0 - 4.0 * CRU * CRU ) * D2U         / 24.0   &
          )   &
        )
      ! CALCULATE DIFFUSION
        QDIF_XY_SS_U( I , J ) = + DRU * MU( I , J ) *   &
        ( FU( I , J ,1,1) *   &
          (                             DDU                  &
          -               CRU         * D2U         /  2.0   &
          )   &
        + FU( I , J ,1,2) *   &
                                        DCU                  &
        )

      END IF

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_U_AT_I0

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_U_AT_I1

    IMPLICIT NONE

    REAL   (CARD_KIND) :: CRU , DRU
    REAL   (CARD_KIND) :: DDU , D2U , DCU

    INTEGER(IJID_KIND) :: I

    I = 1

  ! AT IMPERMEABLE INTERNAL INTERFACE
    IF( ACTIVITY(I+1, J ) == NOACTIVE .OR. ACTIVITY( I , J ) == NOACTIVE ) THEN

      QADV_XY_SS_U( I , J ) = 0.0
      QDIF_XY_SS_U( I , J ) = 0.0

  ! AT FREE INTERNAL INTERFACE
    ELSE

      CRU = DT * RB * U( I , J )                                      !///
      DRU = DT * SCXY * DXYU( I , J )                                 !///

    ! DDU
      !BLOCK
        !BLOCK
          DDU = ( SS(I+1, J ) - SS( I , J ) )                         !\\\
        !END BLOCK
      !END BLOCK

    ! D2U
      IF( CRU >= 0.0 ) THEN
        !BLOCK
          D2U = ( SS(I+1, J ) - SS( I , J ) ) * 4.0 / 3.0             &
          &   - ( SS(I+1, J ) - SU_AT_I0( J ) ) * 8.0 / 3.0           !!!!!!
        !END BLOCK
      ELSE
        IF( ACTIVITY(I+2, J ) == NOACTIVE ) THEN
          D2U = ( SS( I , J ) - SS(I+1, J ) )                         !
        ELSE
          D2U = ( SS( I , J ) - SS(I+1, J ) )                         &
          &   - ( SS(I+1, J ) - SS(I+2, J ) )                         !\\\
        END IF
      END IF

    ! DCU
      IF( CRU >= -1.0 .AND. CRU <= +1.0 ) THEN
        !BLOCK
          DCU = ( SV(I+1, J ) - SV(I+1,J-1) ) * ( 1.0 - CRU ) / 2.0   &
          &   + ( SV( I , J ) - SV( I ,J-1) ) * ( 1.0 + CRU ) / 2.0   !\\\
        !END BLOCK
      ELSE IF( CRU > +1.0 ) THEN
        !BLOCK
          DCU = ( SV( I , J ) - SV( I ,J-1) ) * ( 2.0 - CRU )         &
          &   - (            DC_SU_AT_I0( J ) ) * ( 1.0 - CRU )       !!!!!!
        !END BLOCK
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
      ( (       ( SS(I+1, J ) + SS( I , J ) )     / 2.0   &
        -         CRU         * DDU               / 2.0   &
        - ( 1.0 - CRU * CRU ) * D2U               / 6.0   &
        )   &
      )
    ! CALCULATE DIFFUSION
      QDIF_XY_SS_U( I , J ) = + DRU * MU( I , J ) *   &
      ( FU( I , J ,1,1) *   &
        (                       DDU                       &
        -         CRU         * D2U               / 2.0   &
        )   &
      + FU( I , J ,1,2) *   &
                                DCU                       &
      )

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_U_AT_I1

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_U_AT_II

    IMPLICIT NONE

    REAL   (CARD_KIND) :: CRU , DRU
    REAL   (CARD_KIND) :: DDU , D2U , DCU

    INTEGER(IJID_KIND) :: I

    DO I = 2 , NI-2

    ! AT IMPERMEABLE INTERNAL INTERFACE
      IF( ACTIVITY(I+1, J ) == NOACTIVE .OR. ACTIVITY( I , J ) == NOACTIVE ) THEN

        QADV_XY_SS_U( I , J ) = 0.0
        QDIF_XY_SS_U( I , J ) = 0.0

    ! AT FREE INTERNAL INTERFACE
      ELSE

        CRU = DT * RB * U( I , J )                                      !///
        DRU = DT * SCXY * DXYU( I , J )                                 !///

      ! DDU
        !BLOCK
          !BLOCK
            DDU = ( SS(I+1, J ) - SS( I , J ) )                         !\\\
          !END BLOCK
        !END BLOCK

      ! D2U
        IF( CRU >= 0.0 ) THEN
          IF( ACTIVITY(I-1, J ) == NOACTIVE ) THEN
            D2U = ( SS(I+1, J ) - SS( I , J ) )                         !
          ELSE
            D2U = ( SS(I+1, J ) - SS( I , J ) )                         &
            &   - ( SS( I , J ) - SS(I-1, J ) )                         !\\\
          END IF
        ELSE
          IF( ACTIVITY(I+2, J ) == NOACTIVE ) THEN
            D2U = ( SS( I , J ) - SS(I+1, J ) )                         !
          ELSE
            D2U = ( SS( I , J ) - SS(I+1, J ) )                         &
            &   - ( SS(I+1, J ) - SS(I+2, J ) )                         !\\\
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
        ( (       ( SS(I+1, J ) + SS( I , J ) )     / 2.0   &
          -         CRU         * DDU               / 2.0   &
          - ( 1.0 - CRU * CRU ) * D2U               / 6.0   &
          )   &
        )
      ! CALCULATE DIFFUSION
        QDIF_XY_SS_U( I , J ) = + DRU * MU( I , J ) *   &
        ( FU( I , J ,1,1) *   &
          (                       DDU                       &
          -         CRU         * D2U               / 2.0   &
          )   &
        + FU( I , J ,1,2) *   &
                                  DCU                       &
        )

      END IF

    END DO

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_U_AT_II

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_U_AT_IM

    IMPLICIT NONE

    REAL   (CARD_KIND) :: CRU , DRU
    REAL   (CARD_KIND) :: DDU , D2U , DCU

    INTEGER(IJID_KIND) :: I

    I = NI-1

  ! AT IMPERMEABLE INTERNAL INTERFACE
    IF( ACTIVITY(I+1, J ) == NOACTIVE .OR. ACTIVITY( I , J ) == NOACTIVE ) THEN

      QADV_XY_SS_U( I , J ) = 0.0
      QDIF_XY_SS_U( I , J ) = 0.0

  ! AT FREE INTERNAL INTERFACE
    ELSE

      CRU = DT * RB * U( I , J )                                      !///
      DRU = DT * SCXY * DXYU( I , J )                                 !///

    ! DDU
      !BLOCK
        !BLOCK
          DDU = ( SS(I+1, J ) - SS( I , J ) )                         !\\\
        !END BLOCK
      !END BLOCK

    ! D2U
      IF( CRU >= 0.0 ) THEN
        IF( ACTIVITY(I-1, J ) == NOACTIVE ) THEN
          D2U = ( SS(I+1, J ) - SS( I , J ) )                         !
        ELSE
          D2U = ( SS(I+1, J ) - SS( I , J ) )                         &
          &   - ( SS( I , J ) - SS(I-1, J ) )                         !\\\
        END IF
      ELSE
        !BLOCK
          D2U = ( SS( I , J ) - SS(I+1, J ) )                         !!!!!!
        !END BLOCK
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
        !BLOCK
          DCU = ( SV(I+1, J ) - SV(I+1,J-1) )                         !!!!!!
        !END BLOCK
      END IF

    ! CALCULATE ADVECTION
      QADV_XY_SS_U( I , J ) = - CRU * MU( I , J ) *   &
      ( (       ( SS(I+1, J ) + SS( I , J ) )     / 2.0   &
        -         CRU         * DDU               / 2.0   &
        - ( 1.0 - CRU * CRU ) * D2U               / 6.0   &
        )   &
      )
    ! CALCULATE DIFFUSION
      QDIF_XY_SS_U( I , J ) = + DRU * MU( I , J ) *   &
      ( FU( I , J ,1,1) *   &
        (                       DDU                       &
        -         CRU         * D2U               / 2.0   &
        )   &
      + FU( I , J ,1,2) *   &
                                DCU                       &
      )

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_U_AT_IM

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_U_AT_IN

    IMPLICIT NONE

    REAL   (CARD_KIND) :: CRU , DRU
    REAL   (CARD_KIND) :: D2U , DCU

    INTEGER(IJID_KIND) :: I

    I = NI

  ! AT IMPERMEABLE BOUNDARY
    IF( ACTIVITY( I , J ) == NOACTIVE ) THEN

      QADV_XY_SS_U( I , J ) = 0.0
      QDIF_XY_SS_U( I , J ) = 0.0

  ! AT FREE BOUNDARY
    ELSE

      CRU = DT * RB * U( I , J )                                      !///
      DRU = DT * SCXY * DXYU( I , J )                                 !///

      IF( CRU >= 0.0 ) THEN

      ! D2U
        IF( ACTIVITY(I-1, J ) == NOACTIVE ) THEN
          D2U =  0.0                                                  !
        ELSE
          D2U = ( SS(I-1, J ) - SS( I , J ) )                         !\\\
        END IF

      ! CALCULATE ADVECTION
        IF( CRU <= 0.5 ) THEN
          QADV_XY_SS_U( I , J ) = - CRU * MU( I , J ) *   &
          ( (                       SS( I , J )                &
            -                                         D2U / 24.0   &
            )   &
          )
        ELSE
          QADV_XY_SS_U( I , J ) = - CRU * MU( I , J ) *   &
          ( (                       SS( I , J )                &
            + ( 5.0 - 12.0 * CRU + 8.0 * CRU * CRU) * D2U / 24.0   &
            )   &
          )   &
                                      +   MU( I , J ) *   &
                                                      D2U / 24.0
        END IF

      ! CALCULATE DIFFUSION
        IF( CRU <= 1.0 ) THEN
        ! DCU
          DCU = ( SV( I , J ) - SV( I ,J-1) )                         !\\\

          QDIF_XY_SS_U( I , J ) = + DRU * MU( I , J ) *   &
          ( FU( I , J ,1,2) *   &
                                                      DCU          &
          )
        ELSE
        ! DCU
          DCU = ( SV( I , J ) - SV( I ,J-1) ) * ( 3.0 - CRU ) / 2.0   &
          &   - ( SV(I-1, J ) - SV(I-1,J-1) ) * ( 1.0 - CRU ) / 2.0   !\\\

          QDIF_XY_SS_U( I , J ) = + DRU * MU( I , J ) *   &
          ( FU( I , J ,1,1) *   &
                                       ( 1.0 - CRU) * D2U /  2.0   &
          + FU( I , J ,1,2) *   &
                                                      DCU          &
          )
        END IF

      ELSE
        !BLOCK

        ! DCU
          DCU = ( SV( I , J ) - SV( I ,J-1) )                         !\\\

        ! CALCULATE ADVECTION
          QADV_XY_SS_U( I , J ) = - CRU * MU( I , J ) *   &
                                    SS( I , J )
        ! CALCULATE DIFFUSION
          QDIF_XY_SS_U( I , J ) = + DRU * MU( I , J ) *   &
          ( FU( I , J ,1,2) *   &
                                                      DCU          &
          )

        !END BLOCK
      END IF

    END IF

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_U_AT_IN

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

    USE MR_MOD_INTERP_XY

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ,1:2) :: UV
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI,1:NJ,1:2) :: UU

    REAL   (PARD_KIND) , INTENT(IN ) :: RB , EKXY
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI,0:NJ    ) :: V , VXYV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI,0:NJ,1:2) :: QADV_XY_UV_V , QDIF_XY_UV_V

    INTEGER :: DIM

    DO DIM = 1 , 2

      CALL MR_CALC_QADV_N_QDIF_XY_UV_V_AT_J0
      CALL MR_CALC_QADV_N_QDIF_XY_UV_V_AT_J1
      CALL MR_CALC_QADV_N_QDIF_XY_UV_V_AT_JJ
      CALL MR_CALC_QADV_N_QDIF_XY_UV_V_AT_JM
      CALL MR_CALC_QADV_N_QDIF_XY_UV_V_AT_JN

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_V_AT_J0

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    J = 0

    DO I = 1 , NI

      QADV_XY_UV_V( I , J ,DIM) = 0.0
      QDIF_XY_UV_V( I , J ,DIM) = 0.0

    END DO

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_V_AT_J0

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_V_AT_J1

    IMPLICIT NONE

    REAL   (CARD_KIND) :: CRV , DRV
    REAL   (CARD_KIND) :: DDV , D2V , DCV

    INTEGER(IJID_KIND) :: I , J

    J = 1

    DO I = 1 , NI

    ! AT IMPERMEABLE INTERNAL INTERFACE
      IF( ACTIVITY( I , J ) == NOACTIVE .OR. ACTIVITY( I ,J+1) == NOACTIVE ) THEN

        QADV_XY_UV_V( I , J ,DIM) = 0.0
        QDIF_XY_UV_V( I , J ,DIM) = 0.0

    ! AT FREE INTERNAL INTERFACE
      ELSE

        CRV = DT * RB * V( I , J )                                              !///
        DRV = DT * EKXY * VXYV( I , J )                                         !///

      ! DDV
        !BLOCK
          !BLOCK
            DDV = ( UV( I ,J+1,DIM) - UV( I , J ,DIM) )                         !\\\
          !END BLOCK
        !END BLOCK

      ! DD2V
        IF( CRV >= 0.0 ) THEN
          !BLOCK
            D2V = ( UV( I ,J+1,DIM) - UV( I , J ,DIM) )                         !!!!!!
          !END BLOCK
        ELSE
          IF( ACTIVITY( I ,J+2) == NOACTIVE ) THEN
            D2V = ( UV( I , J ,DIM) - UV( I ,J+1,DIM) )                         !
          ELSE
            D2V = ( UV( I , J ,DIM) - UV( I ,J+1,DIM) )                         &
            &   - ( UV( I ,J+1,DIM) - UV( I ,J+2,DIM) )                         !\\\
          END IF
        END IF

      ! DCV
        IF( CRV >= -1.0 .AND. CRV <= +1.0 ) THEN
          !BLOCK
            DCV = ( UU( I ,J+1,DIM) - UU(I-1,J+1,DIM) ) * ( 1.0 - CRV ) / 2.0   &
            &   + ( UU( I , J ,DIM) - UU(I-1, J ,DIM) ) * ( 1.0 + CRV ) / 2.0   !\\\
          !END BLOCK
        ELSE IF( CRU > +1.0 ) THEN
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
        ( FV( I , J ,2,2) *   &
          (                       DDV                           &
          -         CRV         * D2V                   / 2.0   &
          )   &
        + FV( I , J ,2,1) *   &
                                  DCV                           &
        )

      END IF

    END DO

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_V_AT_J1

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_V_AT_JJ

    IMPLICIT NONE

    REAL   (CARD_KIND) :: CRV , DRV
    REAL   (CARD_KIND) :: DDV , D2V , DCV

    INTEGER(IJID_KIND) :: I , J

    DO J = 2 , NJ-2

      DO I = 1 , NI

      ! AT IMPERMEABLE INTERNAL INTERFACE
        IF( ACTIVITY( I , J ) == NOACTIVE .OR. ACTIVITY( I ,J+1) == NOACTIVE ) THEN

          QADV_XY_UV_V( I , J ,DIM) = 0.0
          QDIF_XY_UV_V( I , J ,DIM) = 0.0

      ! AT FREE INTERNAL INTERFACE
        ELSE

          CRV = DT * RB * V( I , J )                                              !///
          DRV = DT * EKXY * VXYV( I , J )                                         !///

        ! DDV
          !BLOCK
            !BLOCK
              DDV = ( UV( I ,J+1,DIM) - UV( I , J ,DIM) )                         !\\\
            !END BLOCK
          !END BLOCK

        ! DD2V
          IF( CRV >= 0.0 ) THEN
            IF( ACTIVITY( I ,J-1) == NOACTIVE ) THEN
              D2V = ( UV( I ,J+1,DIM) - UV( I , J ,DIM) )                         !
            ELSE
              D2V = ( UV( I ,J+1,DIM) - UV( I , J ,DIM) )                         &
              &   - ( UV( I , J ,DIM) - UV( I ,J-1,DIM) )                         !\\\
            END IF
          ELSE
            IF( ACTIVITY( I ,J+2) == NOACTIVE ) THEN
              D2V = ( UV( I , J ,DIM) - UV( I ,J+1,DIM) )                         !
            ELSE
              D2V = ( UV( I , J ,DIM) - UV( I ,J+1,DIM) )                         &
              &   - ( UV( I ,J+1,DIM) - UV( I ,J+2,DIM) )                         !\\\
            END IF
          END IF

        ! DCV
          IF( CRV >= -1.0 .AND. CRV <= +1.0 ) THEN
            !BLOCK
              DCV = ( UU( I ,J+1,DIM) - UU(I-1,J+1,DIM) ) * ( 1.0 - CRV ) / 2.0   &
              &   + ( UU( I , J ,DIM) - UU(I-1, J ,DIM) ) * ( 1.0 + CRV ) / 2.0   !\\\
            !END BLOCK
          ELSE IF( CRU > +1.0 ) THEN
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
          ( FV( I , J ,2,2) *   &
            (                       DDV                           &
            -         CRV         * D2V                   / 2.0   &
            )   &
          + FV( I , J ,2,1) *   &
                                    DCV                           &
          )

        END IF

      END DO

    END DO

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_V_AT_JJ

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_V_AT_JM

    IMPLICIT NONE

    REAL   (CARD_KIND) :: CRV , DRV
    REAL   (CARD_KIND) :: DDV , D2V , DCV

    INTEGER(IJID_KIND) :: I , J

    J = NJ-1

    DO I = 1 , NI

    ! AT IMPERMEABLE INTERNAL INTERFACE
      IF( ACTIVITY( I , J ) == NOACTIVE .OR. ACTIVITY( I ,J+1) == NOACTIVE ) THEN

        QADV_XY_UV_V( I , J ,DIM) = 0.0
        QDIF_XY_UV_V( I , J ,DIM) = 0.0

    ! AT FREE INTERNAL INTERFACE
      ELSE

        CRV = DT * RB * V( I , J )                                              !///
        DRV = DT * EKXY * VXYV( I , J )                                         !///

      ! DDV
        !BLOCK
          !BLOCK
            DDV = ( UV( I ,J+1,DIM) - UV( I , J ,DIM) )                         !\\\
          !END BLOCK
        !END BLOCK

      ! DD2V
        IF( CRV >= 0.0 ) THEN
          IF( ACTIVITY( I ,J-1) == NOACTIVE ) THEN
            D2V = ( UV( I ,J+1,DIM) - UV( I , J ,DIM) )                         !
          ELSE
            D2V = ( UV( I ,J+1,DIM) - UV( I , J ,DIM) )                         &
            &   - ( UV( I , J ,DIM) - UV( I ,J-1,DIM) )                         !\\\
          END IF
        ELSE
          !BLOCK
            D2V = ( UV( I , J ,DIM) - UV( I ,J+1,DIM) )                         !!!!!!
          !END BLOCK
        END IF

      ! DCV
        IF( CRV >= -1.0 .AND. CRV <= +1.0 ) THEN
          !BLOCK
            DCV = ( UU( I ,J+1,DIM) - UU(I-1,J+1,DIM) ) * ( 1.0 - CRV ) / 2.0   &
            &   + ( UU( I , J ,DIM) - UU(I-1, J ,DIM) ) * ( 1.0 + CRV ) / 2.0   !\\\
          !END BLOCK
        ELSE IF( CRU > +1.0 ) THEN
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
        ( FV( I , J ,2,2) *   &
          (                       DDV                           &
          -         CRV         * D2V                   / 2.0   &
          )   &
        + FV( I , J ,2,1) *   &
                                  DCV                           &
        )

      END IF

    END DO

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_V_AT_JM

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_V_AT_JN

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    J = NJ

    DO I = 1 , NI

      QADV_XY_UV_V( I , J ,DIM) = 0.0
      QDIF_XY_UV_V( I , J ,DIM) = 0.0

    END DO

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_UV_V_AT_JN

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

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ) :: SS
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI,1:NJ) :: SU

    REAL   (PARD_KIND) , INTENT(IN ) :: RB , SCXY
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI,0:NJ) :: V , DXYV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI,0:NJ) :: QADV_XY_SS_V , QDIF_XY_SS_V

    CALL MR_CALC_QADV_N_QDIF_XY_SS_V_AT_J0
    CALL MR_CALC_QADV_N_QDIF_XY_SS_V_AT_J1
    CALL MR_CALC_QADV_N_QDIF_XY_SS_V_AT_JJ
    CALL MR_CALC_QADV_N_QDIF_XY_SS_V_AT_JM
    CALL MR_CALC_QADV_N_QDIF_XY_SS_V_AT_JN

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_V_AT_J0

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    J = 0

    DO I = 1 , NI

      QADV_XY_SS_V( I , J ) = 0.0
      QDIF_XY_SS_V( I , J ) = 0.0

    END DO

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_V_AT_J0

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_V_AT_J1

    IMPLICIT NONE

    REAL   (CARD_KIND) :: CRV , DRV
    REAL   (CARD_KIND) :: DDV , D2V , DCV

    INTEGER(IJID_KIND) :: I , J

    J = 1

    DO I = 1 , NI

    ! AT IMPERMEABLE BOUNDARY
      IF( ACTIVITY( I , J ) == NOACTIVE .OR. ACTIVITY( I ,J+1) == NOACTIVE ) THEN

        QADV_XY_SS_V( I , J ) = 0.0
        QDIF_XY_SS_V( I , J ) = 0.0

    ! AT FREE INTERFACE
      ELSE

        CRV = DT * RB * V( I , J )                                      !///
        DRV = DT * SCXY * DXYV( I , J )                                 !///

      ! DDV
        !BLOCK
          !BLOCK
            DDV = ( SS( I ,J+1) - SS( I , J ) )                         !\\\
          !END BLOCK
        !END BLOCK

      ! DD2V
        IF( CRV >= 0.0 ) THEN
          !BLOCK
            D2V = ( SS( I ,J+1) - SS( I , J ) )                         !!!!!!
          !END BLOCK
        ELSE
          IF( ACTIVITY( I ,J+2) == NOACTIVE ) THEN
            D2V = ( SS( I , J ) - SS( I ,J+1) )                         !
          ELSE
            D2V = ( SS( I , J ) - SS( I ,J+1) )                         &
            &   - ( SS( I ,J+1) - SS( I ,J+2) )                         !\\\
          END IF
        END IF

      ! DCV
        IF( CRV >= -1.0 .AND. CRV <= +1.0 ) THEN
          !BLOCK
            DCV = ( SU( I ,J+1) - SU(I-1,J+1) ) * ( 1.0 - CRV ) / 2.0   &
            &   + ( SU( I , J ) - SU(I-1, J ) ) * ( 1.0 + CRV ) / 2.0   !\\\
          !END BLOCK
        ELSE IF( CRU > +1.0 ) THEN
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
        ( FV( I , J ,2,2) *   &
          (                       DDV                       &
          -         CRV         * D2V               / 2.0   &
          )   &
        + FV( I , J ,2,1) *   &
                                  DCV                       &
        )

      END IF

    END DO

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_V_AT_J1

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_V_AT_JJ

    IMPLICIT NONE

    REAL   (CARD_KIND) :: CRV , DRV
    REAL   (CARD_KIND) :: DDV , D2V , DCV

    INTEGER(IJID_KIND) :: I , J

    DO J = 2 , NJ-2

      DO I = 1 , NI

      ! AT IMPERMEABLE BOUNDARY
        IF( ACTIVITY( I , J ) == NOACTIVE .OR. ACTIVITY( I ,J+1) == NOACTIVE ) THEN

          QADV_XY_SS_V( I , J ) = 0.0
          QDIF_XY_SS_V( I , J ) = 0.0

      ! AT FREE INTERFACE
        ELSE

          CRV = DT * RB * V( I , J )                                      !///
          DRV = DT * SCXY * DXYV( I , J )                                 !///

        ! DDV
          !BLOCK
            !BLOCK
              DDV = ( SS( I ,J+1) - SS( I , J ) )                         !\\\
            !END BLOCK
          !END BLOCK

        ! DD2V
          IF( CRV >= 0.0 ) THEN
            IF( ACTIVITY( I ,J-1) == NOACTIVE ) THEN
              D2V = ( SS( I ,J+1) - SS( I , J ) )                         !
            ELSE
              D2V = ( SS( I ,J+1) - SS( I , J ) )                         &
              &   - ( SS( I , J ) - SS( I ,J-1) )                         !\\\
            END IF
          ELSE
            IF( ACTIVITY( I ,J+2) == NOACTIVE ) THEN
              D2V = ( SS( I , J ) - SS( I ,J+1) )                         !
            ELSE
              D2V = ( SS( I , J ) - SS( I ,J+1) )                         &
              &   - ( SS( I ,J+1) - SS( I ,J+2) )                         !\\\
            END IF
          END IF

        ! DCV
          IF( CRV >= -1.0 .AND. CRV <= +1.0 ) THEN
            !BLOCK
              DCV = ( SU( I ,J+1) - SU(I-1,J+1) ) * ( 1.0 - CRV ) / 2.0   &
              &   + ( SU( I , J ) - SU(I-1, J ) ) * ( 1.0 + CRV ) / 2.0   !\\\
            !END BLOCK
          ELSE IF( CRU > +1.0 ) THEN
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
          ( FV( I , J ,2,2) *   &
            (                       DDV                       &
            -         CRV         * D2V               / 2.0   &
            )   &
          + FV( I , J ,2,1) *   &
                                    DCV                       &
          )

        END IF

      END DO

    END DO

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_V_AT_JJ

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_V_AT_JM

    IMPLICIT NONE

    REAL   (CARD_KIND) :: CRV , DRV
    REAL   (CARD_KIND) :: DDV , D2V , DCV

    INTEGER(IJID_KIND) :: I , J

    J = NJ-1

    DO I = 1 , NI

    ! AT IMPERMEABLE BOUNDARY
      IF( ACTIVITY( I , J ) == NOACTIVE .OR. ACTIVITY( I ,J+1) == NOACTIVE ) THEN

        QADV_XY_SS_V( I , J ) = 0.0
        QDIF_XY_SS_V( I , J ) = 0.0

    ! AT FREE INTERFACE
      ELSE

        CRV = DT * RB * V( I , J )                                      !///
        DRV = DT * SCXY * DXYV( I , J )                                 !///

      ! DDV
        !BLOCK
          !BLOCK
            DDV = ( SS( I ,J+1) - SS( I , J ) )                         !\\\
          !END BLOCK
        !END BLOCK

      ! DD2V
        IF( CRV >= 0.0 ) THEN
          IF( ACTIVITY( I ,J-1) == NOACTIVE ) THEN
            D2V = ( SS( I ,J+1) - SS( I , J ) )                         !
          ELSE
            D2V = ( SS( I ,J+1) - SS( I , J ) )                         &
            &   - ( SS( I , J ) - SS( I ,J-1) )                         !\\\
          END IF
        ELSE
          !BLOCK
            D2V = ( SS( I , J ) - SS( I ,J+1) )                         !!!!!!
          !END BLOCK
        END IF

      ! DCV
        IF( CRV >= -1.0 .AND. CRV <= +1.0 ) THEN
          !BLOCK
            DCV = ( SU( I ,J+1) - SU(I-1,J+1) ) * ( 1.0 - CRV ) / 2.0   &
            &   + ( SU( I , J ) - SU(I-1, J ) ) * ( 1.0 + CRV ) / 2.0   !\\\
          !END BLOCK
        ELSE IF( CRU > +1.0 ) THEN
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
        ( FV( I , J ,2,2) *   &
          (                       DDV                       &
          -         CRV         * D2V               / 2.0   &
          )   &
        + FV( I , J ,2,1) *   &
                                  DCV                       &
        )

      END IF

    END DO

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_V_AT_JM

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
  SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_V_AT_JN

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    J = NJ

    DO I = 1 , NI

      QADV_XY_SS_V( I , J ) = 0.0
      QDIF_XY_SS_V( I , J ) = 0.0

    END DO

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_V_AT_JN

  END SUBROUTINE MR_CALC_QADV_N_QDIF_XY_SS_V

  END MODULE MR_MOD_CALC_QADV_N_QDIF_XY
