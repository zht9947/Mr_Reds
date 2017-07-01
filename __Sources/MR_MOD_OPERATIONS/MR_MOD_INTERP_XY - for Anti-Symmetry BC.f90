#INCLUDE 'MR_H_ALIGN_PADDING.H'
!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE) MOD_INTERP_XY
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
  MODULE MR_MOD_INTERP_XY

    USE MR_KINDS

    USE MR_DEF_ACTIVITY

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_INTERP_XY_UV_U_BY_RCPRAC
    PUBLIC :: MR_INTERP_XY_UV_U_BY_LINEAR
    PUBLIC :: MR_INTERP_XY_UV_U_TO_GET_V_AT_U
    PUBLIC :: MR_INTERP_XY_SS_U
    PUBLIC :: MR_INTERP_XY_ZS_U
    PUBLIC :: MR_INTERP_XY_UV_O_U
    PUBLIC :: MR_INTERP_XY_SS_O_U
    PUBLIC :: MR_INTERP_XY_ZS_O_U
    PUBLIC :: MR_INV_INTERP_XY_UV_FROM_UU
    PUBLIC :: MR_INV_INTERP_XY_SS_FROM_SU

    PUBLIC :: MR_INTERP_XY_UV_V_BY_RCPRAC
    PUBLIC :: MR_INTERP_XY_UV_V_BY_LINEAR
    PUBLIC :: MR_INTERP_XY_UV_V_TO_GET_U_AT_V
    PUBLIC :: MR_INTERP_XY_SS_V
    PUBLIC :: MR_INTERP_XY_ZS_V
    PUBLIC :: MR_INTERP_XY_UV_O_V
    PUBLIC :: MR_INTERP_XY_SS_O_V
    PUBLIC :: MR_INTERP_XY_ZS_O_V
    PUBLIC :: MR_INV_INTERP_XY_UV_FROM_VV
    PUBLIC :: MR_INV_INTERP_XY_SS_FROM_SV

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
  SUBROUTINE MR_INTERP_XY_UV_U_BY_RCPRAC( NI , NJ , ZS , ALFA , BETA , U )

    USE MR_DEF_CURVED_GEOS , ONLY : MU
    USE MR_DEF_FIELD_VARS , ONLY : HU

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ    ) :: ZS
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND),1:NJ,1:2) :: ALFA , BETA

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ    ) :: U

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ

      I = 0
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_U_BY_RCPRAC_I0_JJ
      !END I = 0

     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI-1
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_U_BY_RCPRAC_II_JJ
      END DO

      I = NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_U_BY_RCPRAC_IN_JJ
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
!   2015-06-10    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_INTERP_XY_UV_U_BY_RCPRAC_I0_JJ

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ACTIVITY(I+1, J ) == BEACTIVE .AND. ACTIVITY( P , Q ) == BEACTIVE ) THEN
      U( I , J ) = 0.5 * ( ( ALFA(I+1, J ,1) + ALFA( P , Q ,1) ) * ( ZS(I+1, J ) - ZS( P , Q ) )   &
      + ( BETA(I+1, J ,1) + BETA( P , Q ,1) )   &
      ) / MU( I , J ) / HU( I , J )
    ELSE
      U( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_U_BY_RCPRAC_I0_JJ

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
  SUBROUTINE MR_INTERP_XY_UV_U_BY_RCPRAC_II_JJ

    IMPLICIT NONE

    IF( ACTIVITY(I+1, J ) == BEACTIVE .AND. ACTIVITY( I , J ) == BEACTIVE ) THEN
      U( I , J ) = 0.5 * ( ( ALFA(I+1, J ,1) + ALFA( I , J ,1) ) * ( ZS(I+1, J ) - ZS( I , J ) )   &
      + ( BETA(I+1, J ,1) + BETA( I , J ,1) )   &
      ) / MU( I , J ) / HU( I , J )
    ELSE
      U( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_U_BY_RCPRAC_II_JJ

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
  SUBROUTINE MR_INTERP_XY_UV_U_BY_RCPRAC_IN_JJ

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ACTIVITY(P+1, Q ) == BEACTIVE .AND. ACTIVITY( I , J ) == BEACTIVE ) THEN
      U( I , J ) = 0.5 * ( ( ALFA(P+1, Q ,1) + ALFA( I , J ,1) ) * ( ZS(P+1, Q ) - ZS( I , J ) )   &
      + ( BETA(P+1, Q ,1) + BETA( I , J ,1) )   &
      ) / MU( I , J ) / HU( I , J )
    ELSE
      U( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_U_BY_RCPRAC_IN_JJ

  END SUBROUTINE MR_INTERP_XY_UV_U_BY_RCPRAC

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
  SUBROUTINE MR_INTERP_XY_UV_U_BY_LINEAR( NI , NJ , UV , U )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: UV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ    ) :: U

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ

      I = 0
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_U_BY_LINEAR_I0_JJ
      !END I = 0

     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI-1
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_U_BY_LINEAR_II_JJ
      END DO

      I = NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_U_BY_LINEAR_IN_JJ
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
!   2015-06-10    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_INTERP_XY_UV_U_BY_LINEAR_I0_JJ

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ACTIVITY(I+1, J ) == BEACTIVE .AND. ACTIVITY( P , Q ) == BEACTIVE ) THEN
      U( I , J ) = 0.5 * ( UV(I+1, J ,1) + UV( P , Q ,1) )
    ELSE
      U( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_U_BY_LINEAR_I0_JJ

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
  SUBROUTINE MR_INTERP_XY_UV_U_BY_LINEAR_II_JJ

    IMPLICIT NONE

    IF( ACTIVITY(I+1, J ) == BEACTIVE .AND. ACTIVITY( I , J ) == BEACTIVE ) THEN
      U( I , J ) = 0.5 * ( UV(I+1, J ,1) + UV( I , J ,1) )
    ELSE
      U( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_U_BY_LINEAR_II_JJ

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
  SUBROUTINE MR_INTERP_XY_UV_U_BY_LINEAR_IN_JJ

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ACTIVITY(P+1, Q ) == BEACTIVE .AND. ACTIVITY( I , J ) == BEACTIVE ) THEN
      U( I , J ) = 0.5 * ( UV(P+1, Q ,1) + UV( I , J ,1) )
    ELSE
      U( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_U_BY_LINEAR_IN_JJ

  END SUBROUTINE MR_INTERP_XY_UV_U_BY_LINEAR

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
  SUBROUTINE MR_INTERP_XY_UV_U_TO_GET_V_AT_U( NI , NJ , UV , V_AT_U )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: UV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ    ) :: V_AT_U

    REAL   (FDRD_KIND) , PARAMETER   , DIMENSION(                         1:2) :: FACTOR = (/+1.0,-1.0/)

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ

      I = 0
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_U_TO_GET_V_AT_U_I0_JJ
      !END I = 0

     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI-1
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_U_TO_GET_V_AT_U_II_JJ
      END DO

      I = NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_U_TO_GET_V_AT_U_IN_JJ
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
!   2015-06-10    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_INTERP_XY_UV_U_TO_GET_V_AT_U_I0_JJ

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ACTIVITY(I+1, J ) == BEACTIVE .OR. ACTIVITY( P , Q ) == BEACTIVE ) THEN
      V_AT_U( I , J ) =   &
      ( UV(I+1, J ,2) * ACTIVITY(I+1, J )   &
      + UV( P , Q ,2) * ACTIVITY( P , Q ) * FACTOR(2) )   &
      / ( ACTIVITY(I+1, J ) + ACTIVITY( P , Q ) )
    ELSE
      V_AT_U( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_U_TO_GET_V_AT_U_I0_JJ

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
  SUBROUTINE MR_INTERP_XY_UV_U_TO_GET_V_AT_U_II_JJ

    IMPLICIT NONE

    IF( ACTIVITY(I+1, J ) == BEACTIVE .OR. ACTIVITY( I , J ) == BEACTIVE ) THEN
      V_AT_U( I , J ) =   &
      ( UV(I+1, J ,2) * ACTIVITY(I+1, J )   &
      + UV( I , J ,2) * ACTIVITY( I , J ) )   &
      / ( ACTIVITY(I+1, J ) + ACTIVITY( I , J ) )
    ELSE
      V_AT_U( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_U_TO_GET_V_AT_U_II_JJ

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
  SUBROUTINE MR_INTERP_XY_UV_U_TO_GET_V_AT_U_IN_JJ

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ACTIVITY(P+1, Q ) == BEACTIVE .OR. ACTIVITY( I , J ) == BEACTIVE ) THEN
      V_AT_U( I , J ) =   &
      ( UV(P+1, Q ,2) * ACTIVITY(P+1, Q ) * FACTOR(2)   &
      + UV( I , J ,2) * ACTIVITY( I , J ) )   &
      / ( ACTIVITY(P+1, Q ) + ACTIVITY( I , J ) )
    ELSE
      V_AT_U( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_U_TO_GET_V_AT_U_IN_JJ

  END SUBROUTINE MR_INTERP_XY_UV_U_TO_GET_V_AT_U

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
  SUBROUTINE MR_INTERP_XY_SS_U( NI , NJ , SS , SU )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) :: SS
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ) :: SU

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ

      I = 0
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_SS_U_I0_JJ
      !END I = 0

     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI-1
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_SS_U_II_JJ
      END DO

      I = NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_SS_U_IN_JJ
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
!   2015-06-10    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_INTERP_XY_SS_U_I0_JJ

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ACTIVITY(I+1, J ) == BEACTIVE .OR. ACTIVITY( P , Q ) == BEACTIVE ) THEN
      SU( I , J ) =   &
      ( SS(I+1, J ) * ACTIVITY(I+1, J )   &
      + SS( P , Q ) * ACTIVITY( P , Q ) )   &
      / ( ACTIVITY(I+1, J ) + ACTIVITY( P , Q ) )
    ELSE
      SU( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_SS_U_I0_JJ

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
  SUBROUTINE MR_INTERP_XY_SS_U_II_JJ

    IMPLICIT NONE

    IF( ACTIVITY(I+1, J ) == BEACTIVE .OR. ACTIVITY( I , J ) == BEACTIVE ) THEN
      SU( I , J ) =   &
      ( SS(I+1, J ) * ACTIVITY(I+1, J )   &
      + SS( I , J ) * ACTIVITY( I , J ) )   &
      / ( ACTIVITY(I+1, J ) + ACTIVITY( I , J ) )
    ELSE
      SU( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_SS_U_II_JJ

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
  SUBROUTINE MR_INTERP_XY_SS_U_IN_JJ

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ACTIVITY(P+1, Q ) == BEACTIVE .OR. ACTIVITY( I , J ) == BEACTIVE ) THEN
      SU( I , J ) =   &
      ( SS(P+1, Q ) * ACTIVITY(P+1, Q )   &
      + SS( I , J ) * ACTIVITY( I , J ) )   &
      / ( ACTIVITY(P+1, Q ) + ACTIVITY( I , J ) )
    ELSE
      SU( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_SS_U_IN_JJ

  END SUBROUTINE MR_INTERP_XY_SS_U

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
  SUBROUTINE MR_INTERP_XY_ZS_U( NI , NJ , ZS , ZSU )

    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_SLOPE

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) :: ZS
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ) :: ZSU

    REAL   (FDRD_KIND) :: SRATIO

    INTEGER(IJID_KIND) :: I , J

    SRATIO = XYR/ZR / ALPAR * SLOPE

    DO J = 1 , NJ

      I = 0
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_ZS_U_I0_JJ
      !END I = 0

     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI-1
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_ZS_U_II_JJ
      END DO

      I = NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_ZS_U_IN_JJ
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
!   2015-06-10    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_INTERP_XY_ZS_U_I0_JJ

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ACTIVITY(I+1, J ) == BEACTIVE .OR. ACTIVITY( P , Q ) == BEACTIVE ) THEN
      ZSU( I , J ) =   &
      ( ( ZS(I+1, J ) - 0.5 * JUV(I+1, J ,1,1) * SRATIO ) * ACTIVITY(I+1, J )   &
      + ( ZS( P , Q ) + 0.5 * JUV( P , Q ,1,1) * SRATIO ) * ACTIVITY( P , Q ) )   &
      / ( ACTIVITY(I+1, J ) + ACTIVITY( P , Q ) )
    ELSE
      ZSU( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_ZS_U_I0_JJ

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
  SUBROUTINE MR_INTERP_XY_ZS_U_II_JJ

    IMPLICIT NONE

    IF( ACTIVITY(I+1, J ) == BEACTIVE .OR. ACTIVITY( I , J ) == BEACTIVE ) THEN
      ZSU( I , J ) =   &
      ( ( ZS(I+1, J ) - 0.5 * JUV(I+1, J ,1,1) * SRATIO ) * ACTIVITY(I+1, J )   &
      + ( ZS( I , J ) + 0.5 * JUV( I , J ,1,1) * SRATIO ) * ACTIVITY( I , J ) )   &
      / ( ACTIVITY(I+1, J ) + ACTIVITY( I , J ) )
    ELSE
      ZSU( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_ZS_U_II_JJ

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
  SUBROUTINE MR_INTERP_XY_ZS_U_IN_JJ

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ACTIVITY(P+1, Q ) == BEACTIVE .OR. ACTIVITY( I , J ) == BEACTIVE ) THEN
      ZSU( I , J ) =   &
      ( ( ZS(P+1, Q ) - 0.5 * JUV(P+1, Q ,1,1) * SRATIO ) * ACTIVITY(P+1, Q )   &
      + ( ZS( I , J ) + 0.5 * JUV( I , J ,1,1) * SRATIO ) * ACTIVITY( I , J ) )   &
      / ( ACTIVITY(P+1, Q ) + ACTIVITY( I , J ) )
    ELSE
      ZSU( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_ZS_U_IN_JJ

  END SUBROUTINE MR_INTERP_XY_ZS_U

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
  SUBROUTINE MR_INTERP_XY_UV_O_U( NI , NJ , U , UO )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ    ) :: U
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ    ) :: UO

    INTEGER(IJID_KIND) :: I , J

    J = 0
      I = 0
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_O_U_I0_J0
      !END I = 0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI-1
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_O_U_II_J0
      END DO
      I = NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_O_U_IN_J0
      !END I = NI
    !END J = 0

    DO J = 1 , NJ-1
      I = 0
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_O_U_I0_JJ
      !END I = 0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI-1
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_O_U_II_JJ
      END DO
      I = NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_O_U_IN_JJ
      !END I = NI
    END DO

    J = NJ
      I = 0
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_O_U_I0_JN
      !END I = 0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI-1
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_O_U_II_JN
      END DO
      I = NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_O_U_IN_JN
      !END I = NI
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
  SUBROUTINE MR_INTERP_XY_UV_O_U_I0_J0

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ( ACTIVITY(I+1,J+1) .AND. ACTIVITY( P ,Q-1) ) == BEACTIVE ) THEN
      UO( I , J ) = U( I ,J+1)
    ELSE
      UO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_O_U_I0_J0

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
  SUBROUTINE MR_INTERP_XY_UV_O_U_II_J0

    IMPLICIT NONE

    IF( ( ACTIVITY(I+1,J+1) .AND. ACTIVITY( I ,J+1) ) == BEACTIVE ) THEN
      UO( I , J ) = U( I ,J+1)
    ELSE
      UO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_O_U_II_J0

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
  SUBROUTINE MR_INTERP_XY_UV_O_U_IN_J0

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ( ACTIVITY(P+1,Q-1) .AND. ACTIVITY( I ,J+1) ) == BEACTIVE ) THEN
      UO( I , J ) = U( I ,J+1)
    ELSE
      UO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_O_U_IN_J0

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
  SUBROUTINE MR_INTERP_XY_UV_O_U_I0_JJ

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ( ACTIVITY(I+1,J+1) .AND. ACTIVITY( P ,Q-1) ) == BEACTIVE .OR.   &
    &   ( ACTIVITY(I+1, J ) .AND. ACTIVITY( P , Q ) ) == BEACTIVE ) THEN
      UO( I , J ) =   &
      ( U( I ,J+1) * ( ACTIVITY(I+1,J+1) .AND. ACTIVITY( P ,Q-1) )   &
      + U( I , J ) * ( ACTIVITY(I+1, J ) .AND. ACTIVITY( P , Q ) ) )   &
      / ( ( ACTIVITY(I+1,J+1) .AND. ACTIVITY( P ,Q-1) )   &
      & + ( ACTIVITY(I+1, J ) .AND. ACTIVITY( P , Q ) ) )
    ELSE
      UO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_O_U_I0_JJ

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
  SUBROUTINE MR_INTERP_XY_UV_O_U_II_JJ

    IMPLICIT NONE

    IF( ( ACTIVITY(I+1,J+1) .AND. ACTIVITY( I ,J+1) ) == BEACTIVE .OR.   &
    &   ( ACTIVITY(I+1, J ) .AND. ACTIVITY( I , J ) ) == BEACTIVE ) THEN
      UO( I , J ) =   &
      ( U( I ,J+1) * ( ACTIVITY(I+1,J+1) .AND. ACTIVITY( I ,J+1) )   &
      + U( I , J ) * ( ACTIVITY(I+1, J ) .AND. ACTIVITY( I , J ) ) )   &
      / ( ( ACTIVITY(I+1,J+1) .AND. ACTIVITY( I ,J+1) )   &
      & + ( ACTIVITY(I+1, J ) .AND. ACTIVITY( I , J ) ) )
    ELSE
      UO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_O_U_II_JJ

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
  SUBROUTINE MR_INTERP_XY_UV_O_U_IN_JJ

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ( ACTIVITY(P+1,Q-1) .AND. ACTIVITY( I ,J+1) ) == BEACTIVE .OR.   &
    &   ( ACTIVITY(P+1, Q ) .AND. ACTIVITY( I , J ) ) == BEACTIVE ) THEN
      UO( I , J ) =   &
      ( U( I ,J+1) * ( ACTIVITY(P+1,Q-1) .AND. ACTIVITY( I ,J+1) )   &
      + U( I , J ) * ( ACTIVITY(P+1, Q ) .AND. ACTIVITY( I , J ) ) )   &
      / ( ( ACTIVITY(P+1,Q-1) .AND. ACTIVITY( I ,J+1) )   &
      & + ( ACTIVITY(P+1, Q ) .AND. ACTIVITY( I , J ) ) )
    ELSE
      UO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_O_U_IN_JJ

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
  SUBROUTINE MR_INTERP_XY_UV_O_U_I0_JN

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ( ACTIVITY(I+1, J ) .AND. ACTIVITY( P , Q ) ) == BEACTIVE ) THEN
      UO( I , J ) = U( I , J )
    ELSE
      UO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_O_U_I0_JN

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
  SUBROUTINE MR_INTERP_XY_UV_O_U_II_JN

    IMPLICIT NONE

    IF( ( ACTIVITY(I+1, J ) .AND. ACTIVITY( I , J ) ) == BEACTIVE ) THEN
      UO( I , J ) = U( I , J )
    ELSE
      UO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_O_U_II_JN

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
  SUBROUTINE MR_INTERP_XY_UV_O_U_IN_JN

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ( ACTIVITY(P+1, Q ) .AND. ACTIVITY( I , J ) ) == BEACTIVE ) THEN
      UO( I , J ) = U( I , J )
    ELSE
      UO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_O_U_IN_JN

  END SUBROUTINE MR_INTERP_XY_UV_O_U

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
  SUBROUTINE MR_INTERP_XY_SS_O_U( NI , NJ , SU , SUO )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ) :: SU
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ) :: SUO

    INTEGER(IJID_KIND) :: I , J

    J = 0
      I = 0
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_SS_O_U_I0_J0
      !END I = 0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI-1
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_SS_O_U_II_J0
      END DO
      I = NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_SS_O_U_IN_J0
      !END I = NI
    !END J = 0

    DO J = 1 , NJ-1
      I = 0
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_SS_O_U_I0_JJ
      !END I = 0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI-1
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_SS_O_U_II_JJ
      END DO
      I = NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_SS_O_U_IN_JJ
      !END I = NI
    END DO

    J = NJ
      I = 0
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_SS_O_U_I0_JN
      !END I = 0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI-1
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_SS_O_U_II_JN
      END DO
      I = NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_SS_O_U_IN_JN
      !END I = NI
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
  SUBROUTINE MR_INTERP_XY_SS_O_U_I0_J0

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ( ACTIVITY(I+1,J+1) .OR. ACTIVITY( P ,Q-1) ) == BEACTIVE ) THEN
      SUO( I , J ) = SU( I ,J+1)
    ELSE
      SUO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_SS_O_U_I0_J0

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
  SUBROUTINE MR_INTERP_XY_SS_O_U_II_J0

    IMPLICIT NONE

    IF( ( ACTIVITY(I+1,J+1) .OR. ACTIVITY( I ,J+1) ) == BEACTIVE ) THEN
      SUO( I , J ) = SU( I ,J+1)
    ELSE
      SUO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_SS_O_U_II_J0

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
  SUBROUTINE MR_INTERP_XY_SS_O_U_IN_J0

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ( ACTIVITY(P+1,Q-1) .OR. ACTIVITY( I ,J+1) ) == BEACTIVE ) THEN
      SUO( I , J ) = SU( I ,J+1)
    ELSE
      SUO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_SS_O_U_IN_J0

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
  SUBROUTINE MR_INTERP_XY_SS_O_U_I0_JJ

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ( ACTIVITY(I+1,J+1) .OR. ACTIVITY( P ,Q-1) ) == BEACTIVE .OR.   &
    &   ( ACTIVITY(I+1, J ) .OR. ACTIVITY( P , Q ) ) == BEACTIVE ) THEN
      SUO( I , J ) =   &
      ( SU( I ,J+1) * ( ACTIVITY(I+1,J+1) .OR. ACTIVITY( P ,Q-1) )   &
      + SU( I , J ) * ( ACTIVITY(I+1, J ) .OR. ACTIVITY( P , Q ) ) )   &
      / ( ( ACTIVITY(I+1,J+1) .OR. ACTIVITY( P ,Q-1) )   &
      & + ( ACTIVITY(I+1, J ) .OR. ACTIVITY( P , Q ) ) )
    ELSE
      SUO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_SS_O_U_I0_JJ

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
  SUBROUTINE MR_INTERP_XY_SS_O_U_II_JJ

    IMPLICIT NONE

    IF( ( ACTIVITY(I+1,J+1) .OR. ACTIVITY( I ,J+1) ) == BEACTIVE .OR.   &
    &   ( ACTIVITY(I+1, J ) .OR. ACTIVITY( I , J ) ) == BEACTIVE ) THEN
      SUO( I , J ) =   &
      ( SU( I ,J+1) * ( ACTIVITY(I+1,J+1) .OR. ACTIVITY( I ,J+1) )   &
      + SU( I , J ) * ( ACTIVITY(I+1, J ) .OR. ACTIVITY( I , J ) ) )   &
      / ( ( ACTIVITY(I+1,J+1) .OR. ACTIVITY( I ,J+1) )   &
      & + ( ACTIVITY(I+1, J ) .OR. ACTIVITY( I , J ) ) )
    ELSE
      SUO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_SS_O_U_II_JJ

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
  SUBROUTINE MR_INTERP_XY_SS_O_U_IN_JJ

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ( ACTIVITY(P+1,Q-1) .OR. ACTIVITY( I ,J+1) ) == BEACTIVE .OR.   &
    &   ( ACTIVITY(P+1, Q ) .OR. ACTIVITY( I , J ) ) == BEACTIVE ) THEN
      SUO( I , J ) =   &
      ( SU( I ,J+1) * ( ACTIVITY(P+1,Q-1) .OR. ACTIVITY( I ,J+1) )   &
      + SU( I , J ) * ( ACTIVITY(P+1, Q ) .OR. ACTIVITY( I , J ) ) )   &
      / ( ( ACTIVITY(P+1,Q-1) .OR. ACTIVITY( I ,J+1) )   &
      & + ( ACTIVITY(P+1, Q ) .OR. ACTIVITY( I , J ) ) )
    ELSE
      SUO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_SS_O_U_IN_JJ

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
  SUBROUTINE MR_INTERP_XY_SS_O_U_I0_JN

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ( ACTIVITY(I+1, J ) .OR. ACTIVITY( P , Q ) ) == BEACTIVE ) THEN
      SUO( I , J ) = SU( I , J )
    ELSE
      SUO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_SS_O_U_I0_JN

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
  SUBROUTINE MR_INTERP_XY_SS_O_U_II_JN

    IMPLICIT NONE

    IF( ( ACTIVITY(I+1, J ) .OR. ACTIVITY( I , J ) ) == BEACTIVE ) THEN
      SUO( I , J ) = SU( I , J )
    ELSE
      SUO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_SS_O_U_II_JN

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
  SUBROUTINE MR_INTERP_XY_SS_O_U_IN_JN

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ( ACTIVITY(P+1, Q ) .OR. ACTIVITY( I , J ) ) == BEACTIVE ) THEN
      SUO( I , J ) = SU( I , J )
    ELSE
      SUO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_SS_O_U_IN_JN

  END SUBROUTINE MR_INTERP_XY_SS_O_U

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
  SUBROUTINE MR_INTERP_XY_ZS_O_U( NI , NJ , ZSU , ZSUO )

    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_SLOPE

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ) :: ZSU
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ) :: ZSUO

    REAL   (FDRD_KIND) :: SRATIO

    INTEGER(IJID_KIND) :: I , J

    SRATIO = XYR/ZR / ALPAR * SLOPE

    J = 0
      I = 0
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_ZS_O_U_I0_J0
      !END I = 0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI-1
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_ZS_O_U_II_J0
      END DO
      I = NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_ZS_O_U_IN_J0
      !END I = NI
    !END J = 0

    DO J = 1 , NJ-1
      I = 0
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_ZS_O_U_I0_JJ
      !END I = 0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI-1
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_ZS_O_U_II_JJ
      END DO
      I = NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_ZS_O_U_IN_JJ
      !END I = NI
    END DO

    J = NJ
      I = 0
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_ZS_O_U_I0_JN
      !END I = 0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI-1
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_ZS_O_U_II_JN
      END DO
      I = NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_ZS_O_U_IN_JN
      !END I = NI
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
  SUBROUTINE MR_INTERP_XY_ZS_O_U_I0_J0

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ( ACTIVITY(I+1,J+1) .OR. ACTIVITY( P ,Q-1) ) == BEACTIVE ) THEN
      ZSUO( I , J ) = ZSU( I ,J+1) - 0.5 * JUU( I ,J+1,1,2) * SRATIO
    ELSE
      ZSUO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_ZS_O_U_I0_J0

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
  SUBROUTINE MR_INTERP_XY_ZS_O_U_II_J0

    IMPLICIT NONE

    IF( ( ACTIVITY(I+1,J+1) .OR. ACTIVITY( I ,J+1) ) == BEACTIVE ) THEN
      ZSUO( I , J ) = ZSU( I ,J+1) - 0.5 * JUU( I ,J+1,1,2) * SRATIO
    ELSE
      ZSUO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_ZS_O_U_II_J0

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
  SUBROUTINE MR_INTERP_XY_ZS_O_U_IN_J0

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ( ACTIVITY(P+1,Q-1) .OR. ACTIVITY( I ,J+1) ) == BEACTIVE ) THEN
      ZSUO( I , J ) = ZSU( I ,J+1) - 0.5 * JUU( I ,J+1,1,2) * SRATIO
    ELSE
      ZSUO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_ZS_O_U_IN_J0

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
  SUBROUTINE MR_INTERP_XY_ZS_O_U_I0_JJ

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ( ACTIVITY(I+1,J+1) .OR. ACTIVITY( P ,Q-1) ) == BEACTIVE .OR.   &
    &   ( ACTIVITY(I+1, J ) .OR. ACTIVITY( P , Q ) ) == BEACTIVE ) THEN
      ZSUO( I , J ) =   &
      ( ( ZSU( I ,J+1) - 0.5 * JUU( I ,J+1,1,2) * SRATIO ) * ( ACTIVITY(I+1,J+1) .OR. ACTIVITY( P ,Q-1) )   &
      + ( ZSU( I , J ) + 0.5 * JUU( I , J ,1,2) * SRATIO ) * ( ACTIVITY(I+1, J ) .OR. ACTIVITY( P , Q ) ) )   &
      / ( ( ACTIVITY(I+1,J+1) .OR. ACTIVITY( P ,Q-1) )   &
      & + ( ACTIVITY(I+1, J ) .OR. ACTIVITY( P , Q ) ) )
    ELSE
      ZSUO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_ZS_O_U_I0_JJ

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
  SUBROUTINE MR_INTERP_XY_ZS_O_U_II_JJ

    IMPLICIT NONE

    IF( ( ACTIVITY(I+1,J+1) .OR. ACTIVITY( I ,J+1) ) == BEACTIVE .OR.   &
    &   ( ACTIVITY(I+1, J ) .OR. ACTIVITY( I , J ) ) == BEACTIVE ) THEN
      ZSUO( I , J ) =   &
      ( ( ZSU( I ,J+1) - 0.5 * JUU( I ,J+1,1,2) * SRATIO ) * ( ACTIVITY(I+1,J+1) .OR. ACTIVITY( I ,J+1) )   &
      + ( ZSU( I , J ) + 0.5 * JUU( I , J ,1,2) * SRATIO ) * ( ACTIVITY(I+1, J ) .OR. ACTIVITY( I , J ) ) )   &
      / ( ( ACTIVITY(I+1,J+1) .OR. ACTIVITY( I ,J+1) )   &
      & + ( ACTIVITY(I+1, J ) .OR. ACTIVITY( I , J ) ) )
    ELSE
      ZSUO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_ZS_O_U_II_JJ

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
  SUBROUTINE MR_INTERP_XY_ZS_O_U_IN_JJ

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ( ACTIVITY(P+1,Q-1) .OR. ACTIVITY( I ,J+1) ) == BEACTIVE .OR.   &
    &   ( ACTIVITY(P+1, Q ) .OR. ACTIVITY( I , J ) ) == BEACTIVE ) THEN
      ZSUO( I , J ) =   &
      ( ( ZSU( I ,J+1) - 0.5 * JUU( I ,J+1,1,2) * SRATIO ) * ( ACTIVITY(P+1,Q-1) .OR. ACTIVITY( I ,J+1) )   &
      + ( ZSU( I , J ) + 0.5 * JUU( I , J ,1,2) * SRATIO ) * ( ACTIVITY(P+1, Q ) .OR. ACTIVITY( I , J ) ) )   &
      / ( ( ACTIVITY(P+1,Q-1) .OR. ACTIVITY( I ,J+1) )   &
      & + ( ACTIVITY(P+1, Q ) .OR. ACTIVITY( I , J ) ) )
    ELSE
      ZSUO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_ZS_O_U_IN_JJ

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
  SUBROUTINE MR_INTERP_XY_ZS_O_U_I0_JN

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ( ACTIVITY(I+1, J ) .OR. ACTIVITY( P , Q ) ) == BEACTIVE ) THEN
      ZSUO( I , J ) = ZSU( I , J ) + 0.5 * JUU( I , J ,1,2) * SRATIO
    ELSE
      ZSUO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_ZS_O_U_I0_JN

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
  SUBROUTINE MR_INTERP_XY_ZS_O_U_II_JN

    IMPLICIT NONE

    IF( ( ACTIVITY(I+1, J ) .OR. ACTIVITY( I , J ) ) == BEACTIVE ) THEN
      ZSUO( I , J ) = ZSU( I , J ) + 0.5 * JUU( I , J ,1,2) * SRATIO
    ELSE
      ZSUO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_ZS_O_U_II_JN

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
  SUBROUTINE MR_INTERP_XY_ZS_O_U_IN_JN

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ( ACTIVITY(P+1, Q ) .OR. ACTIVITY( I , J ) ) == BEACTIVE ) THEN
      ZSUO( I , J ) = ZSU( I , J ) + 0.5 * JUU( I , J ,1,2) * SRATIO
    ELSE
      ZSUO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_ZS_O_U_IN_JN

  END SUBROUTINE MR_INTERP_XY_ZS_O_U

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
  SUBROUTINE MR_INV_INTERP_XY_UV_FROM_UU( NI , NJ , UU , UV )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ,1:2) :: UU
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: UV

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    DO DIM = 1 , 2

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
            UV( I , J ,DIM) = 0.5 * ( UU( I , J ,DIM) + UU(I-1, J ,DIM) )
          ELSE
            UV( I , J ,DIM) = 0.0
          END IF
        END DO
      END DO

    END DO

  END SUBROUTINE MR_INV_INTERP_XY_UV_FROM_UU

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
  SUBROUTINE MR_INV_INTERP_XY_SS_FROM_SU( NI , NJ , SU , SS )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ) :: SU
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) :: SS

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
          SS( I , J ) = 0.5 * ( SU( I , J ) + SU(I-1, J ) )
        ELSE
          SS( I , J ) = 0.0
        END IF
      END DO
    END DO

  END SUBROUTINE MR_INV_INTERP_XY_SS_FROM_SU

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
  SUBROUTINE MR_INTERP_XY_UV_V_BY_RCPRAC( NI , NJ , ZS , ALFA , BETA , V )

    USE MR_DEF_CURVED_GEOS , ONLY : MV
    USE MR_DEF_FIELD_VARS , ONLY : HV

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ    ) :: ZS
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,CARD_KIND),1:NJ,1:2) :: ALFA , BETA

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ    ) :: V

    INTEGER(IJID_KIND) :: I , J

    J = 0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_V_BY_RCPRAC_II_J0
      END DO
    !END J = 0

    DO J = 1 , NJ-1
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_V_BY_RCPRAC_II_JJ
      END DO
    END DO

    J = NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_V_BY_RCPRAC_II_JN
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
  SUBROUTINE MR_INTERP_XY_UV_V_BY_RCPRAC_II_J0

    IMPLICIT NONE

    V( I , J ) = 0.0

  END SUBROUTINE MR_INTERP_XY_UV_V_BY_RCPRAC_II_J0

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
  SUBROUTINE MR_INTERP_XY_UV_V_BY_RCPRAC_II_JJ

    IMPLICIT NONE

    IF( ACTIVITY( I ,J+1) == BEACTIVE .AND. ACTIVITY( I , J ) == BEACTIVE ) THEN
      V( I , J ) = 0.5 * ( ( ALFA( I ,J+1,2) + ALFA( I , J ,2) ) * ( ZS( I ,J+1) - ZS( I , J ) )   &
      + ( BETA( I ,J+1,2) + BETA( I , J ,2) )   &
      ) / MV( I , J ) / HV( I , J )
    ELSE
      V( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_V_BY_RCPRAC_II_JJ

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
  SUBROUTINE MR_INTERP_XY_UV_V_BY_RCPRAC_II_JN

    IMPLICIT NONE

    V( I , J ) = 0.0

  END SUBROUTINE MR_INTERP_XY_UV_V_BY_RCPRAC_II_JN

  END SUBROUTINE MR_INTERP_XY_UV_V_BY_RCPRAC

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE) INTERP_XY_UV_V
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
  SUBROUTINE MR_INTERP_XY_UV_V_BY_LINEAR( NI , NJ , UV , V )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: UV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ    ) :: V

    INTEGER(IJID_KIND) :: I , J

    J = 0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_V_BY_LINEAR_II_J0
      END DO
    !END J = 0

    DO J = 1 , NJ-1
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_V_BY_LINEAR_II_JJ
      END DO
    END DO

    J = NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_V_BY_LINEAR_II_JN
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
  SUBROUTINE MR_INTERP_XY_UV_V_BY_LINEAR_II_J0

    IMPLICIT NONE

    V( I , J ) = 0.0

  END SUBROUTINE MR_INTERP_XY_UV_V_BY_LINEAR_II_J0

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
  SUBROUTINE MR_INTERP_XY_UV_V_BY_LINEAR_II_JJ

    IMPLICIT NONE

    IF( ACTIVITY( I ,J+1) == BEACTIVE .AND. ACTIVITY( I , J ) == BEACTIVE ) THEN
      V( I , J ) = 0.5 * ( UV( I ,J+1,2) + UV( I , J ,2) )
    ELSE
      V( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_V_BY_LINEAR_II_JJ

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
  SUBROUTINE MR_INTERP_XY_UV_V_BY_LINEAR_II_JN

    IMPLICIT NONE

    V( I , J ) = 0.0

  END SUBROUTINE MR_INTERP_XY_UV_V_BY_LINEAR_II_JN

  END SUBROUTINE MR_INTERP_XY_UV_V_BY_LINEAR

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
  SUBROUTINE MR_INTERP_XY_UV_V_TO_GET_U_AT_V( NI , NJ , UV , U_AT_V )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: UV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ    ) :: U_AT_V

    INTEGER(IJID_KIND) :: I , J

    J = 0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_V_TO_GET_U_AT_V_II_J0
      END DO
    !END J = 0

    DO J = 1 , NJ-1
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_V_TO_GET_U_AT_V_II_JJ
      END DO
    END DO

    J = NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_V_TO_GET_U_AT_V_II_JN
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
  SUBROUTINE MR_INTERP_XY_UV_V_TO_GET_U_AT_V_II_J0

    IMPLICIT NONE

    IF( ACTIVITY( I ,J+1) == BEACTIVE ) THEN
      U_AT_V( I , J ) = UV( I ,J+1,1)
    ELSE
      U_AT_V( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_V_TO_GET_U_AT_V_II_J0

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
  SUBROUTINE MR_INTERP_XY_UV_V_TO_GET_U_AT_V_II_JJ

    IMPLICIT NONE

    IF( ACTIVITY( I ,J+1) == BEACTIVE .OR. ACTIVITY( I , J ) == BEACTIVE ) THEN
      U_AT_V( I , J ) =   &
      ( UV( I ,J+1,1) * ACTIVITY( I ,J+1)   &
      + UV( I , J ,1) * ACTIVITY( I , J ) )   &
      / ( ACTIVITY( I ,J+1) + ACTIVITY( I , J ) )
    ELSE
      U_AT_V( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_V_TO_GET_U_AT_V_II_JJ

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
  SUBROUTINE MR_INTERP_XY_UV_V_TO_GET_U_AT_V_II_JN

    IMPLICIT NONE

    IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
      U_AT_V( I , J ) = UV( I , J ,1)
    ELSE
      U_AT_V( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_V_TO_GET_U_AT_V_II_JN

  END SUBROUTINE MR_INTERP_XY_UV_V_TO_GET_U_AT_V

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
  SUBROUTINE MR_INTERP_XY_SS_V( NI , NJ , SS , SV )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) :: SS
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ) :: SV

    INTEGER(IJID_KIND) :: I , J

    J = 0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_SS_V_II_J0
      END DO
    !END J = 0

    DO J = 1 , NJ-1
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_SS_V_II_JJ
      END DO
    END DO

    J = NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_SS_V_II_JN
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
  SUBROUTINE MR_INTERP_XY_SS_V_II_J0

    IMPLICIT NONE

    IF( ACTIVITY( I ,J+1) == BEACTIVE ) THEN
      SV( I , J ) = SS( I ,J+1)
    ELSE
      SV( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_SS_V_II_J0

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
  SUBROUTINE MR_INTERP_XY_SS_V_II_JJ

    IMPLICIT NONE

    IF( ACTIVITY( I ,J+1) == BEACTIVE .OR. ACTIVITY( I , J ) == BEACTIVE ) THEN
      SV( I , J ) =   &
      ( SS( I ,J+1) * ACTIVITY( I ,J+1)   &
      + SS( I , J ) * ACTIVITY( I , J ) )   &
      / ( ACTIVITY( I ,J+1) + ACTIVITY( I , J ) )
    ELSE
      SV( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_SS_V_II_JJ

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
  SUBROUTINE MR_INTERP_XY_SS_V_II_JN

    IMPLICIT NONE

    IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
      SV( I , J ) = SS( I , J )
    ELSE
      SV( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_SS_V_II_JN

  END SUBROUTINE MR_INTERP_XY_SS_V

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
  SUBROUTINE MR_INTERP_XY_ZS_V( NI , NJ , ZS , ZSV )

    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_SLOPE

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) :: ZS
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ) :: ZSV

    REAL   (FDRD_KIND) :: SRATIO

    INTEGER(IJID_KIND) :: I , J

    SRATIO = XYR/ZR / ALPAR * SLOPE

    J = 0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_ZS_V_II_J0
      END DO
    !END J = 0

    DO J = 1 , NJ-1
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_ZS_V_II_JJ
      END DO
    END DO

    J = NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_ZS_V_II_JN
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
  SUBROUTINE MR_INTERP_XY_ZS_V_II_J0

    IMPLICIT NONE

    IF( ACTIVITY( I ,J+1) == BEACTIVE ) THEN
      ZSV( I , J ) = ZS( I ,J+1) - 0.5 * JUV( I ,J+1,1,2) * SRATIO
    ELSE
      ZSV( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_ZS_V_II_J0

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
  SUBROUTINE MR_INTERP_XY_ZS_V_II_JJ

    IMPLICIT NONE

    IF( ACTIVITY( I ,J+1) == BEACTIVE .OR. ACTIVITY( I , J ) == BEACTIVE ) THEN
      ZSV( I , J ) =   &
      ( ( ZS( I ,J+1) - 0.5 * JUV( I ,J+1,1,2) * SRATIO ) * ACTIVITY( I ,J+1)   &
      + ( ZS( I , J ) + 0.5 * JUV( I , J ,1,2) * SRATIO ) * ACTIVITY( I , J ) )   &
      / ( ACTIVITY( I ,J+1) + ACTIVITY( I , J ) )
    ELSE
      ZSV( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_ZS_V_II_JJ

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
  SUBROUTINE MR_INTERP_XY_ZS_V_II_JN

    IMPLICIT NONE

    IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
      ZSV( I , J ) = ZS( I , J ) + 0.5 * JUV( I , J ,1,2) * SRATIO
    ELSE
      ZSV( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_ZS_V_II_JN

  END SUBROUTINE MR_INTERP_XY_ZS_V

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
  SUBROUTINE MR_INTERP_XY_UV_O_V( NI , NJ , V , VO )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ    ) :: V
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ    ) :: VO

    REAL   (FDRD_KIND) , PARAMETER   , DIMENSION(                         1:2) :: FACTOR = (/+1.0,-1.0/)

    INTEGER(IJID_KIND) :: I , J

    J = 0
     !DIR$ VECTOR ALIGNED
      DO I = 0 , NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_O_V_II_J0
      END DO
    !END J = 0

    DO J = 1 , NJ-1
      I = 0
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_O_V_I0_JJ
      !END I = 0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI-1
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_O_V_II_JJ
      END DO
      I = NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_O_V_IN_JJ
      !END I = NI
    END DO

    J = NJ
     !DIR$ VECTOR ALIGNED
      DO I = 0 , NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_UV_O_V_II_JN
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
  SUBROUTINE MR_INTERP_XY_UV_O_V_II_J0

    IMPLICIT NONE

    VO( I , J ) = 0.0

  END SUBROUTINE MR_INTERP_XY_UV_O_V_II_J0

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
  SUBROUTINE MR_INTERP_XY_UV_O_V_I0_JJ

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ( ACTIVITY(I+1,J+1) .AND. ACTIVITY(I+1, J ) ) == BEACTIVE .OR.   &
    &   ( ACTIVITY( P ,Q-1) .AND. ACTIVITY( P , Q ) ) == BEACTIVE ) THEN
      VO( I , J ) =   &
      ( V(I+1, J ) * ( ACTIVITY(I+1,J+1) .AND. ACTIVITY(I+1, J ) )   &
      + V( P ,Q-1) * ( ACTIVITY( P ,Q-1) .AND. ACTIVITY( P , Q ) ) * FACTOR(2) )   &
      / ( ( ACTIVITY(I+1,J+1) .AND. ACTIVITY(I+1, J ) )   &
      & + ( ACTIVITY( P ,Q-1) .AND. ACTIVITY( P , Q ) ) )
    ELSE
      VO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_O_V_I0_JJ

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
  SUBROUTINE MR_INTERP_XY_UV_O_V_II_JJ

    IMPLICIT NONE

    IF( ( ACTIVITY(I+1,J+1) .AND. ACTIVITY(I+1, J ) ) == BEACTIVE .OR.   &
    &   ( ACTIVITY( I ,J+1) .AND. ACTIVITY( I , J ) ) == BEACTIVE ) THEN
      VO( I , J ) =   &
      ( V(I+1, J ) * ( ACTIVITY(I+1,J+1) .AND. ACTIVITY(I+1, J ) )   &
      + V( I , J ) * ( ACTIVITY( I ,J+1) .AND. ACTIVITY( I , J ) ) )   &
      / ( ( ACTIVITY(I+1,J+1) .AND. ACTIVITY(I+1, J ) )   &
      & + ( ACTIVITY( I ,J+1) .AND. ACTIVITY( I , J ) ) )
    ELSE
      VO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_O_V_II_JJ

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
  SUBROUTINE MR_INTERP_XY_UV_O_V_IN_JJ

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ( ACTIVITY(P+1,Q-1) .AND. ACTIVITY(P+1, Q ) ) == BEACTIVE .OR.   &
    &   ( ACTIVITY( I ,J+1) .AND. ACTIVITY( I , J ) ) == BEACTIVE ) THEN
      VO( I , J ) =   &
      ( V(P+1,Q-1) * ( ACTIVITY(P+1,Q-1) .AND. ACTIVITY(P+1, Q ) ) * FACTOR(2)  &
      + V( I , J ) * ( ACTIVITY( I ,J+1) .AND. ACTIVITY( I , J ) ) )   &
      / ( ( ACTIVITY(P+1,Q-1) .AND. ACTIVITY(P+1, Q ) )   &
      & + ( ACTIVITY( I ,J+1) .AND. ACTIVITY( I , J ) ) )
    ELSE
      VO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_O_V_IN_JJ

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
  SUBROUTINE MR_INTERP_XY_UV_O_V_II_JN

    IMPLICIT NONE

    VO( I , J ) = 0.0

  END SUBROUTINE MR_INTERP_XY_UV_O_V_II_JN

  END SUBROUTINE MR_INTERP_XY_UV_O_V

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
  SUBROUTINE MR_INTERP_XY_SS_O_V( NI , NJ , SV , SVO )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ) :: SV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ) :: SVO

    INTEGER(IJID_KIND) :: I , J

    J = 0
      I = 0
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_SS_O_V_I0_J0
      !END I = 0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI-1
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_SS_O_V_II_J0
      END DO
      I = NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_SS_O_V_IN_J0
      !END I = NI
    !END J = NJ

    DO J = 1 , NJ-1
      I = 0
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_SS_O_V_I0_JJ
      !END I = 0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI-1
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_SS_O_V_II_JJ
      END DO
      I = NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_SS_O_V_IN_JJ
      !END I = NI
    END DO

    J = NJ
      I = 0
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_SS_O_V_I0_JN
      !END I = 0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI-1
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_SS_O_V_II_JN
      END DO
      I = NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_SS_O_V_IN_JN
      !END I = NI
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
  SUBROUTINE MR_INTERP_XY_SS_O_V_I0_J0

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ACTIVITY(I+1,J+1) == BEACTIVE .OR.   &
    &   ACTIVITY( P ,Q-1) == BEACTIVE ) THEN
      SVO( I , J ) =   &
      ( SV(I+1, J ) * ACTIVITY(I+1,J+1)   &
      + SV( P ,Q-1) * ACTIVITY( P ,Q-1) )   &
      / ( ACTIVITY(I+1,J+1) + ACTIVITY( P ,Q-1) )
    ELSE
      SVO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_SS_O_V_I0_J0

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
  SUBROUTINE MR_INTERP_XY_SS_O_V_II_J0

    IMPLICIT NONE

    IF( ACTIVITY(I+1,J+1) == BEACTIVE .OR.   &
    &   ACTIVITY( I ,J+1) == BEACTIVE ) THEN
      SVO( I , J ) =   &
      ( SV(I+1, J ) * ACTIVITY(I+1,J+1)   &
      + SV( I , J ) * ACTIVITY( I ,J+1) )   &
      / ( ACTIVITY(I+1,J+1) + ACTIVITY( I ,J+1) )
    ELSE
      SVO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_SS_O_V_II_J0

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
  SUBROUTINE MR_INTERP_XY_SS_O_V_IN_J0

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ACTIVITY(P+1,Q-1) == BEACTIVE .OR.   &
    &   ACTIVITY( I ,J+1) == BEACTIVE ) THEN
      SVO( I , J ) =   &
      ( SV(P+1,Q-1) * ACTIVITY(P+1,Q-1)   &
      + SV( I , J ) * ACTIVITY( I ,J+1) )   &
      / ( ACTIVITY(P+1,Q-1) + ACTIVITY( I ,J+1) )
    ELSE
      SVO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_SS_O_V_IN_J0

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
  SUBROUTINE MR_INTERP_XY_SS_O_V_I0_JJ

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ( ACTIVITY(I+1,J+1) .OR. ACTIVITY(I+1, J ) ) == BEACTIVE .OR.   &
    &   ( ACTIVITY( P ,Q-1) .OR. ACTIVITY( P , Q ) ) == BEACTIVE ) THEN
      SVO( I , J ) =   &
      ( SV(I+1, J ) * ( ACTIVITY(I+1,J+1) .OR. ACTIVITY(I+1, J ) )   &
      + SV( P ,Q-1) * ( ACTIVITY( P ,Q-1) .OR. ACTIVITY( P , Q ) ) )   &
      / ( ( ACTIVITY(I+1,J+1) .OR. ACTIVITY(I+1, J ) )   &
      & + ( ACTIVITY( P ,Q-1) .OR. ACTIVITY( P , Q ) ) )
    ELSE
      SVO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_SS_O_V_I0_JJ

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
  SUBROUTINE MR_INTERP_XY_SS_O_V_II_JJ

    IMPLICIT NONE

    IF( ( ACTIVITY(I+1,J+1) .OR. ACTIVITY(I+1, J ) ) == BEACTIVE .OR.   &
    &   ( ACTIVITY( I ,J+1) .OR. ACTIVITY( I , J ) ) == BEACTIVE ) THEN
      SVO( I , J ) =   &
      ( SV(I+1, J ) * ( ACTIVITY(I+1,J+1) .OR. ACTIVITY(I+1, J ) )   &
      + SV( I , J ) * ( ACTIVITY( I ,J+1) .OR. ACTIVITY( I , J ) ) )   &
      / ( ( ACTIVITY(I+1,J+1) .OR. ACTIVITY(I+1, J ) )   &
      & + ( ACTIVITY( I ,J+1) .OR. ACTIVITY( I , J ) ) )
    ELSE
      SVO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_SS_O_V_II_JJ

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
  SUBROUTINE MR_INTERP_XY_SS_O_V_IN_JJ

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ( ACTIVITY(P+1,Q-1) .OR. ACTIVITY(P+1, Q ) ) == BEACTIVE .OR.   &
    &   ( ACTIVITY( I ,J+1) .OR. ACTIVITY( I , J ) ) == BEACTIVE ) THEN
      SVO( I , J ) =   &
      ( SV(P+1,Q-1) * ( ACTIVITY(P+1,Q-1) .OR. ACTIVITY(P+1, Q ) )   &
      + SV( I , J ) * ( ACTIVITY( I ,J+1) .OR. ACTIVITY( I , J ) ) )   &
      / ( ( ACTIVITY(P+1,Q-1) .OR. ACTIVITY(P+1, Q ) )   &
      & + ( ACTIVITY( I ,J+1) .OR. ACTIVITY( I , J ) ) )
    ELSE
      SVO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_SS_O_V_IN_JJ

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
  SUBROUTINE MR_INTERP_XY_SS_O_V_I0_JN

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ACTIVITY(I+1, J ) == BEACTIVE .OR.   &
    &   ACTIVITY( P , Q ) == BEACTIVE ) THEN
      SVO( I , J ) =   &
      ( SV(I+1, J ) * ACTIVITY(I+1, J )   &
      + SV( P ,Q-1) * ACTIVITY( P , Q ) )   &
      / ( ACTIVITY(I+1, J ) + ACTIVITY( P , Q ) )
    ELSE
      SVO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_SS_O_V_I0_JN

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
  SUBROUTINE MR_INTERP_XY_SS_O_V_II_JN

    IMPLICIT NONE

    IF( ACTIVITY(I+1, J ) == BEACTIVE .OR.   &
    &   ACTIVITY( I , J ) == BEACTIVE ) THEN
      SVO( I , J ) =   &
      ( SV(I+1, J ) * ACTIVITY(I+1, J )   &
      + SV( I , J ) * ACTIVITY( I , J ) )   &
      / ( ACTIVITY(I+1, J ) + ACTIVITY( I , J ) )
    ELSE
      SVO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_SS_O_V_II_JN

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
  SUBROUTINE MR_INTERP_XY_SS_O_V_IN_JN

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ACTIVITY(P+1, Q ) == BEACTIVE .OR.   &
    &   ACTIVITY( I , J ) == BEACTIVE ) THEN
      SVO( I , J ) =   &
      ( SV(P+1,Q-1) * ACTIVITY(P+1, Q )   &
      + SV( I , J ) * ACTIVITY( I , J ) )   &
      / ( ACTIVITY(P+1, Q ) + ACTIVITY( I , J ) )
    ELSE
      SVO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_SS_O_V_IN_JN

  END SUBROUTINE MR_INTERP_XY_SS_O_V

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
  SUBROUTINE MR_INTERP_XY_ZS_O_V( NI , NJ , ZSV , ZSVO )

    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_SLOPE

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ) :: ZSV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ) :: ZSVO

    REAL   (FDRD_KIND) :: SRATIO

    INTEGER(IJID_KIND) :: I , J

    SRATIO = XYR/ZR / ALPAR * SLOPE

    J = 0
      I = 0
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_ZS_O_V_I0_J0
      !END I = 0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI-1
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_ZS_O_V_II_J0
      END DO
      I = NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_ZS_O_V_IN_J0
      !END I = NI
    !END J = 0

    DO J = 1 , NJ-1
      I = 0
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_ZS_O_V_I0_JJ
      !END I = 0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI-1
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_ZS_O_V_II_JJ
      END DO
      I = NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_ZS_O_V_IN_JJ
      !END I = NI
    END DO

    J = NJ
      I = 0
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_ZS_O_V_I0_JN
      !END I = 0
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI-1
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_ZS_O_V_II_JN
      END DO
      I = NI
       !DIR$ FORCEINLINE
        CALL MR_INTERP_XY_ZS_O_V_IN_JN
      !END I = NI
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
  SUBROUTINE MR_INTERP_XY_ZS_O_V_I0_J0

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ACTIVITY(I+1,J+1) == BEACTIVE .OR.   &
    &   ACTIVITY( P ,Q-1) == BEACTIVE ) THEN
      ZSVO( I , J ) =   &
      ( ( ZSV(I+1, J ) - 0.5 * JVV(I+1, J ,1,1) * SRATIO ) * ACTIVITY(I+1,J+1)   &
      + ( ZSV( P ,Q-1) + 0.5 * JVV( P ,Q-1,1,1) * SRATIO ) * ACTIVITY( P ,Q-1) )   &
      / ( ACTIVITY(I+1,J+1) + ACTIVITY( P ,Q-1) )
    ELSE
      ZSVO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_ZS_O_V_I0_J0

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
  SUBROUTINE MR_INTERP_XY_ZS_O_V_II_J0

    IMPLICIT NONE

    IF( ACTIVITY(I+1,J+1) == BEACTIVE .OR.   &
    &   ACTIVITY( I ,J+1) == BEACTIVE ) THEN
      ZSVO( I , J ) =   &
      ( ( ZSV(I+1, J ) - 0.5 * JVV(I+1, J ,1,1) * SRATIO ) * ACTIVITY(I+1,J+1)   &
      + ( ZSV( I , J ) + 0.5 * JVV( I , J ,1,1) * SRATIO ) * ACTIVITY( I ,J+1) )   &
      / ( ACTIVITY(I+1,J+1) + ACTIVITY( I ,J+1) )
    ELSE
      ZSVO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_ZS_O_V_II_J0

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
  SUBROUTINE MR_INTERP_XY_ZS_O_V_IN_J0

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ACTIVITY(P+1,Q-1) == BEACTIVE .OR.   &
    &   ACTIVITY( I ,J+1) == BEACTIVE ) THEN
      ZSVO( I , J ) =   &
      ( ( ZSV(P+1,Q-1) - 0.5 * JVV(P+1,Q-1,1,1) * SRATIO ) * ACTIVITY(P+1,Q-1)   &
      + ( ZSV( I , J ) + 0.5 * JVV( I , J ,1,1) * SRATIO ) * ACTIVITY( I ,J+1) )   &
      / ( ACTIVITY(P+1,Q-1) + ACTIVITY( I ,J+1) )
    ELSE
      ZSVO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_ZS_O_V_IN_J0

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
  SUBROUTINE MR_INTERP_XY_ZS_O_V_I0_JJ

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ( ACTIVITY(I+1,J+1) .OR. ACTIVITY(I+1, J ) ) == BEACTIVE .OR.   &
    &   ( ACTIVITY( P ,Q-1) .OR. ACTIVITY( P , Q ) ) == BEACTIVE ) THEN
      ZSVO( I , J ) =   &
      ( ( ZSV(I+1, J ) - 0.5 * JVV(I+1, J ,1,1) * SRATIO ) * ( ACTIVITY(I+1,J+1) .OR. ACTIVITY(I+1, J ) )   &
      + ( ZSV( P ,Q-1) + 0.5 * JVV( P ,Q-1,1,1) * SRATIO ) * ( ACTIVITY( P ,Q-1) .OR. ACTIVITY( P , Q ) ) )   &
      / ( ( ACTIVITY(I+1,J+1) .OR. ACTIVITY(I+1, J ) )   &
      & + ( ACTIVITY( P ,Q-1) .OR. ACTIVITY( P , Q ) ) )
    ELSE
      ZSVO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_ZS_O_V_I0_JJ

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
  SUBROUTINE MR_INTERP_XY_ZS_O_V_II_JJ

    IMPLICIT NONE

    IF( ( ACTIVITY(I+1,J+1) .OR. ACTIVITY(I+1, J ) ) == BEACTIVE .OR.   &
    &   ( ACTIVITY( I ,J+1) .OR. ACTIVITY( I , J ) ) == BEACTIVE ) THEN
      ZSVO( I , J ) =   &
      ( ( ZSV(I+1, J ) - 0.5 * JVV(I+1, J ,1,1) * SRATIO ) * ( ACTIVITY(I+1,J+1) .OR. ACTIVITY(I+1, J ) )   &
      + ( ZSV( I , J ) + 0.5 * JVV( I , J ,1,1) * SRATIO ) * ( ACTIVITY( I ,J+1) .OR. ACTIVITY( I , J ) ) )   &
      / ( ( ACTIVITY(I+1,J+1) .OR. ACTIVITY(I+1, J ) )   &
      & + ( ACTIVITY( I ,J+1) .OR. ACTIVITY( I , J ) ) )
    ELSE
      ZSVO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_ZS_O_V_II_JJ

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
  SUBROUTINE MR_INTERP_XY_ZS_O_V_IN_JJ

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ( ACTIVITY(P+1,Q-1) .OR. ACTIVITY(P+1, Q ) ) == BEACTIVE .OR.   &
    &   ( ACTIVITY( I ,J+1) .OR. ACTIVITY( I , J ) ) == BEACTIVE ) THEN
      ZSVO( I , J ) =   &
      ( ( ZSV(P+1,Q-1) - 0.5 * JVV(P+1,Q-1,1,1) * SRATIO ) * ( ACTIVITY(P+1,Q-1) .OR. ACTIVITY(P+1, Q ) )   &
      + ( ZSV( I , J ) + 0.5 * JVV( I , J ,1,1) * SRATIO ) * ( ACTIVITY( I ,J+1) .OR. ACTIVITY( I , J ) ) )   &
      / ( ( ACTIVITY(P+1,Q-1) .OR. ACTIVITY(P+1, Q ) )   &
      & + ( ACTIVITY( I ,J+1) .OR. ACTIVITY( I , J ) ) )
    ELSE
      ZSVO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_ZS_O_V_IN_JJ

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
  SUBROUTINE MR_INTERP_XY_ZS_O_V_I0_JN

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ACTIVITY(I+1, J ) == BEACTIVE .OR.   &
    &   ACTIVITY( P , Q ) == BEACTIVE ) THEN
      ZSVO( I , J ) =   &
      ( ( ZSV(I+1, J ) - 0.5 * JVV(I+1, J ,1,1) * SRATIO ) * ACTIVITY(I+1, J )   &
      + ( ZSV( P ,Q-1) + 0.5 * JVV( P ,Q-1,1,1) * SRATIO ) * ACTIVITY( P , Q ) )   &
      / ( ACTIVITY(I+1, J ) + ACTIVITY( P , Q ) )
    ELSE
      ZSVO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_ZS_O_V_I0_JN

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
  SUBROUTINE MR_INTERP_XY_ZS_O_V_II_JN

    IMPLICIT NONE

    IF( ACTIVITY(I+1, J ) == BEACTIVE .OR.   &
    &   ACTIVITY( I , J ) == BEACTIVE ) THEN
      ZSVO( I , J ) =   &
      ( ( ZSV(I+1, J ) - 0.5 * JVV(I+1, J ,1,1) * SRATIO ) * ACTIVITY(I+1, J )   &
      + ( ZSV( I , J ) + 0.5 * JVV( I , J ,1,1) * SRATIO ) * ACTIVITY( I , J ) )   &
      / ( ACTIVITY(I+1, J ) + ACTIVITY( I , J ) )
    ELSE
      ZSVO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_ZS_O_V_II_JN

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
  SUBROUTINE MR_INTERP_XY_ZS_O_V_IN_JN

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: P , Q

    P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1

    IF( ACTIVITY(P+1, Q ) == BEACTIVE .OR.   &
    &   ACTIVITY( I , J ) == BEACTIVE ) THEN
      ZSVO( I , J ) =   &
      ( ( ZSV(P+1,Q-1) - 0.5 * JVV(P+1,Q-1,1,1) * SRATIO ) * ACTIVITY(P+1, Q )   &
      + ( ZSV( I , J ) + 0.5 * JVV( I , J ,1,1) * SRATIO ) * ACTIVITY( I , J ) )   &
      / ( ACTIVITY(P+1, Q ) + ACTIVITY( I , J ) )
    ELSE
      ZSVO( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_ZS_O_V_IN_JN

  END SUBROUTINE MR_INTERP_XY_ZS_O_V

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
  SUBROUTINE MR_INV_INTERP_XY_UV_FROM_VV( NI , NJ , VV , UV )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ,1:2) :: VV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: UV

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    DO DIM = 1 , 2

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
            UV( I , J ,DIM) = 0.5 * ( VV( I , J ,DIM) + VV( I ,J-1,DIM) )
          ELSE 
            UV( I , J ,DIM) = 0.0
          END IF
        END DO
      END DO

    END DO

  END SUBROUTINE MR_INV_INTERP_XY_UV_FROM_VV

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
  SUBROUTINE MR_INV_INTERP_XY_SS_FROM_SV( NI , NJ , SV , SS )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ) :: SV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) :: SS

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
          SS( I , J ) = 0.5 * ( SV( I , J ) + SV( I ,J-1) )
        ELSE
          SS( I , J ) = 0.0
        END IF
      END DO
    END DO

  END SUBROUTINE MR_INV_INTERP_XY_SS_FROM_SV

  END MODULE MR_MOD_INTERP_XY