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
    PUBLIC :: MR_INTERP_XY_XUV_U

    PUBLIC :: MR_INTERP_XY_UV_V_BY_RCPRAC
    PUBLIC :: MR_INTERP_XY_UV_V_BY_LINEAR
    PUBLIC :: MR_INTERP_XY_UV_V_TO_GET_U_AT_V
    PUBLIC :: MR_INTERP_XY_XUV_V

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
  SUBROUTINE MR_INTERP_XY_UV_U_BY_RCPRAC( NI , NJ , ZS , ALFA , BETA , U , U_AT_I0 )

    USE MR_DEF_CURVED_GEOS , ONLY : MU
    USE MR_DEF_FIELD_VARS , ONLY : HU

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ) :: ZS
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ,1:2) :: ALFA , BETA

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI,1:NJ) :: U

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(     1:NJ) :: U_AT_I0

    INTEGER(IJID_KIND) :: J

    DO J = 1 , NJ

      CALL MR_INTERP_XY_UV_U_BY_RCPRAC_AT_I0
      CALL MR_INTERP_XY_UV_U_BY_RCPRAC_AT_II
      CALL MR_INTERP_XY_UV_U_BY_RCPRAC_AT_IN

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
  SUBROUTINE MR_INTERP_XY_UV_U_BY_RCPRAC_AT_I0

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I

    I = 0

    IF( ACTIVITY(I+1, J ) == BEACTIVE ) THEN
      U( I , J ) = U_AT_I0( J )
    ELSE
      U( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_U_BY_RCPRAC_AT_I0

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
  SUBROUTINE MR_INTERP_XY_UV_U_BY_RCPRAC_AT_II

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I

    DO I = 1 , NI-1

      IF( ACTIVITY(I+1, J ) == BEACTIVE .AND. ACTIVITY( I , J ) == BEACTIVE ) THEN
        U( I , J ) = 0.5 * ( ( ALFA(I+1, J ,1) + ALFA( I , J ,1) ) * ( ZS(I+1, J ) - ZS( I , J ) )   &
        + ( BETA(I+1, J ,1) + BETA( I , J ,1) )   &
        ) / MU( I , J ) / HU( I , J )
      ELSE
        U( I , J ) = 0.0
      END IF

    END DO

  END SUBROUTINE MR_INTERP_XY_UV_U_BY_RCPRAC_AT_II

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
  SUBROUTINE MR_INTERP_XY_UV_U_BY_RCPRAC_AT_IN

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I

    I = NI

    IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
      U( I , J ) = UV( I , J ,1)
    ELSE
      U( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_U_BY_RCPRAC_AT_IN

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
  SUBROUTINE MR_INTERP_XY_UV_U_BY_LINEAR( NI , NJ , UV , U , U_AT_I0 )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ,1:2) :: UV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI,1:NJ) :: U

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(     1:NJ) :: U_AT_I0

    INTEGER(IJID_KIND) :: J

    DO J = 1 , NJ

      CALL MR_INTERP_XY_UV_U_BY_LINEAR_AT_I0
      CALL MR_INTERP_XY_UV_U_BY_LINEAR_AT_II
      CALL MR_INTERP_XY_UV_U_BY_LINEAR_AT_IN

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
  SUBROUTINE MR_INTERP_XY_UV_U_BY_LINEAR_AT_I0

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I

    I = 0

    IF( ACTIVITY(I+1, J ) == BEACTIVE ) THEN
      U( I , J ) = U_AT_I0( J )
    ELSE
      U( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_U_BY_LINEAR_AT_I0

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
  SUBROUTINE MR_INTERP_XY_UV_U_BY_LINEAR_AT_II

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I

    DO I = 1 , NI-1

      IF( ACTIVITY(I+1, J ) == BEACTIVE .AND. ACTIVITY( I , J ) == BEACTIVE ) THEN
        U( I , J ) = 0.5 * ( UV(I+1, J ,1) + UV( I , J ,1) )
      ELSE
        U( I , J ) = 0.0
      END IF

    END DO

  END SUBROUTINE MR_INTERP_XY_UV_U_BY_LINEAR_AT_II

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
  SUBROUTINE MR_INTERP_XY_UV_U_BY_LINEAR_AT_IN

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I

    I = NI

    IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
      U( I , J ) = UV( I , J ,1)
    ELSE
      U( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_U_BY_LINEAR_AT_IN

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
  SUBROUTINE MR_INTERP_XY_UV_U_TO_GET_V_AT_U( NI , NJ , UV , V_AT_U , V_AT_U_AT_I0 )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ,1:2) :: UV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI,1:NJ) :: V_AT_U

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(     1:NJ) :: V_AT_U_AT_I0

    INTEGER(IJID_KIND) :: J

    DO J = 1 , NJ

      CALL MR_INTERP_XY_UV_U_TO_GET_V_AT_U_AT_I0
      CALL MR_INTERP_XY_UV_U_TO_GET_V_AT_U_AT_II
      CALL MR_INTERP_XY_UV_U_TO_GET_V_AT_U_AT_IN

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
  SUBROUTINE MR_INTERP_XY_UV_U_TO_GET_V_AT_U_AT_I0

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I

    I = 0

    IF( ACTIVITY(I+1, J ) == BEACTIVE ) THEN
      V_AT_U( I , J ) = V_AT_U_AT_I0( J )
    ELSE
      V_AT_U( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_U_TO_GET_V_AT_U_AT_I0

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
  SUBROUTINE MR_INTERP_XY_UV_U_TO_GET_V_AT_U_AT_II

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I

    DO I = 1 , NI-1

      IF( ACTIVITY(I+1, J ) == BEACTIVE .OR. ACTIVITY( I , J ) == BEACTIVE ) THEN
        V_AT_U( I , J ) = ( UV(I+1, J ,2) * ACTIVITY(I+1, J ) + UV( I , J ,2) * ACTIVITY( I , J ) )   &
        / ( ACTIVITY(I+1, J ) + ACTIVITY( I , J ) )
      ELSE
        V_AT_U( I , J ) = 0.0
      END IF

    END DO

  END SUBROUTINE MR_INTERP_XY_UV_U_TO_GET_V_AT_U_AT_II

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
  SUBROUTINE MR_INTERP_XY_UV_U_TO_GET_V_AT_U_AT_IN

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I

    I = NI

    IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
      V_AT_U( I , J ) = UV( I , J ,2)
    ELSE
      V_AT_U( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_UV_U_TO_GET_V_AT_U_AT_IN

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
  SUBROUTINE MR_INTERP_XY_SS_U( NI , NJ , SS , SU , SU_AT_I0 )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ) :: SS
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(0:NI,1:NJ) :: SU

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(     1:NJ) :: SU_AT_I0

    INTEGER(IJID_KIND) :: J

    DO J = 1 , NJ

      CALL MR_INTERP_XY_SS_U_AT_I0
      CALL MR_INTERP_XY_SS_U_AT_II
      CALL MR_INTERP_XY_SS_U_AT_IN

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
  SUBROUTINE MR_INTERP_XY_SS_U_AT_I0

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I

    I = 0

    IF( ACTIVITY(I+1, J ) == BEACTIVE ) THEN
      SU( I , J ) = SU_AT_I0( J )
    ELSE
      SU( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_SS_U_AT_I0

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
  SUBROUTINE MR_INTERP_XY_SS_U_AT_II

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I

    DO I = 1 , NI-1

      IF( ACTIVITY(I+1, J ) == BEACTIVE .OR. ACTIVITY( I , J ) == BEACTIVE ) THEN
        SU( I , J ) = ( SS(I+1, J ) * ACTIVITY(I+1, J ) + SS( I , J ) * ACTIVITY( I , J ) )   &
        / ( ACTIVITY(I+1, J ) + ACTIVITY( I , J ) )
      ELSE
        SU( I , J ) = 0.0
      END IF

    END DO

  END SUBROUTINE MR_INTERP_XY_SS_U_AT_II

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
  SUBROUTINE MR_INTERP_XY_SS_U_AT_IN

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I

    I = NI

    IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
      SU( I , J ) = SS( I , J )
    ELSE
      SU( I , J ) = 0.0
    END IF

  END SUBROUTINE MR_INTERP_XY_SS_U_AT_IN

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
  SUBROUTINE MR_INTERP_XY_XUV_U( NI , NJ , XUV , XU )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ,1:2,1:2) :: XUV
    REAL   (GJRD_KIND) , INTENT(OUT) , DIMENSION(0:NI,1:NJ,1:2,1:2) :: XU

    REAL   (GJRD_KIND) , DIMENSION(1:2,1:2) , PARAMETER :: factor = RESHAPE((/+1.0,-1.0,-1.0,+1.0/),(/2,2/))

    INTEGER(IJID_KIND) :: J

    INTEGER :: DIM , DJM

    DO DJM = 1 , 2
      DO DIM = 1 , 2

        DO J = 1 , NJ

          CALL MR_INTERP_XY_XUV_U_AT_I0
          CALL MR_INTERP_XY_XUV_U_AT_II
          CALL MR_INTERP_XY_XUV_U_AT_IN

        END DO

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
  SUBROUTINE MR_INTERP_XY_XUV_U_AT_I0

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I

    I = 0

    XU( I , J ,DIM,DJM) = XUV(I+1, J ,DIM,DJM)

  END SUBROUTINE MR_INTERP_XY_XUV_U_AT_I0

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
  SUBROUTINE MR_INTERP_XY_XUV_U_AT_II

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I

    DO I = 1 , NI-1

      XU( I , J ,DIM,DJM) = 0.5 * ( XUV(I+1, J ,DIM,DJM) + XUV( I , J ,DIM,DJM) )

    END DO

  END SUBROUTINE MR_INTERP_XY_XUV_U_AT_II

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
  SUBROUTINE MR_INTERP_XY_XUV_U_AT_IN

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I

    I = NI

    XU( I , J ,DIM,DJM) = XUV( I , J ,DIM,DJM)

  END SUBROUTINE MR_INTERP_XY_XUV_U_AT_IN

  END SUBROUTINE MR_INTERP_XY_XUV_U

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

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ) :: ZS
    REAL   (CARD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ,1:2) :: ALFA , BETA

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI,0:NJ) :: V

    CALL MR_INTERP_XY_UV_V_BY_RCPRAC_AT_J0
    CALL MR_INTERP_XY_UV_V_BY_RCPRAC_AT_JJ
    CALL MR_INTERP_XY_UV_V_BY_RCPRAC_AT_JN

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
  SUBROUTINE MR_INTERP_XY_UV_V_BY_RCPRAC_AT_J0

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    J = 0

    DO I = 1 , NI

      V( I , J ) = 0.0

    END DO

  END SUBROUTINE MR_INTERP_XY_UV_V_BY_RCPRAC_AT_J0

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
  SUBROUTINE MR_INTERP_XY_UV_V_BY_RCPRAC_AT_JJ

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J


    DO J = 1 , NJ-1
      DO I = 1 , NI

        IF( ACTIVITY( I ,J+1) == BEACTIVE .AND. ACTIVITY( I , J ) == BEACTIVE ) THEN
          V( I , J ) = 0.5 * ( ( ALFA( I ,J+1,2) + ALFA( I , J ,2) ) * ( ZS( I ,J+1) - ZS( I , J ) )   &
          + ( BETA( I ,J+1,2) + BETA( I , J ,2) )   &
          ) / MV( I , J ) / HV( I , J )
        ELSE
          V( I , J ) = 0.0
        END IF

      END DO
    END DO

  END SUBROUTINE MR_INTERP_XY_UV_V_BY_RCPRAC_AT_JJ

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
  SUBROUTINE MR_INTERP_XY_UV_V_BY_RCPRAC_AT_JN

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    J = NJ

    DO I = 1 , NI

      V( I , J ) = 0.0

    END DO

  END SUBROUTINE MR_INTERP_XY_UV_V_BY_RCPRAC_AT_JN

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

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ,1:2) :: UV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI,0:NJ) :: V

    CALL MR_INTERP_XY_UV_V_BY_LINEAR_AT_J0
    CALL MR_INTERP_XY_UV_V_BY_LINEAR_AT_JJ
    CALL MR_INTERP_XY_UV_V_BY_LINEAR_AT_JN

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
  SUBROUTINE MR_INTERP_XY_UV_V_BY_LINEAR_AT_J0

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    J = 0

    DO I = 1 , NI

      V( I , J ) = 0.0

    END DO

  END SUBROUTINE MR_INTERP_XY_UV_V_BY_LINEAR_AT_J0

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
  SUBROUTINE MR_INTERP_XY_UV_V_BY_LINEAR_AT_JJ

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ-1
      DO I = 1 , NI

        IF( ACTIVITY( I ,J+1) == BEACTIVE .AND. ACTIVITY( I , J ) == BEACTIVE ) THEN
          V( I , J ) = 0.5 * ( UV( I ,J+1,2) + UV( I , J ,2) )
        ELSE
          V( I , J ) = 0.0
        END IF

      END DO
    END DO

  END SUBROUTINE MR_INTERP_XY_UV_V_BY_LINEAR_AT_JJ

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
  SUBROUTINE MR_INTERP_XY_UV_V_BY_LINEAR_AT_JN

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    J = NJ

    DO I = 1 , NI

      V( I , J ) = 0.0

    END DO

  END SUBROUTINE MR_INTERP_XY_UV_V_BY_LINEAR_AT_JN

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

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ,1:2) :: UV
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI,0:NJ) :: U_AT_V

    CALL MR_INTERP_XY_UV_V_TO_GET_U_AT_V_AT_J0
    CALL MR_INTERP_XY_UV_V_TO_GET_U_AT_V_AT_JJ
    CALL MR_INTERP_XY_UV_V_TO_GET_U_AT_V_AT_JN

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
  SUBROUTINE MR_INTERP_XY_UV_V_TO_GET_U_AT_V_AT_J0

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    J = 0

    DO I = 1 , NI

      IF( ACTIVITY( I ,J+1) == BEACTIVE ) THEN
        U_AT_V( I , J ) = UV( I ,J+1,1)
      ELSE
        U_AT_V( I , J ) = 0.0
      END IF

    END DO

  END SUBROUTINE MR_INTERP_XY_UV_V_TO_GET_U_AT_V_AT_J0

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
  SUBROUTINE MR_INTERP_XY_UV_V_TO_GET_U_AT_V_AT_JJ

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ-1
      DO I = 1 , NI

        IF( ACTIVITY( I ,J+1) == BEACTIVE .OR. ACTIVITY( I , J ) == BEACTIVE ) THEN
          U_AT_V( I , J ) = ( UV( I ,J+1,1) * ACTIVITY( I ,J+1) + UV( I , J ,1) * ACTIVITY( I , J ) )   &
          / ( ACTIVITY( I ,J+1) + ACTIVITY( I , J ) )
        ELSE
          U_AT_V( I , J ) = 0.0
        END IF

      END DO
    END DO

  END SUBROUTINE MR_INTERP_XY_UV_V_TO_GET_U_AT_V_AT_JJ

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
  SUBROUTINE MR_INTERP_XY_UV_V_TO_GET_U_AT_V_AT_JN

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    J = NJ

    DO I = 1 , NI

      IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
        U_AT_V( I , J ) = UV( I , J ,1)
      ELSE
        U_AT_V( I , J ) = 0.0
      END IF

    END DO

  END SUBROUTINE MR_INTERP_XY_UV_V_TO_GET_U_AT_V_AT_JN

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

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ) :: SS
    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI,0:NJ) :: SV

    CALL MR_INTERP_XY_SS_U_AT_J0
    CALL MR_INTERP_XY_SS_U_AT_JJ
    CALL MR_INTERP_XY_SS_U_AT_JN

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
  SUBROUTINE MR_INTERP_XY_SS_V_AT_J0

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    J = 0

    DO I = 1 , NI

      IF( ACTIVITY( I ,J+1) == BEACTIVE ) THEN
        SV( I , J ) = SS( I ,J+1)
      ELSE
        SV( I , J ) = 0.0
      END IF

    END DO

  END SUBROUTINE MR_INTERP_XY_SS_V_AT_J0

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
  SUBROUTINE MR_INTERP_XY_SS_V_AT_JJ

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ-1
      DO I = 1 , NI

        IF( ACTIVITY( I ,J+1) == BEACTIVE .OR. ACTIVITY( I , J ) == BEACTIVE ) THEN
          SV( I , J ) = ( SS( I ,J+1) * ACTIVITY( I ,J+1) + SS( I , J ) * ACTIVITY( I , J ) )   &
          / ( ACTIVITY( I ,J+1) + ACTIVITY( I , J ) )
        ELSE
          SV( I , J ) = 0.0
        END IF

      END DO
    END DO

  END SUBROUTINE MR_INTERP_XY_SS_V_AT_JJ

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
  SUBROUTINE MR_INTERP_XY_SS_V_AT_JN

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    J = NJ

    DO I = 1 , NI

      IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
        SV( I , J ) = SS( I , J )
      ELSE
        SV( I , J ) = 0.0
      END IF

    END DO

  END SUBROUTINE MR_INTERP_XY_SS_V_AT_JN

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
  SUBROUTINE MR_INTERP_XY_XUV_V( NI , NJ , XUV , XV )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (GJRD_KIND) , INTENT(IN ) , DIMENSION(1:NI,1:NJ,1:2,1:2) :: XUV
    REAL   (GJRD_KIND) , INTENT(OUT) , DIMENSION(1:NI,0:NJ,1:2,1:2) :: XV

    INTEGER :: DIM , DJM

    DO DJM = 1 , 2
      DO DIM = 1 , 2

        CALL MR_INTERP_XY_XUV_V_AT_J0
        CALL MR_INTERP_XY_XUV_V_AT_JJ
        CALL MR_INTERP_XY_XUV_V_AT_JN

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
  SUBROUTINE MR_INTERP_XY_XUV_V_AT_J0

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    J = 0

    DO I = 1 , NI

      XV( I , J ,DIM,DJM) = XUV( I ,J+1,DIM,DJM)

    END DO

  END SUBROUTINE MR_INTERP_XY_XUV_V_AT_J0

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
  SUBROUTINE MR_INTERP_XY_XUV_V_AT_JJ

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ-1
      DO I = 1 , NI

        XV( I , J ,DIM,DJM) = 0.5 * ( XUV( I ,J+1,DIM,DJM) + XUV( I , J ,DIM,DJM) )

      END DO
    END DO

  END SUBROUTINE MR_INTERP_XY_XUV_V_AT_JJ

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
  SUBROUTINE MR_INTERP_XY_XUV_V_AT_JN

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    J = NJ

    DO I = 1 , NI

      XV( I , J ,DIM,DJM) = XUV( I , J ,DIM,DJM)

    END DO

  END SUBROUTINE MR_INTERP_XY_XUV_V_AT_JN

  END SUBROUTINE MR_INTERP_XY_XUV_V

  END MODULE MR_MOD_INTERP_XY