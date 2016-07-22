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
  MODULE MR_MOD_CALC_GRAD_XY

    USE MR_KINDS

    USE MR_DEF_ACTIVITY

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CALC_GRAD_XY_UV , MR_CALC_GRAD_XY_SS
    PUBLIC :: MR_CALC_REDC_GRAD_XY_UV , MR_CALC_REDC_GRAD_XY_SS
    PUBLIC :: MR_CALC_BY_INTERP_GRAD_XY_SS

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
  SUBROUTINE MR_CALC_GRAD_XY_UV( NI , NJ , UU , VV , GRAD_XY_UV )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(FDRD_KIND),1:NJ,1:2) :: UU
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(FDRD_KIND),0:NJ,1:2) :: VV

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,1:2,1:2) :: GRAD_XY_UV

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

  ! U DIRECTION
    DO DIM = 1 , 2

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
            GRAD_XY_UV( I , J ,DIM,1) = UU( I , J ,DIM) - UU(I-1, J ,DIM)
          ELSE
            GRAD_XY_UV( I , J ,DIM,1) = 0.0
          END IF
        END DO
      END DO

    END DO

  ! V DIRECTION
    DO DIM = 1 , 2

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
            GRAD_XY_UV( I , J ,DIM,2) = VV( I , J ,DIM) - VV( I ,J-1,DIM)
          ELSE
            GRAD_XY_UV( I , J ,DIM,2) = 0.0
          END IF
        END DO
      END DO

    END DO

  END SUBROUTINE MR_CALC_GRAD_XY_UV

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
  SUBROUTINE MR_CALC_REDC_GRAD_XY_UV( NI , NJ , UU , VV , REDC_GRAD_XY_UV )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(FDRD_KIND),1:NJ,1:2) :: UU
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(FDRD_KIND),0:NJ,1:2) :: VV

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,1:2) :: REDC_GRAD_XY_UV

    REAL   (FDRD_KIND) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,1:2,1:2) :: GRAD_XY_UV

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    CALL MR_CALC_GRAD_XY_UV( NI , NJ , UU , VV , GRAD_XY_UV )

    DO DIM = 1 , 2

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          REDC_GRAD_XY_UV( I , J ,DIM) = GRAD_XY_UV( I , J ,DIM,1) + GRAD_XY_UV( I , J ,DIM,2)
        END DO
      END DO

    END DO

  END SUBROUTINE MR_CALC_REDC_GRAD_XY_UV

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
  SUBROUTINE MR_CALC_GRAD_XY_SS( NI , NJ , SU , SV , GRAD_XY_SS )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(FDRD_KIND),1:NJ) :: SU
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(FDRD_KIND),0:NJ) :: SV

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,1:2) :: GRAD_XY_SS

    INTEGER(IJID_KIND) :: I , J

  ! U DIRECTION
    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
          GRAD_XY_SS( I , J ,1) = SU( I , J ) - SU(I-1, J )
        ELSE
          GRAD_XY_SS( I , J ,1) = 0.0
        END IF
      END DO
    END DO

  ! V DIRECTION
    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
          GRAD_XY_SS( I , J ,2) = SV( I , J ) - SV( I ,J-1)
        ELSE
          GRAD_XY_SS( I , J ,2) = 0.0
        END IF
      END DO
    END DO

  END SUBROUTINE MR_CALC_GRAD_XY_SS

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
  SUBROUTINE MR_CALC_REDC_GRAD_XY_SS( NI , NJ , SU , SV , REDC_GRAD_XY_SS )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(0:NI0(FDRD_KIND),1:NJ) :: SU
    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(FDRD_KIND),0:NJ) :: SV

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(FDRD_KIND),1:NJ) :: REDC_GRAD_XY_SS

    REAL   (FDRD_KIND) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,1:2) :: GRAD_XY_SS

    INTEGER(IJID_KIND) :: I , J

    CALL MR_CALC_GRAD_XY_SS( NI , NJ , SU , SV , GRAD_XY_SS )

    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        REDC_GRAD_XY_SS( I , J ) = GRAD_XY_SS( I , J ,1) + GRAD_XY_SS( I , J ,2)
      END DO
    END DO

  END SUBROUTINE MR_CALC_REDC_GRAD_XY_SS

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
  SUBROUTINE MR_CALC_BY_INTERP_GRAD_XY_SS( NI , NJ , SS , GRAD_XY_SS )

    USE MR_MOD_INTERP_XY

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(IN ) , DIMENSION(1:NI1(FDRD_KIND),1:NJ) :: SS

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(FDRD_KIND),1:NJ,1:2) :: GRAD_XY_SS

    REAL   (FDRD_KIND) , DIMENSION(0:NI0(FDRD_KIND),1:NJ) :: SU
    REAL   (FDRD_KIND) , DIMENSION(1:NI1(FDRD_KIND),0:NJ) :: SV

    CALL MR_INTERP_XY_SS_U ( NI , NJ , SS , SU )
    CALL MR_INTERP_XY_SS_V ( NI , NJ , SS , SV )

    CALL MR_CALC_GRAD_XY_SS( NI , NJ , SU , SV , GRAD_XY_SS )

  END SUBROUTINE MR_CALC_BY_INTERP_GRAD_XY_SS

  END MODULE MR_MOD_CALC_GRAD_XY