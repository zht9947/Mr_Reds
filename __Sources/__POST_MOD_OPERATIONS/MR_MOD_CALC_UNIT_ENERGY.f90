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
  MODULE MR_MOD_CALC_UNIT_ENERGY

    USE MR_KINDS

    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_FIELD_VARS
    USE MR_DEF_ACTIVITY

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_CALC_UNIT_ENERGY

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
  SUBROUTINE MR_CALC_UNIT_ENERGY( NI , NJ , NK , UNIT_ENERGY )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: UNIT_ENERGY

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM
    INTEGER(KKID_KIND) :: K

    !BLOCK
      DO DIM = 1 , 2

        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            !BLOCK
              UNIT_ENERGY( I , J ,DIM) = 0.0
            !END BLOCK
          END DO
        END DO

      END DO
    !END BLOCK

    DO K = 1 , NK

      DO DIM = 1 , 2

        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            IF( ACTIVITY( I , J ) == BEACTIVE ) THEN
              UNIT_ENERGY( I , J ,DIM) = UNIT_ENERGY( I , J ,DIM) +   &
              & DSIGMA * H( I , J ) *   &
              & 0.5 * GUV( I , J ,DIM,DIM) * UV( I , J ,DIM, K ) * UV( I , J ,DIM, K )
            END IF
          END DO
        END DO

      END DO

    END DO

  END SUBROUTINE MR_CALC_UNIT_ENERGY

  END MODULE MR_MOD_CALC_UNIT_ENERGY