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

    REAL   (FDRD_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:NK) :: UNIT_ENERGY

    INTEGER(IJID_KIND) :: I , J
    INTEGER(KKID_KIND) :: K

    DO K = 1 , NK

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          IF( ACTIVITY( I , J ) == NOACTIVE ) THEN
            UNIT_ENERGY( I , J , K ) = 0.0
          ELSE
            UNIT_ENERGY( I , J , K ) = 0.5 * GUV( I , J ,1,1) * UV( I , J ,1, K ) * UV( I , J ,1, K )
          END IF
        END DO
      END DO

    END DO

  END SUBROUTINE MR_CALC_UNIT_ENERGY

  END MODULE MR_MOD_CALC_UNIT_ENERGY