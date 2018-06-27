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
  MODULE MR_MOD_UPDT_VZWW

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_FIELD_VARS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_UPDT_VZWW

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
  SUBROUTINE MR_UPDT_VZWW

    USE MR_MOD_FUNC_VZWW

    USE MR_MOD_INTERP_XY
    USE MR_MOD_INTERP_Z

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER(KKID_KIND) :: K

    DO K = 1 , NK

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          VZWW( I , J , K ) = MR_FUNC_VZWW( V0 , KI( I , J , K ) , DI( I , J , K ) )
        END DO
      END DO

      CALL MR_INTERP_XY_SS_U( NI , NJ , VZWW(:,:, K ) , VXYU(:,:, K ) )
      CALL MR_INTERP_XY_SS_V( NI , NJ , VZWW(:,:, K ) , VXYV(:,:, K ) )

    END DO

    CALL MR_INTERP_Z_SS_W( NI , NJ , NK , VZWW , VZW )

  END SUBROUTINE MR_UPDT_VZWW

  END MODULE MR_MOD_UPDT_VZWW