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
  MODULE MR_MOD_UPDT_VZW

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_FIELD_VARS

    USE MR_MCS_K_EPS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_UPDT_VZW

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
  SUBROUTINE MR_UPDT_VZW

    USE MR_MOD_FUNC_VZWW

    USE MR_MOD_INTERP_Z

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER(KKID_KIND) :: K

    ALLOCATE( VZWW(1:NI1(NI,FDRD_KIND),1:NJ,1:NK) )
      DO K = 1 , NK
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            VZWW( I , J , K ) = MR_FUNC_VZWW( V0 , KI( I , J , K ) , DI( I , J , K ) )
          END DO
        END DO
      END DO

      CALL MR_INTERP_Z_SS_W( NI , NJ , NK , VZWW , VZW )

    DEALLOCATE( VZWW )

  END SUBROUTINE MR_UPDT_VZW

  END MODULE MR_MOD_UPDT_VZW