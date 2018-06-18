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
  MODULE MR_MOD_FUNC_VZWW

    USE MR_KINDS

    USE MR_DEF_CONSTS_N_REF_PARS

    USE MR_MOC_K_EPS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_FUNC_VZWW

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
  FUNCTION MR_FUNC_VZWW( V0 , KI , DI ) RESULT( VZWW )
   !DIR$ ATTRIBUTES VECTOR : UNIFORM( V0 ) :: MR_FUNC_VZWW
    IMPLICIT NONE

    REAL(PARD_KIND) , INTENT(IN ) :: V0
    REAL(FDRD_KIND) , INTENT(IN ) :: KI , DI

    REAL(FDRD_KIND) :: VZWW

    VZWW = V0 / VZR + CV0 / EKZ * KI * KI / MAX( DI , EPSILON(DI) )

  END FUNCTION MR_FUNC_VZWW

  END MODULE MR_MOD_FUNC_VZWW