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
  MODULE MR_MOD_FUNC_TBUV

    USE MR_KINDS

    USE MR_DEF_CONSTS_N_REF_PARS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_FUNC_TBUV_COMP

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
  FUNCTION MR_FUNC_TBUV_COMP( SIGMA , H , HKS , HKS2 , TBUV_MOD2 , UV_MOD , UV_COMP ) RESULT( TBUV_COMP )

   !DIR$ ATTRIBUTES VECTOR : UNIFORM( SIGMA ) :: MR_FUNC_TBUV_COMP

    IMPLICIT NONE

    REAL(PARD_KIND) , INTENT(IN ) :: SIGMA

    REAL(FDRD_KIND) , INTENT(IN ) :: H , HKS
    REAL(FDRD_KIND) , INTENT(IN ) :: HKS2 , TBUV_MOD2

    REAL(FDRD_KIND) , INTENT(IN ) :: UV_MOD , UV_COMP

    REAL(FDRD_KIND) :: TBUV_COMP

    TBUV_COMP = RBT * UV_COMP * UV_MOD /   &
    & MAX( MR_FUNC_CHEZY( SIGMA , H , HKS , HKS2 , TBUV_MOD2 ) , SQRT(EPSILON(H)) )**2

  END FUNCTION MR_FUNC_TBUV_COMP

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
  FUNCTION MR_FUNC_CHEZY( SIGMA , H , HKS , HKS2 , TBUV_MOD2 ) RESULT( CHEZY )

   !DIR$ ATTRIBUTES VECTOR : UNIFORM( SIGMA ) :: MR_FUNC_CHEZY

    IMPLICIT NONE

    REAL(PARD_KIND) , INTENT(IN ) :: SIGMA

    REAL(FDRD_KIND) , INTENT(IN ) :: H , HKS
    REAL(FDRD_KIND) , INTENT(IN ) :: HKS2 , TBUV_MOD2

    REAL(FDRD_KIND) :: CHEZY

   !DIR$ FORCEINLINE
    CHEZY = LOG( (1.0+SIGMA) * H / HKS ) / KAR + MR_FUNC_ROUGHNESS_STATE( HKS2 , TBUV_MOD2 )
   !END DIR$ FORCEINLINE

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
  FUNCTION MR_FUNC_ROUGHNESS_STATE( HKS2 , TBUV_MOD2 ) RESULT( ROUGHNESS_STATE )

   !DIR$ ATTRIBUTES VECTOR :: MR_FUNC_ROUGHNESS_STATE

    IMPLICIT NONE

    REAL(FDRD_KIND) , INTENT(IN ) :: HKS2 , TBUV_MOD2

    REAL(PARD_KIND) :: RESHEARS
    REAL(PARD_KIND) :: CTMP1 , CTMP2

    REAL(FDRD_KIND) :: ROUGHNESS_STATE

    RESHEARS = MAX( RET * HKS2 * SQRT( TBUV_MOD2 ) , 1.0 )

    CTMP1 = LOG( RESHEARS )
    CTMP2 = CTMP1**2.55

    ROUGHNESS_STATE =   &
    &           8.5         * ( 1.0 - EXP( -0.0594 * CTMP2 ) ) +   &
    & ( CTMP1 / KAR + 5.5 ) *         EXP( -0.0705 * CTMP2 )

  END FUNCTION MR_FUNC_ROUGHNESS_STATE

  END FUNCTION MR_FUNC_CHEZY

  END MODULE MR_MOD_FUNC_TBUV
