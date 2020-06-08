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
  MODULE MR_MOD_FUNC_HKS

    USE MR_KINDS

    USE MR_DEF_CONSTS_N_REF_PARS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_FUNC_HDKS
    PUBLIC :: MR_FUNC_HKS

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
  FUNCTION MR_FUNC_HDKS( DKS ) RESULT( HDKS )

   !DIR$ ATTRIBUTES VECTOR : UNIFORM( DKS ) :: MR_FUNC_HDKS

    IMPLICIT NONE

    REAL(PARD_KIND) , INTENT(IN ) :: DKS

    REAL(FDRD_KIND) :: HDKS

    HDKS = 3.0 * ( DKS * DSPAR )

  END FUNCTION MR_FUNC_HDKS

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
  FUNCTION MR_FUNC_HKS( DKS , TCRKS , H , TBFUV_MOD ) RESULT( HKS )

   !DIR$ ATTRIBUTES VECTOR : UNIFORM( DKS , TCRKS ) :: MR_FUNC_HKS

    IMPLICIT NONE

    REAL(PARD_KIND) , INTENT(IN ) :: DKS , TCRKS
    REAL(FDRD_KIND) , INTENT(IN ) :: H , TBFUV_MOD

    REAL(PARD_KIND) :: TRANS_STATE
    REAL(PARD_KIND) :: SDUNE , HDUNE

    REAL(FDRD_KIND) :: HKS

    TRANS_STATE = MIN( MAX( TBFUV_MOD / TCRKS - 1.0 , 0.0 ) , 25.0 )

    SDUNE = 0.015 * ( ( ( DKS * DSPAR ) / H )**0.3 ) *   &
    & ( 1.0 - EXP( -0.5 * TRANS_STATE ) ) * ( 25.0 - TRANS_STATE )
    HDUNE = 7.3 * H * SDUNE

    HKS = MR_FUNC_HDKS( DKS ) + 1.1 * HDUNE * ( 1.0 - EXP( -25.0 * SDUNE ) )

  END FUNCTION MR_FUNC_HKS

  END MODULE MR_MOD_FUNC_HKS
