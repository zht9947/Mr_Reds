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

    PUBLIC :: MR_FUNC_TBFUV_COMP
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
  FUNCTION MR_FUNC_HDS( D0 ) RESULT( HDS )
   !DIR$ ATTRIBUTES VECTOR : UNIFORM( D0 ) :: MR_FUNC_HDS
    IMPLICIT NONE

    REAL(PARD_KIND) , INTENT(IN ) :: D0

    REAL(FDRD_KIND) :: HDS

    HDS = 2.0 * (D0/ZR)

  END FUNCTION MR_FUNC_HDS

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
  FUNCTION MR_FUNC_TBFUV_COMP( TBFUV_MOD , D0 , H , SIGMA , UV_MOD , UV_COMP ) RESULT( TBFUV_COMP )
   !DIR$ ATTRIBUTES VECTOR : UNIFORM( D0 , SIGMA ) :: MR_FUNC_TBFUV_COMP
    IMPLICIT NONE

    REAL(PARD_KIND) , INTENT(IN ) :: D0
    REAL(FDRD_KIND) , INTENT(IN ) :: TBFUV_MOD , H

    REAL(PARD_KIND) , INTENT(IN ) :: SIGMA
    REAL(FDRD_KIND) , INTENT(IN ) :: UV_MOD , UV_COMP

    REAL(FDRD_KIND) :: TBFUV_COMP

    TBFUV_COMP = RBT * UV_COMP * UV_MOD /   &
    & MR_FUNC_CHEZY( TBFUV_MOD , MR_FUNC_HDS( D0 ) , H , SIGMA )**2

  END FUNCTION MR_FUNC_TBFUV_COMP

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
  FUNCTION MR_FUNC_HKS( TBFUV_MOD , D0 , TCRS , H ) RESULT( HKS )
   !DIR$ ATTRIBUTES VECTOR : UNIFORM( D0 , TCRS ) :: MR_FUNC_HKS
    IMPLICIT NONE

    REAL(PARD_KIND) , INTENT(IN ) :: D0 , TCRS
    REAL(FDRD_KIND) , INTENT(IN ) :: TBFUV_MOD , H

    REAL(FDRD_KIND) :: TRANSTAT
    REAL(FDRD_KIND) :: SDUNE , HDUNE

    REAL(FDRD_KIND) :: HKS

    TRANSTAT = MIN( MAX( TBFUV_MOD / TCRS - 1.0 , 0.0 ) , 25.0 )

    SDUNE = 0.015 * ( ( (D0/ZR) / H )**0.3 ) *   &
    & ( 1.0 - EXP( -0.5 * TRANSTAT ) ) * ( 25.0 - TRANSTAT )
    HDUNE = 7.3 * H * SDUNE

    HKS = MR_FUNC_HDS( D0 ) + 1.1 * HDUNE * ( 1.0 - EXP( -25.0 * SDUNE ) )

  END FUNCTION MR_FUNC_HKS

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
  FUNCTION MR_FUNC_TBUV_COMP( TBUV_MOD , TBFUV_MOD , D0 , TCRS , H , SIGMA , UV_MOD , UV_COMP ) RESULT( TBUV_COMP )
   !DIR$ ATTRIBUTES VECTOR : UNIFORM( D0 , TCRS , SIGMA ) :: MR_FUNC_TBUV_COMP
    IMPLICIT NONE

    REAL(PARD_KIND) , INTENT(IN ) :: D0 , TCRS
    REAL(FDRD_KIND) , INTENT(IN ) :: TBUV_MOD , TBFUV_MOD , H

    REAL(PARD_KIND) , INTENT(IN ) :: SIGMA
    REAL(FDRD_KIND) , INTENT(IN ) :: UV_MOD , UV_COMP

    REAL(FDRD_KIND) :: TBUV_COMP

    TBUV_COMP = RBT * UV_COMP * UV_MOD /   &
    & MR_FUNC_CHEZY( TBUV_MOD , MR_FUNC_HKS( TBFUV_MOD , D0 , TCRS , H ) , H , SIGMA )**2

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
  FUNCTION MR_FUNC_CHEZY( TBFUV_MOD , HDS , H , SIGMA ) RESULT( CHEZY )
   !DIR$ ATTRIBUTES VECTOR : UNIFORM( SIGMA ) :: MR_FUNC_CHEZY
    IMPLICIT NONE

    REAL(FDRD_KIND) , INTENT(IN ) :: TBFUV_MOD , HDS , H

    REAL(PARD_KIND) , INTENT(IN ) :: SIGMA

    REAL(FDRD_KIND) :: CHEZY

   !DIR$ FORCEINLINE
    CHEZY = LOG( (1.0+SIGMA) * H / HDS ) / KAR + MR_FUNC_BS( TBFUV_MOD , HDS )
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
  FUNCTION MR_FUNC_BS( TBFUV_MOD , HDS ) RESULT( BS )
   !DIR$ ATTRIBUTES VECTOR :: MR_FUNC_BS
    IMPLICIT NONE

    REAL(FDRD_KIND) , INTENT(IN ) :: TBFUV_MOD , HDS

    REAL(FDRD_KIND) :: BS

    REAL(FDRD_KIND) :: RESHEARS
    REAL(FDRD_KIND) :: CTMP1 , CTMP2

    RESHEARS = MAX( RET * HDS * SQRT( TBFUV_MOD ) , 1.0 )

    CTMP1 = LOG( RESHEARS )
    CTMP2 = CTMP1**2.55

    BS =           8.5         * ( 1.0 - EXP( -0.0594 * CTMP2 ) )   &
    &  + ( CTMP1 / KAR + 5.5 ) *         EXP( -0.0705 * CTMP2 )

  END FUNCTION MR_FUNC_BS

  END FUNCTION MR_FUNC_CHEZY

  END MODULE MR_MOD_FUNC_TBUV
