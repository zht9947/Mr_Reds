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
  MODULE MR_MOD_INIT_CONSTS_N_REF_PARS

    USE MR_KINDS

    USE MR_DEF_RANKS

    USE MR_DEF_CONSTS_N_REF_PARS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_INIT_CONSTS_N_REF_PARS

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
! DEFREADION OF VARIABLES:
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
  SUBROUTINE MR_INIT_CONSTS_N_REF_PARS( ERROR , ERRMSG )

    USE MR_MOD_FUNC_DS
    USE MR_MOD_FUNC_TCRS

    IMPLICIT NONE

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER(KKID_KIND) :: K

    INTEGER(KSID_KIND) :: KS

    COR2 = COR2 / COR

  ! CALCULATE REFERENCE PARAMETERS
    WR = UVR / ( XYR / ZR )
    SR = COR * UVR * XYR / GR
    TUVR = R0 * COR * UVR * ZR
    QSUVR = R0 * COR * XYR * ZR
    KIR = COR * UVR * ZR
    DIR = COR * UVR * UVR

  ! CALCULATE DIMENSIONLESS COMBINATIONS
    RB = UVR / ( COR * XYR )
    RBT = UVR / ( COR * ZR )
    EKXY = VZR / ( COR * XYR * XYR )
    EKZ = VZR / ( COR * ZR * ZR )
    SCXY = DZR / ( COR * XYR * XYR )
    SCZ = DZR / ( COR * ZR * ZR )
    RE = UVR * ZR / V0
    RET = RE / SQRT( RBT )
    FR = UVR * UVR / ( GR * ZR )
    FRD = FR * R0 / ( RR - R0 )
    BPAR = RB * RB / FR
    SURPAR = FR / RB
    SLOPEPAR = ( XYR / ZR ) / SURPAR

   !BLOCK
    IF( NK <= 0 ) THEN
      ERROR = - 1
      ERRMSG = "Invalid number of layers"
      RETURN
    ELSE
    ! CALCULATE DSIGMA
      DSIGMA = 1.0_PARD_KIND / NK
    ! CALCULATE SIGMA COORDINATES
      DO K = 1 , NK
        SIGMA( K ) = (K-NK-0.5_PARD_KIND) * DSIGMA
      END DO
    END IF
   !END BLOCK

   !BLOCK
    IF( NKS <= 0 ) THEN
      ERROR = - 1
      ERRMSG = "Invalid number of sediment sizes"
      RETURN
    ELSE
    ! CALCULATE DS, TCRS, WS & RBS
     !DO KS = 1 , NKS
        DS = MR_FUNC_DS( D0 )
        TCRS = MR_FUNC_TCRS( D0 , DS )
     !END DO
    END IF
   !END BLOCK

  END SUBROUTINE MR_INIT_CONSTS_N_REF_PARS

  END MODULE MR_MOD_INIT_CONSTS_N_REF_PARS