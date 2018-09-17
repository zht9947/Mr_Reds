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
  MODULE MR_MOD_INIT_CONSTS_N_REF_PARS_DEFAULT

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_CONSTS_N_REF_PARS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_INIT_CONSTS_N_REF_PARS_DEFAULT

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
  SUBROUTINE MR_INIT_CONSTS_N_REF_PARS_DEFAULT( ERROR , ERRMSG )

    USE MR_MOD_FUNC_DS
    USE MR_MOD_FUNC_TCRS

    IMPLICIT NONE

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER(KKID_KIND) :: K

    R0  = 1.000E+3
    V0  = 1.00E-6

    SS  = 2.65
    PS  = 0.40

    KAR = 0.40
    COR = 1.0
    GR  = 9.80665

    XYR = 1.0
    ZR  = 1.0
    UVR = 1.0
    VZR = 1.00E-6
    RR  = 1.200E+3
 
  ! CALCULATE REFERENCE PARAMETERS
    WR = UVR / XYR * ZR
    SR = COR * UVR * XYR / GR
    TUVR = R0 * COR * UVR * ZR
    QSUVR = R0 * COR * XYR * ZR
    KIR = COR * UVR * ZR
    DIR = COR * UVR * UVR
    DZR = VZR

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
    IF( .NOT. ( NK > 0 ) ) THEN
      ERROR = - 1
      ERRMSG = "Number of layers is identified with zero, "   &
      //"please check the command arguments"
      RETURN
    ELSE
    ! CALCULATE DSIGMA
      DSIGMA = 1.0_PARD_KIND / NK
    ! CALCULATE SIGMA COORDINATES
      SIGMA(NK) = - 0.5_PARD_KIND * DSIGMA
      DO K = NK-1 , 1 , -1
        SIGMA( K ) = SIGMA(K+1) - DSIGMA
      END DO
    END IF
   !END BLOCK

  ! CALCULATE DS, TCRS, WS & RBS
    DS = MR_FUNC_DS( D0 )
    TCRS = MR_FUNC_TCRS( D0 , DS )

  END SUBROUTINE MR_INIT_CONSTS_N_REF_PARS_DEFAULT

  END MODULE MR_MOD_INIT_CONSTS_N_REF_PARS_DEFAULT