!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE) MR_DEF_CONSTS_N_REF_PARS
!
! PURPOSE:
!
!   TO DEFINE THE CONSTANTS AND REFERENCE PARAMETERS, WHICH ARE MAINLY USED TO NONDIMENSIONALIZE THE SYSTEM OF FORMULAE, EQUATIONS
!   AND AUXILIARY RELATIONS SO AS TO ELIMINATE THE DIMENSION EFFECT DUE TO THE OVERLARGE OR OVERSMALL SCALE OF INPUT DATA. THESE
!   PARAMETERS ARE SELECTED INDEPENDENT OF TIME AND LOCATION, THAT IS, FOR A DEFINITE PROJECT CASE, THEY HAVE DEFINITE VALUES.
!   ALTHOUGH THESE PARAMETERS HAVE NO DISTRIBUTION ON THE GRID, SOME OF THEM MAY BE STILL CLASSIFIED BY LAYER (EG. SIGMA COORDINATE)
!   OR BY SEDIMENT GRIAN-SIZE (EG. SEDIMENT CRITICAL SHEAR STRESS, FALL VELOCITY, ETC.). IN ADDITION, THE SIGMA THICKNESS AS A
!   COMPUTATIONAL CONSTANT IS ALSO DEFINED HERE FOR IT HAS THE SAME PRECISION AS THE OTHER PARAMETERS.
!
! DEFINITION OF VARIABLES:
!
!                              R0    :    WATER DENSITY
!                              V0    :    WATER KINEMATIC VISCOSITY
!
!                                      \\ ABOVE ARE WATER PHYSICAL PROPERTIES
!
!                              SS    :    SEDIMENT SPECIFIC GRAVITY
!                              PS    :    SEDIMENT POROSITY
!
!                                      \\ ABOVE ARE SEDIMENT PHYSICAL PROPERTIES
!
!                             KAR    :    VON KARMAN CONSTANT
!                             COR    :    CORIOLIS FREQUENCY
!                              GR    :    GRAVITATIONAL ACCELERATION
!
!                                      \\ ABOVE ARE PHYSICAL CONSTANTS
!
!                             XYR    :    REFERENCE HORIZONTAL DIMENSION,
!                                         GENERALLY AS AVERAGE CELL DISTANCE
!                              ZR    :    REFERENCE VERTICAL DIMENSION,
!                                         GENERALLY AS AVERAGE DEPTH
!                             UVR    :    REFERENCE HORIZONTAL VELOCITY,
!                                         GENERALLY AS AVERAGE HORIZONTAL VELOCITY MAGNITUDE
!                              WR    :    REFERENCE VERTICAL VELOCITY, WR = UVR / XYR * ZR
!                              SR    :    REFERENCE FREE SURFACE ELEVATION, SR = COR * UVR * XYR / GR
!                            TUVR    :    REFERENCE SHEAR STRESS, TUVR = R0 * COR * UVR * ZR
!                            QUVR    :    REFERENCE TRANSPORT FLUX, QUVR = COR * XYR * ZR
!                             KIR    :    REFERENCE TURBULENCE KINETIC ENERGY, KIR = COR * UVR * ZR
!                             DIR    :    REFERENCE TURBULENCE DISSIPATION RATE, DIR = COR * UVR2
!                       VXYR, VZR    :    REFERENCE HORIZONTAL & VERTICAL EDDY KINEMATIC VISCOSITY
!                       DXYR, DZR    :    REFERENCE HORIZONTAL & VERTICAL EDDY KINEMATIC DIFFUSIVITY
!                              RR    :    REFERENCE WATER-SEDIMENT MIXTURE DENSITY,
!                                         GENERALLY AS MAXIMUM WATER-SEDIMENT MIXTURE DENSITY EXPECTED
!
!                                      \\ ABOVE ARE REFERENCE PARAMETERS
!
!                              RB    :    ROSSBY NUMBER, RB = UVR / ( COR * XYR )
!                             RBT    :    ROSSBY NUMBER OF SHEAR STRESS, RBT = UVR / ( COR * ZR )
!                       EKXY, EKZ    :    HORIZONTAL & VERTICAL EKMAN NUMBER, EKXY = VXYR / ( COR * XYR2 ), EKZ = VZR / ( COR * ZR2 )
!                       SCXY, SCZ    :    HORIZONTAL & VERTICAL SCHMIDT NUMBER, SCXY = DXYR / ( COR * XYR2 ), SCZ = DZR / ( COR * ZR2 )
!                             FR2    :    SQUARED FROUDE NUMBER, FR2 = UVR2 / ( GR * ZR )
!                            FRD2    :    SQUARED DENSIMETRIC FROUDE NUMBER, FRD2 = FR2 * R0 / ( RR - R0 )
!                           ALPAR    :    ALPHA PARAMETER, ALPAR = FR2 / RB
!                            BPAR    :    BETA PARAMETER, BPAR = RB2 / FR2
!
!                                      \\ ABOVE ARE DIMENSIONLESS COMBINATIONS
!
!   // BELOW ARE CASE CLASSIFIED PARAMETERS
!
!                          D0, DS    :    GRAIN-SIZE AND DIMENSIONLESS GRAIN-SIZE NUMBER
!                            TCRS    :    DIMENSIONLESS CRITICAL SHEAR STRESS
!                              WS    :    DIMENSIONLESS FALL VELOCITY
!                             RBS    :    ROSSBY NUMBERS OF FALL VELOCITY
!
!                                      \\ ABOVE ARE SEDIMENT GRIAN-SIZE CLASSIFIED PARAMETERS
!
!                           SIGMA    :    SIGMA COORDINATE
!
!                                      \\ ABOVE ARE LAYER CLASSFIED PARAMETERS
!
!   // BELOW IS CONSTANT SIGMA THICKNESS
!
!                          DSIGMA    :    SIGMA THICKNESS
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-03-24    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_DEF_CONSTS_N_REF_PARS

    USE MR_KINDS

    IMPLICIT NONE

    REAL   (PARD_KIND) :: R0
    REAL   (PARD_KIND) :: V0

    REAL   (PARD_KIND) :: SS
    REAL   (PARD_KIND) :: PS

    REAL   (PARD_KIND) :: KAR
    REAL   (PARD_KIND) :: COR
    REAL   (PARD_KIND) :: GR

    REAL   (PARD_KIND) :: XYR
    REAL   (PARD_KIND) :: ZR
    REAL   (PARD_KIND) :: UVR
    REAL   (PARD_KIND) :: WR
    REAL   (PARD_KIND) :: SR
    REAL   (PARD_KIND) :: TUVR
    REAL   (PARD_KIND) :: QUVR
    REAL   (PARD_KIND) :: KIR
    REAL   (PARD_KIND) :: DIR
    REAL   (PARD_KIND) :: VXYR , VZR
    REAL   (PARD_KIND) :: DXYR , DZR
    REAL   (PARD_KIND) :: RR

    REAL   (PARD_KIND) :: RB
    REAL   (PARD_KIND) :: RBT
    REAL   (PARD_KIND) :: EKXY , EKZ
    REAL   (PARD_KIND) :: SCXY , SCZ
    REAL   (PARD_KIND) :: FR2
    REAL   (PARD_KIND) :: FRD2
    REAL   (PARD_KIND) :: ALPAR
    REAL   (PARD_KIND) :: BPAR

    REAL   (PARD_KIND) :: D0 , DS
    REAL   (PARD_KIND) :: TCRS
    REAL   (PARD_KIND) :: WS
    REAL   (PARD_KIND) :: RBS

    REAL   (PARD_KIND) , ALLOCATABLE , DIMENSION(:) :: SIGMA

    REAL   (PARD_KIND) :: DSIGMA

  END MODULE MR_DEF_CONSTS_N_REF_PARS