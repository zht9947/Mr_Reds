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
!                           QSUVR    :    REFERENCE TRANSPORT FLUX, QSUVR = R0 * COR * XYR * ZR
!                             KIR    :    REFERENCE TURBULENCE KINETIC ENERGY, KIR = COR * UVR * ZR
!                             DIR    :    REFERENCE TURBULENCE DISSIPATION RATE, DIR = COR * UVR * UVR
!                             VZR    :    REFERENCE EDDY KINEMATIC VISCOSITY
!                             DZR    :    REFERENCE EDDY KINEMATIC DIFFUSIVITY
!                              RR    :    REFERENCE WATER-SEDIMENT MIXTURE DENSITY,
!                                         GENERALLY AS MAXIMUM WATER-SEDIMENT MIXTURE DENSITY EXPECTED
!
!                                      \\ ABOVE ARE REFERENCE PARAMETERS
!
!                              RB    :    ROSSBY NUMBER, RB = UVR / ( COR * XYR )
!                             RBT    :    ROSSBY NUMBER FOR SHEAR STRESS, RBT = UVR / ( COR * ZR )
!                       EKXY, EKZ    :    HORIZONTAL & VERTICAL EKMAN NUMBER, EKXY = VZR / ( COR * XYR * XYR ), EKZ = VZR / ( COR * ZR * ZR )
!                       SCXY, SCZ    :    HORIZONTAL & VERTICAL SCHMIDT NUMBER, SCXY = DZR / ( COR * XYR * XYR ), SCZ = DZR / ( COR * ZR * ZR )
!                              RE    :    REYNOLDS NUMBER, RE = UVR * ZR / V0
!                             RET    :    REYNOLDS NUMBER FOR SHEAR STRESS, RET = RE / SQRT( RBT )
!                              FR    :    SQUARED FROUDE NUMBER, FR = UVR * UVR / ( GR * ZR )
!                             FRD    :    SQUARED DENSIMETRIC FROUDE NUMBER, FRD = FR * R0 / ( RR - R0 )
!                           ALPAR    :    ALPHA PARAMETER, ALPAR = FR / RB
!                            BPAR    :    BETA PARAMETER, BPAR = RB * RB / FR
!
!                                      \\ ABOVE ARE DIMENSIONLESS COMBINATIONS
!
!   // BELOW ARE CASE CLASSIFIED PARAMETERS
!
!                          D0, DS    :    GRAIN-SIZE; DIMENSIONLESS GRAIN-SIZE
!                            TCRS    :    DIMENSIONLESS CRITICAL SHEAR STRESS
!                           WALLS    :    DIMENSIONLESS FALL VELOCITY
!                             RBS    :    ROSSBY NUMBERS FOR FALL VELOCITY
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
    REAL   (PARD_KIND) :: QSUVR
    REAL   (PARD_KIND) :: KIR
    REAL   (PARD_KIND) :: DIR
    REAL   (PARD_KIND) :: VZR
    REAL   (PARD_KIND) :: DZR
    REAL   (PARD_KIND) :: RR

    REAL   (PARD_KIND) :: RB
    REAL   (PARD_KIND) :: RBT
    REAL   (PARD_KIND) :: EKXY , EKZ
    REAL   (PARD_KIND) :: SCXY , SCZ
    REAL   (PARD_KIND) :: RE
    REAL   (PARD_KIND) :: RET
    REAL   (PARD_KIND) :: FR
    REAL   (PARD_KIND) :: FRD
    REAL   (PARD_KIND) :: ALPAR
    REAL   (PARD_KIND) :: BPAR

    REAL   (PARD_KIND) :: D0 , DS
    REAL   (PARD_KIND) :: TCRS
    REAL   (PARD_KIND) :: WALLS
    REAL   (PARD_KIND) :: RBS

    REAL   (PARD_KIND) , ALLOCATABLE , DIMENSION(:) :: SIGMA

    REAL   (PARD_KIND) :: DSIGMA

  END MODULE MR_DEF_CONSTS_N_REF_PARS