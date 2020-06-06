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
!                            COR2    :    CORIOLIS FREQUENCY
!                              GR    :    GRAVITATIONAL ACCELERATION
!
!                                      \\ ABOVE ARE PHYSICAL CONSTANTS
!
!                             COR    :    REFERENCE FREQUENCY
!                             XYR    :    REFERENCE HORIZONTAL DIMENSION,
!                                         GENERALLY AS AVERAGE CELL DISTANCE
!                              ZR    :    REFERENCE VERTICAL DIMENSION,
!                                         GENERALLY AS AVERAGE DEPTH
!                             UVR    :    REFERENCE HORIZONTAL VELOCITY,
!                                         GENERALLY AS AVERAGE HORIZONTAL VELOCITY MAGNITUDE
!                              WR    :    REFERENCE VERTICAL VELOCITY, WR = UVR / ( XYR / ZR )
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
!                            BPAR    :    BETA PARAMETER, BPAR = RB * RB / FR
!                          SURPAR    :    PARAMETER THAT MEASURES FREE SURFACE ELEVATION AS BED ELEVATION, SURPAR = FR / RB
!                        SLOPEPAR    :    PARAMETER THAT MEASURES BED SLOPE AS FREE SURFACE SLOPE, SLOPEPAR = ( XYR / ZR ) / SURPAR
!                           DSPAR    :    PARAMETER THAT MEASURES GRAIN SIZE AS BED ROUGHNESS, DSPAR = ( ( V0 * V0) / ( GR * (SS-1.0) ) )**(1/3) / ZR
!
!                                      \\ ABOVE ARE DIMENSIONLESS COMBINATIONS
!
!   // BELOW IS CONSTANT SIGMA THICKNESS
!
!                          DSIGMA    :    SIGMA THICKNESS
!
!   // BELOW ARE RANKED PARAMETERS
!
!                           SIGMA    :    SIGMA COORDINATE
!
!                                      \\ ABOVE ARE LAYER RANKED PARAMETERS
!
!                              D0    :    GRAIN SIZE
!                              DS    :    DIMENSIONLESS GRAIN SIZE
!                            TCRS    :    DIMENSIONLESS CRITICAL SHEAR STRESS
!                           WALLS    :    DIMENSIONLESS FALL VELOCITY
!                             RBS    :    ROSSBY NUMBERS FOR FALL VELOCITY
!
!                                      \\ ABOVE ARE SEDIMENT SIZE RANKED PARAMETERS
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_DEF_CONSTS_N_REF_PARS

    USE MR_KINDS

    IMPLICIT NONE

    REAL   (PARD_KIND) :: R0    =    1.000E+3
    REAL   (PARD_KIND) :: V0    =    1.00E-6

    REAL   (PARD_KIND) :: SS    =    2.65
    REAL   (PARD_KIND) :: PS    =    0.4

    REAL   (PARD_KIND) :: KAR   =    0.40
    REAL   (PARD_KIND) :: COR2  =    7.292124E-5
    REAL   (PARD_KIND) :: GR    =    9.80665

    REAL   (PARD_KIND) :: COR   =    1.0
    REAL   (PARD_KIND) :: XYR   =    1.0
    REAL   (PARD_KIND) :: ZR    =    1.0
    REAL   (PARD_KIND) :: UVR   =    1.0
    REAL   (PARD_KIND) :: WR
    REAL   (PARD_KIND) :: SR
    REAL   (PARD_KIND) :: TUVR
    REAL   (PARD_KIND) :: QSUVR
    REAL   (PARD_KIND) :: KIR
    REAL   (PARD_KIND) :: DIR
    REAL   (PARD_KIND) :: VZR   =    1.00E-6
    REAL   (PARD_KIND) :: DZR   =    1.00E-6
    REAL   (PARD_KIND) :: RR    =    1.200E+3

    REAL   (PARD_KIND) :: RB
    REAL   (PARD_KIND) :: RBT
    REAL   (PARD_KIND) :: EKXY , EKZ
    REAL   (PARD_KIND) :: SCXY , SCZ
    REAL   (PARD_KIND) :: RE
    REAL   (PARD_KIND) :: RET
    REAL   (PARD_KIND) :: FR
    REAL   (PARD_KIND) :: FRD
    REAL   (PARD_KIND) :: BPAR
    REAL   (PARD_KIND) :: SURPAR
    REAL   (PARD_KIND) :: SLOPEPAR
    REAL   (PARD_KIND) :: DSPAR

    REAL   (PARD_KIND) :: DSIGMA

    REAL   (PARD_KIND) :: SIGMA(32)

    REAL   (PARD_KIND) :: D0(8) = (/ 1.0E-3 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 /)
    REAL   (PARD_KIND) :: DS(8)
    REAL   (PARD_KIND) :: TCRS(8)
    REAL   (PARD_KIND) :: WALLS(8)
    REAL   (PARD_KIND) :: RBS(8)

  END MODULE MR_DEF_CONSTS_N_REF_PARS