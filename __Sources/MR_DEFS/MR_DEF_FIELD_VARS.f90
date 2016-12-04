!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE) MR_DEF_FIELD_VARS
!
! PURPOSE:
!
!   TO DEFINE THE FIELD VARIABLES, WHICH HAVE A DISTRIBUTION ON THE GRID AND THUS ARE OF FORM OF ARRAYS. IN GENERAL, THESE VARIABLES
!   ARE OF SUBSTANCE OF SCALARS OR VECTORS WHICH APPEAR IN THE FORMULAE, EQUATIONS AND AUXILIARY RELATIONS AS THE MAIN KNOWN OR
!   UNKNOWN QUANTITIES. JUST AS THE CONSTANTS AND REFERENCE PARAMETERS, SOME OF THEM CAN BE ALSO CLASSIFIED BY LAYER OR BY SEDIMENT
!   GRIAN-SIZE OR BY BOTH, WHICH MEANS THAT THEIR ARRAY DIMENSIONS CAN BE AS MANY AS 4 OR 5.
!
! DEFINITION OF VARIABLES:
!
!                           TBFUV    :    FRACTIONAL BED SHEAR STRESS
!                            TBUV    :    TOTAL BED SHEAR STRESS
!                             KIB    :    TURBULENCE KINETIC ENERGY AT BED
!                             DIB    :    TURBULENCE DISSIPATION AT BED
!                              KI    :    TURBULENCE KINETIC ENERGY
!                              DI    :    TURBULENCE DISSIPATION
!                             DIS    :    TURBULENCE DISSIPATION AT FREE SURFACE
!                             CSS    :    SUSPENDED-LOAD SEDIMENT CONCENTRATION
!               QSBUV, QSBU, QSBV    :    BED-LOAD SEDIMENT FLUX
!                              ZB    :    BED ELEVATION
!                    ZS, ZSU, ZSV    :    FREE SURFACE ELEVATION
!                       H, HU, HV    :    DEPTH FROM BED TO FREE SURFACE
!                     UVA, UA, VA    :    DEPTH-AVERAGED VELOCITY
!                        UV, U, V    :    HORIZONTAL VELOCITY VECTOR, MAY ALSO BE THE DEVIATION OF THE HORIZONTAL VELOCITY VECTOR
!                               W    :    VERTICAL VELOCITY SCALAR
!                               R    :    WATER-SEDIMENT MIXTURE DENSITY
!                              RI    :    LOCAL DEPTH-AVERAGED WATER-SEDIMENT MIXTURE DENSITY
!               VXYUV, VXYU, VXYV    :    HORIZONTAL EDDY KINEMATIC VISCOSITY VECTOR
!                       VZWW, VZW    :    VERTICAL EDDY KINEMATIC VISCOSITY SCALAR
!               DXYUV, DXYU, DXYV    :    HORIZONTAL EDDY KINEMATIC DIFFUSIVITY VECTOR
!                       DZWW, DZW    :    VERTICAL EDDY KINEMATIC DIFFUSIVITY SCALAR
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-03-26    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_DEF_FIELD_VARS

    USE MR_KINDS

    IMPLICIT NONE

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:    ) :: TBFUV
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:    ) :: TBUV
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:      ) :: KIB
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:      ) :: DIB
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,  :  ) :: KI
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,  :  ) :: DI
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:      ) :: DIS
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,  :  ) :: CSS
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:    ) :: QSBUV
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:      ) :: QSBU , QSBV
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:      ) :: ZB
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:      ) :: ZS
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:      ) :: ZSU , ZSV
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:      ) :: H
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:      ) :: HU , HV
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:    ) :: UVA
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:      ) :: UA , VA
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:,:  ) :: UV
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,  :  ) :: U , V
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,  :  ) :: W
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,  :  ) :: R
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,  :  ) :: RI
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:,:  ) :: VXYUV
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,  :  ) :: VXYU , VXYV
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,  :  ) :: VZW
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:,:  ) :: DXYUV
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,  :  ) :: DXYU , DXYV
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,  :  ) :: DZW

  END MODULE MR_DEF_FIELD_VARS