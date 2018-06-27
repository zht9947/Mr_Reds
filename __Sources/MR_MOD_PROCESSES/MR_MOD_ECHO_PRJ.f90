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
  MODULE MR_MOD_ECHO_PRJ

    USE MR_KINDS

    USE MR_DEF_PRJ_METADATA
    USE MR_DEF_RANKS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_SLOPE
    USE MR_DEF_TIMING

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_ECHO_PRJ

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
  SUBROUTINE MR_ECHO_PRJ

    IMPLICIT NONE

    CHARACTER( 1 ) :: PRJ_SETS_CORRECT_Y_OR_N

    WRITE(*,'( )')
    WRITE(*,'(2X,"Please check the project settings below:")')

    WRITE(*,'( )')
    WRITE(*,'(4X,"   <TITLE> ", A )') TRIM(PRJ_TITLE)
    WRITE(*,'(4X,"<ABSTRACT> ", A )') TRIM(PRJ_ABSTRACT)

    WRITE(*,'( )')
    WRITE(*,'(4X,"    NUMBER OF LAYERS ",I09)') NK
    WRITE(*,'(4X,"       GRAIN SIZE ",F12.5," mm")') D0*1000.0

    WRITE(*,'( )')
    WRITE(*,'(4X,"----Timing----")')
    WRITE(*,'(4X,"                       Computational time interval [s]              : ", F16.6       )') DT/COR
    WRITE(*,'(4X,"                                    Time beginning [s]              : ", F15.5       )') T_START
    WRITE(*,'(4X,"           Total number of timesteps for computing                  : ", I09         )') NTSS
    WRITE(*,'(4X,"     Inteval of number of timesteps for outputting                  : ", I09         )') NTSS_OUTPUT
    WRITE(*,'(4X,"                            Time relaxation factor                  : ", F13.3       )') PHI

    WRITE(*,'( )')
    WRITE(*,'(4X,"----Constants and Reference Parameters----")')
    WRITE(*,'(4X,"  ----Water Physical Properties----")')
    WRITE(*,'(4X,"                                     Water density [kg/m^3]         : ", F16.6,"E+03")') R0/1.0E+03
    WRITE(*,'(4X,"                         Water kinematic viscosity [m^2/s]          : ", F16.6,"E-06")') V0/1.0E-06
    WRITE(*,'(4X,"  ----Sediment Physical Properties----")')
    WRITE(*,'(4X,"                         Sediment specific gravity                  : ", F16.6       )') SS
    WRITE(*,'(4X,"                                 Sediment porosity                  : ", F16.6       )') PS
    WRITE(*,'(4X,"  ----Physical Constants----")')
    WRITE(*,'(4X,"                               Von Karman constant                  : ", F16.6       )') KAR
    WRITE(*,'(4X,"                                Coriolis frequency [1/s]            : ",ES20.6       )') COR
    WRITE(*,'(4X,"                        Gravitational acceleration [m/s^2]          : ", F16.6       )') GR
    WRITE(*,'(4X,"  ----Reference Parameters----")')
    WRITE(*,'(4X,"                    Reference horizontal dimension [m]              : ", F16.6       )') XYR
    WRITE(*,'(4X,"                      Reference vertical dimension [m]              : ", F16.6       )') ZR
    WRITE(*,'(4X,"                     Reference horizontal velocity [m/s]            : ", F16.6       )') UVR
    WRITE(*,'(4X,"                Reference eddy kinematic viscosity [m^2/s]          : ",ES20.6       )') VZR
    WRITE(*,'(4X,"          Reference water-sediment mixture density [kg/m^3]         : ", F16.6,"E+03")') RR/1.0E+03

    WRITE(*,'( )')
    WRITE(*,'(4X,"----Slope----")')
    WRITE(*,'(4X,"    Please confirm that the slope")')
    WRITE(*,'(4X,"    be along the centerline!")')
    WRITE(*,'(4X,"                                             Slope                  : ",ES20.6       )') SLOPE

  END SUBROUTINE MR_ECHO_PRJ

  END MODULE MR_MOD_ECHO_PRJ