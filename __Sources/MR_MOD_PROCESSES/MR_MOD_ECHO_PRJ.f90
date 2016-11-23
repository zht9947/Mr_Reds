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
!   2015-03-26    |     DR. HYDE     |    ORIGINAL CODE.
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
!   2015-03-26    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_ECHO_PRJ

    IMPLICIT NONE

    CHARACTER(LEN=1)   :: ECHO_Y_OR_N

    WRITE(*,'( )')
    WRITE(*,'(2X,"Please check the project settings below:")')

    WRITE(*,'( )')
    WRITE(*,'(4X,"   <TITLE> ", A )') TRIM(PRJ_TITLE)
    WRITE(*,'(4X,"<ABSTRACT> ", A )') TRIM(PRJ_ABSTRACT)

    WRITE(*,'( )')
    WRITE(*,'(4X,"     GRID ",I5," x ",I5," x ",I5)') NI , NJ , NK
    WRITE(*,'(4X,"       GRAIN SIZE ",F7.3," mm")') D0*1000.0

    WRITE(*,'( )')
    WRITE(*,'(4X,"----Timing----")')
    WRITE(*,'(4X,"                       Computational time interval [s]              : ", F16.6       )') DT/COR
    WRITE(*,'(4X,"                                    Time beginning [s]              : ", F15.5       )') T_START
    WRITE(*,'(4X,"          Total number of time steps for computing                  : ", I09         )') NTSS
    WRITE(*,'(4X,"    Inteval of number of time steps for outputting                  : ", I09         )') ITS_OUTPUT
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
    WRITE(*,'(4X,"                                Coriolis frequency [1/s]            : ",EN20.6       )') COR
    WRITE(*,'(4X,"                        Gravitational acceleration [m/s^2]          : ", F16.6       )') GR
    WRITE(*,'(4X,"  ----Reference Parameters----")')
    WRITE(*,'(4X,"                    Reference horizontal dimension [m]              : ", F16.6       )') XYR
    WRITE(*,'(4X,"                      Reference vertical dimension [m]              : ", F16.6       )') ZR
    WRITE(*,'(4X,"                     Reference horizontal velocity [m/s]            : ", F16.6       )') UVR
    WRITE(*,'(4X,"     Reference horizontal eddy kinematic viscosity [m^2/s]          : ",EN20.6       )') VXYR
    WRITE(*,'(4X,"       Reference vertical eddy kinematic viscosity [m^2/s]          : ",EN20.6       )') VZR
    WRITE(*,'(4X,"          Reference water-sediment mixture density [kg/m^3]         : ", F16.6,"E+03")') RR/1.0E+03

    WRITE(*,'( )')
    WRITE(*,'(4X,"----Flow Plane Slope----")')
    WRITE(*,'(4X,"    Please confirm the flow plane slope")')
    WRITE(*,'(4X,"    be coincidence with X axis!")')
    WRITE(*,'(4X,"                                  Flow plane slope                  : ",ES20.6       )') SLOPE

    WRITE(*,'( )')
    DO
      WRITE(*,'(2X,"Are all these settings above correct? (Y/N): ", $ )')
      READ(*,*) ECHO_Y_OR_N
      SELECT CASE( ECHO_Y_OR_N )
      CASE( "Y" , "y" )
        RETURN
      CASE( "N" , "n" )
        STOP
      CASE DEFAULT
        CYCLE
      END SELECT
    END DO

  END SUBROUTINE MR_ECHO_PRJ

  END MODULE MR_MOD_ECHO_PRJ