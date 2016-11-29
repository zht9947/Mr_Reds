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
  MODULE MR_MOD_DETER_START_MODE

    USE MR_KINDS

    USE MR_DEF_PRJ_METADATA

    USE MR_DEF_RANKS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_SLOPE
    USE MR_DEF_TIMING

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_DETER_START_MODE

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
  SUBROUTINE MR_DETER_START_MODE( FLAG_COLD_START )

    IMPLICIT NONE

    LOGICAL            , INTENT(OUT)  :: FLAG_COLD_START

    CHARACTER( LEN=1 ) :: COLD_START_Y_OR_N

    WRITE(*,'( )')
    DO
      WRITE(*,'("Cold start? (Y/N): ", $ )')
      READ(*,*) COLD_START_Y_OR_N
      SELECT CASE( COLD_START_Y_OR_N )
      CASE( "Y" , "y" )
        FLAG_COLD_START = .TRUE.
        RETURN
      CASE( "N" , "n" )
        FLAG_COLD_START = .FALSE.
        RETURN
      CASE DEFAULT
        CYCLE
      END SELECT
    END DO

  END SUBROUTINE MR_DETER_START_MODE

  END MODULE MR_MOD_DETER_START_MODE