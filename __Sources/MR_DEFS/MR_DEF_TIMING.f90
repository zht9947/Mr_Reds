!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE) MR_DEF_TIMING
!
! PURPOSE:
!
!   TO DEFINE THE TIME CONTROL PARAMETERS. IT SHOULD BE ALWAYS BORNE IN MIND THAT THE COMPUTATIONAL PROCESS IS A VITUAL ITERATIVE
!   PROCESS RATHER THAN A REAL PHYSICAL ONE UNLESS THE REAL INITIAL CONDITION IS SPECIFIED. IN THE CASE THAT THE BOUNDARY CONDITION
!   IS STEADY, THE TIME CONTROL PARAMETERS, ESPECIALLY THE TIME RELAXATION FACTOR, GENERALLY SERVE AS A WAY TO IMPROVE THE
!   CONVERGENCY OF THE ITERATION TO ACQUIRE A STEADY SOLUTION.
!
! DEFINITION OF VARIABLES:
!
!                              DT    :    COMPUTATIONAL TIME INTERVAL
!
!                         T_START    :    STARTING TIME
!
!                            NTSS    :    TOTAL NUMBER OF TIMESTEPS
!                     NTSS_OUTPUT    :    NUMBER OF TIMESTEPS BETWEEN TWO OUTPUTS
!
!                             PHI    :    TIME RELAXATION FACTOR
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   2015-03-21    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_DEF_TIMING

    USE MR_KINDS

    IMPLICIT NONE

    REAL   (TMRD_KIND) :: DT

    REAL   (TMRD_KIND) :: T_START

    INTEGER(TSID_KIND) :: NTSS
    INTEGER(TSID_KIND) :: NTSS_OUTPUT

    REAL   (PARD_KIND) :: PHI

  END MODULE MR_DEF_TIMING