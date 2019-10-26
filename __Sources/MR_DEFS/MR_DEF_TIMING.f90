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
!                              DT    :    TIME INTERVAL
!
!                             PHI    :    TIME RELAXATION FACTOR
!
!                            NTSS    :    TOTAL NUMBER OF TIMESTEPS COMPUTED
!                     NTSS_OUTPUT    :    NUMBER OF TIMESTEPS BETWEEN TWO OUTPUTS
!
!                         T_START    :    STARTING TIME (ONLY FOR COLD MODE)
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_DEF_TIMING

    USE MR_KINDS

    IMPLICIT NONE

    REAL   (TMRD_KIND) :: DT = 0.000001

    REAL   (PARD_KIND) :: PHI = 1.000

    INTEGER(TSID_KIND) :: NTSS = 10000
    INTEGER(TSID_KIND) :: NTSS_OUTPUT = 100

    REAL   (TMRD_KIND) :: T_START = 0.000000

  END MODULE MR_DEF_TIMING