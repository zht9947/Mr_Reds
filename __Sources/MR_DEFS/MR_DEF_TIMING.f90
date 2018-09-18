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

    REAL   (TMRD_KIND) :: DT = 1.00000E-6

    REAL   (PARD_KIND) :: PHI = 1.000

    INTEGER(TSID_KIND) :: NTSS = 100
    INTEGER(TSID_KIND) :: NTSS_OUTPUT = 1

  END MODULE MR_DEF_TIMING