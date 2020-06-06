!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE) MR_DEF_RANKS
!
! PURPOSE:
!
!   TO DEFINE THE RANK PARAMETERS DETERMINING THE SIZE OF ARRAYS.
!
! DEFINITION OF VARIABLES:
!
!                             NND    :    NUMBER OF NODES
!                             NEM    :    NUMBER OF ELEMENTS
!                          NI, NJ    :    NUMBER OF COLUMNS AND ROWS, RESPECTIVELY
!                              NK    :    NUMBER OF LAYERS
!                             NKS    :    NUMBER OF SEDIMENT SIZES
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_DEF_RANKS

    USE MR_KINDS

    IMPLICIT NONE

    INTEGER(NDID_KIND) :: NND
    INTEGER(EMID_KIND) :: NEM
    INTEGER(IJID_KIND) :: NI , NJ
    INTEGER(KKID_KIND) :: NK = 10
    INTEGER(KSID_KIND) :: NKS = 0

  END MODULE MR_DEF_RANKS