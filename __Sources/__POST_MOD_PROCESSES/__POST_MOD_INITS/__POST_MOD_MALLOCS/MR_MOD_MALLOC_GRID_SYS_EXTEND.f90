#INCLUDE 'MR_H_ALIGN_PADDING.H'
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
  MODULE MR_MOD_MALLOC_GRID_SYS_EXTEND

    USE MR_KINDS

    USE MR_DEF_RANKS , ONLY : NI , NJ
    USE MR_DEF_GRID_SYS_EXTEND

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_MALLOC_GRID_SYS_EXTEND

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
  SUBROUTINE MR_MALLOC_GRID_SYS_EXTEND( NLOOPS )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: NLOOPS

    ALLOCATE( EMIDW2(1:NI1(NI*NLOOPS,EMID_KIND),1:NJ) )

    ALLOCATE( NDIDW2(1:NI1(NI*NLOOPS,NDID_KIND),1:NJ) )
    ALLOCATE( NDIDU2(0:NI0(NI*NLOOPS,NDID_KIND),1:NJ) , NDIDV2(1:NI1(NI*NLOOPS,NDID_KIND),0:NJ) )
    ALLOCATE( NDIDO2(0:NI0(NI*NLOOPS,NDID_KIND),0:NJ) )

  END SUBROUTINE MR_MALLOC_GRID_SYS_EXTEND

  END MODULE MR_MOD_MALLOC_GRID_SYS_EXTEND