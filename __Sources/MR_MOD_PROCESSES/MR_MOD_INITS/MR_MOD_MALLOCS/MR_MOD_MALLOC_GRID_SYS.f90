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
  MODULE MR_MOD_MALLOC_GRID_SYS

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_GRID_SYS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_MALLOC_GRID_SYS

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
  SUBROUTINE MR_MALLOC_GRID_SYS

    IMPLICIT NONE

    ALLOCATE( EMIDW(1:NI1(EMID_KIND),1:NJ) )

    ALLOCATE( NDIDW(1:NI1(NDID_KIND),1:NJ) )
    ALLOCATE( NDIDU(0:NI0(NDID_KIND),1:NJ) , NDIDV(1:NI1(NDID_KIND),0:NJ) )
    ALLOCATE( NDIDO(0:NI0(NDID_KIND),0:NJ) )

  END SUBROUTINE MR_MALLOC_GRID_SYS

  END MODULE MR_MOD_MALLOC_GRID_SYS