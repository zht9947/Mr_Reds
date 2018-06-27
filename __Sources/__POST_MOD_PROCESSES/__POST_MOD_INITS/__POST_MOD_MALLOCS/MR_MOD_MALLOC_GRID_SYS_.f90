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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_MOD_MALLOC_GRID_SYS_

    USE MR_KINDS

    USE MR_DEF_RANKS_
    USE MR_DEF_GRID_SYS_

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_MALLOC_GRID_SYS_

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
  SUBROUTINE MR_MALLOC_GRID_SYS_

    IMPLICIT NONE

    ALLOCATE( EMIDW_(1:NI1(NI_,EMID_KIND),1:NJ_) )

    ALLOCATE( NDIDW_(1:NI1(NI_,NDID_KIND),1:NJ_) )
    ALLOCATE( NDIDU_(0:NI0(NI_,NDID_KIND),1:NJ_) , NDIDV_(1:NI1(NI_,NDID_KIND),0:NJ_) )
    ALLOCATE( NDIDO_(0:NI0(NI_,NDID_KIND),0:NJ_) )

  END SUBROUTINE MR_MALLOC_GRID_SYS_

  END MODULE MR_MOD_MALLOC_GRID_SYS_