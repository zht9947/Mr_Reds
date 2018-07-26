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
  MODULE MR_MOD_MALLOC_ERROR_ARRAY

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_ERROR_ARRAY

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_MALLOC_ERROR_1D_ARRAY

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
  SUBROUTINE MR_MALLOC_ERROR_1D_ARRAY

    IMPLICIT NONE

    ALLOCATE( ERROR_1D_ARRAY(1:NI1(NI,GJRD_KIND)) )

  END SUBROUTINE MR_MALLOC_ERROR_1D_ARRAY

  END MODULE MR_MOD_MALLOC_ERROR_ARRAY