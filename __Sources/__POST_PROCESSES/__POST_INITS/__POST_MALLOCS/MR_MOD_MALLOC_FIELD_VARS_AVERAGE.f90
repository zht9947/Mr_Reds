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
  MODULE MR_MOD_MALLOC_FIELD_VARS_AVERAGE

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_FIELD_VARS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_MALLOC_FIELD_VARS_AVERAGE

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
  SUBROUTINE MR_MALLOC_FIELD_VARS_AVERAGE

    IMPLICIT NONE

    ALLOCATE(      H(1:NI1(FDRD_KIND),1:NJ           ) )

    ALLOCATE(    UVA(1:NI1(FDRD_KIND),1:NJ,1:2       ) )

    ALLOCATE(   TBUV(1:NI1(FDRD_KIND),1:NJ,1:2       ) )

    ALLOCATE(  NUMFR(1:NI1(FDRD_KIND),1:NJ,1:2       ) )
    ALLOCATE( NUMFRS(1:NI1(FDRD_KIND),1:NJ,1:2       ) )

  END SUBROUTINE MR_MALLOC_FIELD_VARS_AVERAGE

  END MODULE MR_MOD_MALLOC_FIELD_VARS_AVERAGE