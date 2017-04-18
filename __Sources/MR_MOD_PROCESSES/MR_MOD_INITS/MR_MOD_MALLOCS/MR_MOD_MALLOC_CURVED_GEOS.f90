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
  MODULE MR_MOD_MALLOC_CURVED_GEOS

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_CURVED_GEOS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_MALLOC_CURVED_GEOS

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
  SUBROUTINE MR_MALLOC_CURVED_GEOS

    IMPLICIT NONE

    ALLOCATE( JUV(1:NI1(GJRD_KIND),1:NJ,1:2,1:2) )
    ALLOCATE( JUU(0:NI0(GJRD_KIND),1:NJ,1:2,1:2) , JVV(1:NI1(GJRD_KIND),0:NJ,1:2,1:2) )
    ALLOCATE( JOO(0:NI0(GJRD_KIND),0:NJ,1:2,1:2) )

    ALLOCATE( IUV(1:NI1(GJRD_KIND),1:NJ,1:2,1:2) )
    ALLOCATE( IUU(0:NI0(GJRD_KIND),1:NJ,1:2,1:2) , IVV(1:NI1(GJRD_KIND),0:NJ,1:2,1:2) )
    ALLOCATE( IOO(0:NI0(GJRD_KIND),0:NJ,1:2,1:2) )

    ALLOCATE( GUV(1:NI1(GJRD_KIND),1:NJ,1:2,1:2) )

    ALLOCATE( FUV(1:NI1(GJRD_KIND),1:NJ,1:2,1:2) )
    ALLOCATE( FUU(0:NI0(GJRD_KIND),1:NJ,1:2,1:2) , FVV(1:NI1(GJRD_KIND),0:NJ,1:2,1:2) )

    ALLOCATE(  MW(1:NI1(GJRD_KIND),1:NJ)         )
    ALLOCATE(  MU(0:NI0(GJRD_KIND),1:NJ)         ,  MV(1:NI1(GJRD_KIND),0:NJ)         )
    ALLOCATE(  MO(0:NI0(GJRD_KIND),0:NJ)         )

  END SUBROUTINE MR_MALLOC_CURVED_GEOS

  END MODULE MR_MOD_MALLOC_CURVED_GEOS