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
  SUBROUTINE MR_MALLOC_CURVED_GEOS( NI , NJ )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ

    ALLOCATE( JUV(1:NI,1:NJ,1:2,1:2) , JUU(0:NI,1:NJ,1:2,1:2) , JVV(1:NI,0:NJ,1:2,1:2) , JOO(0:NI,0:NJ,1:2,1:2) )
    ALLOCATE( IUV(1:NI,1:NJ,1:2,1:2) , IUU(0:NI,1:NJ,1:2,1:2) , IVV(1:NI,0:NJ,1:2,1:2) , IOO(0:NI,0:NJ,1:2,1:2) )
    ALLOCATE( FUV(1:NI,1:NJ,1:2,1:2) , FUU(0:NI,1:NJ,1:2,1:2) , FVV(1:NI,0:NJ,1:2,1:2) )

    ALLOCATE( MW(1:NI,1:NJ) , MU(0:NI,1:NJ) , MV(1:NI,0:NJ) )

  END SUBROUTINE MR_MALLOC_CURVED_GEOS

  END MODULE MR_MOD_MALLOC_CURVED_GEOS