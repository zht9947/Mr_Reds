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
  MODULE MR_MOD_MALLOC_CURVED_GEOS_PLUS

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_CURVED_GEOS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_MALLOC_CURVED_GEOS_GUU , MR_MALLOC_CURVED_GEOS_GVV
    PUBLIC :: MR_MALLOC_CURVED_GEOS_GOO

    PUBLIC :: MR_MALLOC_CURVED_GEOS_FOO

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
  SUBROUTINE MR_MALLOC_CURVED_GEOS_GUU

    IMPLICIT NONE

    ALLOCATE( GUU(0:NI0(NI,GJRD_KIND),1:NJ,1:2,1:2) )

  END SUBROUTINE MR_MALLOC_CURVED_GEOS_GUU

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
  SUBROUTINE MR_MALLOC_CURVED_GEOS_GVV

    IMPLICIT NONE

    ALLOCATE( GVV(1:NI1(NI,GJRD_KIND),0:NJ,1:2,1:2) )

  END SUBROUTINE MR_MALLOC_CURVED_GEOS_GVV

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
  SUBROUTINE MR_MALLOC_CURVED_GEOS_GOO

    IMPLICIT NONE

    ALLOCATE( GOO(0:NI0(NI,GJRD_KIND),0:NJ,1:2,1:2) )

  END SUBROUTINE MR_MALLOC_CURVED_GEOS_GOO

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
  SUBROUTINE MR_MALLOC_CURVED_GEOS_FOO

    IMPLICIT NONE

    ALLOCATE( FOO(0:NI0(NI,GJRD_KIND),0:NJ,1:2,1:2) )

  END SUBROUTINE MR_MALLOC_CURVED_GEOS_FOO

  END MODULE MR_MOD_MALLOC_CURVED_GEOS_PLUS