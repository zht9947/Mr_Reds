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
  MODULE MR_MOD_MALLOC_FIELD_VARS_AVERAGE

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_FIELD_VARS_AVERAGE

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
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  SUBROUTINE MR_MALLOC_FIELD_VARS_AVERAGE

    IMPLICIT NONE

    ALLOCATE(           AVERAGE_XSEC_H(1:NI1(NI,FDRD_KIND)    ) )
    ALLOCATE( AVERAGE_XSEC_UNIT_FORCES(1:NI1(NI,FDRD_KIND),1:2) )
    ALLOCATE( AVERAGE_XSEC_UNIT_MOTION(1:NI1(NI,FDRD_KIND),1:2) )
    ALLOCATE( AVERAGE_XSEC_UNIT_ENERGY(1:NI1(NI,FDRD_KIND)    ) )
    ALLOCATE(       AVERAGE_XSEC_TBFUV(1:NI1(NI,FDRD_KIND),1:2) )
    ALLOCATE(   AVERAGE_XSEC_TBFUV_MOD(1:NI1(NI,FDRD_KIND)    ) )
    ALLOCATE(        AVERAGE_XSEC_TBUV(1:NI1(NI,FDRD_KIND),1:2) )
    ALLOCATE(    AVERAGE_XSEC_TBUV_MOD(1:NI1(NI,FDRD_KIND)    ) )
    ALLOCATE(         AVERAGE_XSEC_QUV(1:NI1(NI,FDRD_KIND),1:2) )

  END SUBROUTINE MR_MALLOC_FIELD_VARS_AVERAGE

  END MODULE MR_MOD_MALLOC_FIELD_VARS_AVERAGE