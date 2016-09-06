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
  MODULE MR_MOD_MALLOC_FIELD_VARS

    USE MR_KINDS

    USE MR_DEF_FIELD_VARS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_MALLOC_FIELD_VARS

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
  SUBROUTINE MR_MALLOC_FIELD_VARS( NI , NJ , NK )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER(KKID_KIND) , INTENT(IN ) :: NK

    ALLOCATE(    ZB(1:NI1(FDRD_KIND),1:NJ           ) )

    ALLOCATE(    ZS(1:NI1(FDRD_KIND),1:NJ           ) )
    ALLOCATE(   ZSU(0:NI0(FDRD_KIND),1:NJ           ) ,  ZSV(1:NI1(FDRD_KIND),0:NJ           ) )

    ALLOCATE(     H(1:NI1(FDRD_KIND),1:NJ           ) )
    ALLOCATE(    HU(0:NI0(FDRD_KIND),1:NJ           ) ,   HV(1:NI1(FDRD_KIND),0:NJ           ) )

    ALLOCATE(     R(1:NI1(FDRD_KIND),1:NJ,    1:NK  ) )

    ALLOCATE(    RI(1:NI1(FDRD_KIND),1:NJ,    1:NK  ) )

    ALLOCATE(   UVA(1:NI1(FDRD_KIND),1:NJ,1:2       ) )
    ALLOCATE(    UA(0:NI0(FDRD_KIND),1:NJ           ) ,   VA(1:NI1(FDRD_KIND),0:NJ           ) )

    ALLOCATE(    UV(1:NI1(FDRD_KIND),1:NJ,1:2,1:NK  ) )
    ALLOCATE(     U(0:NI0(FDRD_KIND),1:NJ,    1:NK  ) ,    V(1:NI1(FDRD_KIND),0:NJ,    1:NK  ) )

    ALLOCATE(    WW(1:NI1(FDRD_KIND),1:NJ,    1:NK  ) )
    ALLOCATE(     W(1:NI1(FDRD_KIND),1:NJ,    0:NK  ) )

    ALLOCATE(  TBUV(1:NI1(FDRD_KIND),1:NJ,1:2       ) )

    ALLOCATE( TBFUV(1:NI1(FDRD_KIND),1:NJ,1:2       ) )
    ALLOCATE(  TBFU(0:NI0(FDRD_KIND),1:NJ           ) , TBFV(1:NI1(FDRD_KIND),0:NJ           ) )

    ALLOCATE( QSBUV(1:NI1(FDRD_KIND),1:NJ,1:2       ) )
    ALLOCATE(  QSBU(0:NI0(FDRD_KIND),1:NJ           ) , QSBV(1:NI1(FDRD_KIND),0:NJ           ) )

    ALLOCATE(   BSB(1:NI1(FDRD_KIND),1:NJ           ) )

    ALLOCATE(   SSS(1:NI1(FDRD_KIND),1:NJ,    1:NK  ) )

    ALLOCATE( VXYUV(1:NI1(FDRD_KIND),1:NJ,1:2,1:NK  ) )
    ALLOCATE(  VXYU(0:NI0(FDRD_KIND),1:NJ,    1:NK  ) , VXYV(1:NI1(FDRD_KIND),0:NJ,    1:NK  ) )

    ALLOCATE(  VZWW(1:NI1(FDRD_KIND),1:NJ,    1:NK  ) )
    ALLOCATE(   VZW(1:NI1(FDRD_KIND),1:NJ,    0:NK  ) )

    ALLOCATE( DXYUV(1:NI1(FDRD_KIND),1:NJ,1:2,1:NK  ) )
    ALLOCATE(  DXYU(0:NI0(FDRD_KIND),1:NJ,    1:NK  ) , DXYV(1:NI1(FDRD_KIND),0:NJ,    1:NK  ) )

    ALLOCATE(  DZWW(1:NI1(FDRD_KIND),1:NJ,    1:NK  ) )
    ALLOCATE(   DZW(1:NI1(FDRD_KIND),1:NJ,    0:NK  ) )

    ALLOCATE(    KI(1:NI1(FDRD_KIND),1:NJ,    1:NK  ) )

    ALLOCATE(    DI(1:NI1(FDRD_KIND),1:NJ,    1:NK  ) )

  END SUBROUTINE MR_MALLOC_FIELD_VARS

  END MODULE MR_MOD_MALLOC_FIELD_VARS