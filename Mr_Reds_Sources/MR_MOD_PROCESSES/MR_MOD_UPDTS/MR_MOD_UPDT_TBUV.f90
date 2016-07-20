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
  MODULE MR_MOD_UPDT_TBUV

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_FIELD_VARS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_UPDT_TBUV

    REAL(FDRD_KIND)    , ALLOCATABLE , DIMENSION(:,:      ) :: UVAM

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
  SUBROUTINE MR_UPDT_TBUV

    USE MR_MOD_FUNC_TBUV

    USE MR_MOD_OPERATOR_SS
    USE MR_MOD_OPERATOR_UV

    IMPLICIT NONE
    
    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    ALLOCATE( UVAM(1:NI1(FDRD_KIND),1:NJ) )

      UVAM = .MRSSQRT. ( .MRUVSQR. ( JUV .MRUVTFM. UVA ) )

      DO DIM = 1 , 2
        DO J = 1 , NJ
          DO I = 1 , NI
            TBUV( I , J ,DIM) = MR_FUNC_TBUV_COMP( 9.1699E-3 , H( I , J ) , UVA( I , J ,DIM) , UVAM( I , J ) )
          END DO
        END DO
      END DO

    DEALLOCATE( UVAM )

  END SUBROUTINE MR_UPDT_TBUV

  END MODULE MR_MOD_UPDT_TBUV