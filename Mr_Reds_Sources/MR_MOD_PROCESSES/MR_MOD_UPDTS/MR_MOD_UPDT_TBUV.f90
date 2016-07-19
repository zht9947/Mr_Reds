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
    USE MR_MOD_OPERATOR_UV

    IMPLICIT NONE

    INTEGER :: DIM

    ALLOCATE( UVAM(1:NI,1:NJ) )

      UVAM = SQRT( .MRUVSQR. ( JUV .MRUVTFM. UVA ) )

      DO DIM = 1 , 2
        TBUV(:,:,DIM) = MR_FUNC_TBUV_COMP( 9.1699E-3 , H , UVA(:,:,DIM) , UVAM )
      END DO

    DEALLOCATE( UVAM )

  END SUBROUTINE MR_UPDT_TBUV

  END MODULE MR_MOD_UPDT_TBUV