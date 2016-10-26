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
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_FIELD_VARS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_UPDT_TBUV

    REAL(FDRD_KIND)    , ALLOCATABLE , DIMENSION(:,:,  :  ) :: UV_MOD
    REAL(FDRD_KIND)    , ALLOCATABLE , DIMENSION(:,:      ) :: TBUV_MOD
    REAL(FDRD_KIND)    , ALLOCATABLE , DIMENSION(:,:      ) :: TBFUV_MOD

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

    ALLOCATE( UV_MOD(1:NI1(FDRD_KIND),1:NJ,1:1) , TBUV_MOD(1:NI1(FDRD_KIND),1:NJ) , TBFUV_MOD(1:NI1(FDRD_KIND),1:NJ) )

      UV_MOD(:,:, 1 ) = .MRSSQRT. ( .MRUVSQR. ( JUV .MRUVTFM. UV(:,:,1:2, 1 ) ) )

      TBFUV_MOD = .MRSSQRT. ( .MRUVSQR. ( JUV .MRUVTFM. TBFUV ) )
      DO DIM = 1 , 2
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            TBFUV( I , J ,DIM) = MR_FUNC_TBFUV_COMP( TBFUV_MOD( I , J ) , D0 , H( I , J ) ,   &
            & SIGMA( 1 ) , UV_MOD( I , J , 1 ) , UV( I , J ,DIM, 1 ) )
          END DO
        END DO
      END DO

      TBFUV_MOD = .MRSSQRT. ( .MRUVSQR. ( JUV .MRUVTFM. TBFUV ) )
      TBUV_MOD = .MRSSQRT. ( .MRUVSQR. ( JUV .MRUVTFM. TBUV ) )
      DO DIM = 1 , 2
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            TBUV( I , J ,DIM) = MR_FUNC_TBUV_COMP( TBUV_MOD( I , J ) , TBFUV_MOD( I , J ) , D0 , TCRS , H( I , J ) ,   &
            & SIGMA( 1 ) , UV_MOD( I , J , 1 ) , UV( I , J ,DIM, 1 ) )
          END DO
        END DO
      END DO

    DEALLOCATE( UV_MOD , TBUV_MOD , TBFUV_MOD )

  END SUBROUTINE MR_UPDT_TBUV

  END MODULE MR_MOD_UPDT_TBUV