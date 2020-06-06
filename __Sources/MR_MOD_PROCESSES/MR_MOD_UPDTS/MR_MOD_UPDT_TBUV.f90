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
  MODULE MR_MOD_UPDT_TBUV

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_FIELD_VARS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_UPDT_TBUV

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,  :  ) :: UV_MOD
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:      ) :: TBUV_MOD , TBFUV_MOD
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:      ) :: HKS , HDKS

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
  SUBROUTINE MR_UPDT_TBUV

    USE MR_MOD_FUNC_HKS
    USE MR_MOD_FUNC_TBUV

    USE MR_MOD_OPERATOR_SS
    USE MR_MOD_OPERATOR_UV

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    ALLOCATE( UV_MOD(1:NI1(NI,FDRD_KIND),1:NJ,1:1) )

    UV_MOD(:,:, 1 ) = .MRSSQRT. ( .MRUVSQR. ( JUV .MRUVTFM. UV(:,:,1:2, 1 ) ) )

      IF( NKS == 0 ) THEN

        ALLOCATE( TBUV_MOD(1:NI1(NI,FDRD_KIND),1:NJ) , HKS(1:NI1(NI,FDRD_KIND),1:NJ) )

          TBUV_MOD = .MRSSQRT. ( .MRUVSQR. ( JUV .MRUVTFM. TBUV ) )
          !BLOCK
            DO J = 1 , NJ
             !DIR$ VECTOR ALIGNED
              DO I = 1 , NI
                HKS( I , J ) = D0(1) / ZR
              END DO
            END DO
          !END BLOCK
          DO DIM = 1 , 2
            DO J = 1 , NJ
             !DIR$ VECTOR ALIGNED
              DO I = 1 , NI
                TBUV( I , J ,DIM) = MR_FUNC_TBUV_COMP( SIGMA( 1 ) ,   &
                & H( I , J ) , HKS( I , J ) ,   &
                & HKS( I , J ) , TBUV_MOD( I , J ) ,   &
                & UV_MOD( I , J , 1 ) , UV( I , J ,DIM, 1 ) )
              END DO
            END DO
          END DO

          DO DIM = 1 , 2
            DO J = 1 , NJ
             !DIR$ VECTOR ALIGNED
              DO I = 1 , NI
                TBFUV( I , J ,DIM) = TBUV( I , J ,DIM)
              END DO
            END DO
          END DO

        DEALLOCATE( TBUV_MOD , HKS )

      ELSE

        ALLOCATE( TBUV_MOD(1:NI1(NI,FDRD_KIND),1:NJ) , HKS(1:NI1(NI,FDRD_KIND),1:NJ) ,   &
        & TBFUV_MOD(1:NI1(NI,FDRD_KIND),1:NJ) , HDKS(1:NI1(NI,FDRD_KIND),1:NJ) )

          TBFUV_MOD = .MRSSQRT. ( .MRUVSQR. ( JUV .MRUVTFM. TBFUV ) )
          TBUV_MOD = .MRSSQRT. ( .MRUVSQR. ( JUV .MRUVTFM. TBUV ) )
          !BLOCK
            DO J = 1 , NJ
             !DIR$ VECTOR ALIGNED
              DO I = 1 , NI
                HDKS( I , J ) = MR_FUNC_HDKS( DS(1) )
              END DO
            END DO
            DO J = 1 , NJ
             !DIR$ VECTOR ALIGNED
              DO I = 1 , NI
                HKS( I , J ) = MR_FUNC_HKS( DS(1) , TCRS(1) , H( I , J ) , TBFUV_MOD( I , J ) )
              END DO
            END DO
          !END BLOCK
          DO DIM = 1 , 2
            DO J = 1 , NJ
             !DIR$ VECTOR ALIGNED
              DO I = 1 , NI
                TBFUV( I , J ,DIM) = MR_FUNC_TBUV_COMP( SIGMA( 1 ) ,   &
                & H( I , J ) , HDKS( I , J ) ,   &
                & HKS( I , J ) , TBUV_MOD( I , J ) ,   &
                & UV_MOD( I , J , 1 ) , UV( I , J ,DIM, 1 ) )
              END DO
            END DO
          END DO

          TBFUV_MOD = .MRSSQRT. ( .MRUVSQR. ( JUV .MRUVTFM. TBFUV ) )
          !BLOCK
            DO J = 1 , NJ
             !DIR$ VECTOR ALIGNED
              DO I = 1 , NI
                HKS( I , J ) = MR_FUNC_HKS( DS(1) , TCRS(1) , H( I , J ) , TBFUV_MOD( I , J ) )
              END DO
            END DO
          !END BLOCK
          DO DIM = 1 , 2
            DO J = 1 , NJ
             !DIR$ VECTOR ALIGNED
              DO I = 1 , NI
                TBUV( I , J ,DIM) = MR_FUNC_TBUV_COMP( SIGMA( 1 ) ,   &
                & H( I , J ) , HKS( I , J ) ,   &
                & HKS( I , J ) , TBUV_MOD( I , J ) ,   &
                & UV_MOD( I , J , 1 ) , UV( I , J ,DIM, 1 ) )
              END DO
            END DO
          END DO

        DEALLOCATE( TBUV_MOD , TBFUV_MOD , HKS , HDKS )

      END IF

    DEALLOCATE( UV_MOD )

  END SUBROUTINE MR_UPDT_TBUV

  END MODULE MR_MOD_UPDT_TBUV