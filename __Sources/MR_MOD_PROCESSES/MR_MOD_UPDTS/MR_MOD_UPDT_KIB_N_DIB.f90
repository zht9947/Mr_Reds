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
  MODULE MR_MOD_UPDT_KIB_N_DIB

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_FIELD_VARS

    USE MR_MCS_K_EPS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_UPDT_KIB_N_DIB

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:      ) :: TBUV_SQR , TBUV_MOD

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
  SUBROUTINE MR_UPDT_KIB_N_DIB

    USE MR_MOD_OPERATOR_UV
    USE MR_MOD_OPERATOR_SS

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE( TBUV_SQR(1:NI1(FDRD_KIND),1:NJ) , TBUV_MOD(1:NI1(FDRD_KIND),1:NJ) )

      TBUV_SQR = .MRUVSQR. ( JUV .MRUVTFM. TBUV )
      TBUV_MOD = .MRSSQRT. ( TBUV_SQR )

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          KIB( I , J ) = TBUV_MOD( I , J ) / ( SQRT( CV0 ) )
          DIB( I , J ) = TBUV_SQR( I , J ) / ( KAR * EKZ * (70.0*V0/VZR) )
        END DO
      END DO

    DEALLOCATE( TBUV_SQR , TBUV_MOD )

  END SUBROUTINE MR_UPDT_KIB_N_DIB

  END MODULE MR_MOD_UPDT_KIB_N_DIB