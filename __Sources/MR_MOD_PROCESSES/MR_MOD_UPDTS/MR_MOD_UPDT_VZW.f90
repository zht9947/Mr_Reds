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
  MODULE MR_MOD_UPDT_VZW

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_FIELD_VARS

    USE MR_MCS_K_EPS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_UPDT_VZW

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:    ) :: VZWW
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:      ) :: VZB , VZS

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
  SUBROUTINE MR_UPDT_VZW

    USE MR_MOD_INTERP_Z

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER(KKID_KIND) :: K

    ALLOCATE( VZWW(1:NI1(FDRD_KIND),1:NJ,1:NK) )
      DO K = 1 , NK
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            VZWW( I , J , K ) = V0 / VZR + CV0 / EKZ * KI( I , J , K ) * KI( I , J , K ) / MAX( DI( I , J , K ) , EPSILON(DI) )
          END DO
        END DO
      END DO

      ALLOCATE( VZB(1:NI1(FDRD_KIND),1:NJ) , VZS(1:NI1(FDRD_KIND),1:NJ) )
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            VZB( I , J ) = V0 / VZR + CV0 / EKZ * KIB( I , J ) * KIB( I , J ) / MAX( DIB( I , J ) , EPSILON(DIB) )
            VZS( I , J ) = V0 / VZR + CV0 / EKZ * KI( I , J ,NK ) * KI( I , J ,NK ) / MAX( DIS( I , J ) , EPSILON(DIS) )
          END DO
        END DO

        CALL MR_INTERP_Z_SS_W( NI , NJ , NK , VZWW , VZW , VZB , VZS )

      DEALLOCATE( VZB , VZS )

    DEALLOCATE( VZWW )

  END SUBROUTINE MR_UPDT_VZW

  END MODULE MR_MOD_UPDT_VZW