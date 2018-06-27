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
  MODULE MR_MOD_UPDT_H

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_FIELD_VARS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_UPDT_H

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:      ) :: ZBU , ZBV

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:      ) :: HSTOR
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:      ) :: HSTORU , HSTORV

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
  SUBROUTINE MR_UPDT_H

    USE MR_MOD_FUNC_H

    USE MR_MOD_INTERP_XY

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    ALLOCATE(  HSTOR(1:NI1(NI,FDRD_KIND),1:NJ) ) ;  HSTOR = 0.0
      !BLOCK
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
             H( I , J ) = MR_FUNC_H(  HSTOR( I , J ) ,  ZS( I , J ) ,  ZB( I , J ) )
          END DO
        END DO
      !END BLOCK
    DEALLOCATE(  HSTOR )

    ALLOCATE( HSTORU(0:NI0(NI,FDRD_KIND),1:NJ) ) ; HSTORU = 0.0
      ALLOCATE( ZBU(0:NI0(NI,FDRD_KIND),1:NJ) )
        CALL MR_INTERP_XY_SS_U( NI , NJ , ZB , ZBU )
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 0 , NI
            HU( I , J ) = MR_FUNC_H( HSTORU( I , J ) , ZSU( I , J ) , ZBU( I , J ) )
          END DO
        END DO
      DEALLOCATE( ZBU )
    DEALLOCATE( HSTORU )

    ALLOCATE( HSTORV(1:NI1(NI,FDRD_KIND),0:NJ) ) ; HSTORV = 0.0
      ALLOCATE( ZBV(1:NI1(NI,FDRD_KIND),0:NJ) )
        CALL MR_INTERP_XY_SS_V( NI , NJ , ZB , ZBV )
        DO J = 0 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            HV( I , J ) = MR_FUNC_H( HSTORV( I , J ) , ZSV( I , J ) , ZBV( I , J ) )
          END DO
        END DO
      DEALLOCATE( ZBV )
    DEALLOCATE( HSTORV )

  END SUBROUTINE MR_UPDT_H

  END MODULE MR_MOD_UPDT_H