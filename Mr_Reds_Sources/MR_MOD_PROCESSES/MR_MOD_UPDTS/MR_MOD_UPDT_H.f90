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
  MODULE MR_MOD_UPDT_H

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_FIELD_VARS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_UPDT_H

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:      ) :: ZBU , ZBV

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
  SUBROUTINE MR_UPDT_H

    USE MR_MOD_FUNC_H

    USE MR_MOD_INTERP_XY

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        H( I , J ) = MR_FUNC_H( ZS( I , J ) , ZB( I , J ) )
      END DO
    END DO

    ALLOCATE( ZBU(0:NI0(FDRD_KIND),1:NJ) )
      CALL MR_INTERP_XY_SS_U( NI , NJ , ZB , ZBU )

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 0 , NI
          HU( I , J ) = MR_FUNC_H( ZSU( I , J ) , ZBU( I , J ) )
        END DO
      END DO

    DEALLOCATE( ZBU )

    ALLOCATE( ZBV(1:NI1(FDRD_KIND),0:NJ) )
      CALL MR_INTERP_XY_SS_V( NI , NJ , ZB , ZBV )

      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          HV( I , J ) = MR_FUNC_H( ZSV( I , J ) , ZBV( I , J ) )
        END DO
      END DO

    DEALLOCATE( ZBV )

  END SUBROUTINE MR_UPDT_H

  END MODULE MR_MOD_UPDT_H