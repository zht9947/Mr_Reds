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

    H = MR_FUNC_H( ZS , ZB )

    ALLOCATE( ZBU(0:NI,1:NJ) )
      CALL MR_INTERP_XY_SS_U( NI , NJ , ZB , ZBU )

      HU = MR_FUNC_H( ZSU , ZBU )

    DEALLOCATE( ZBU )

    ALLOCATE( ZBV(1:NI,0:NJ) )
      CALL MR_INTERP_XY_SS_V( NI , NJ , ZB , ZBV )

      HV = MR_FUNC_H( ZSV , ZBV )

    DEALLOCATE( ZBV )

  END SUBROUTINE MR_UPDT_H

  END MODULE MR_MOD_UPDT_H