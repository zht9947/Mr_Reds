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
  MODULE MR_MOD_FUNC_DS

    USE MR_KINDS

    USE MR_DEF_CONSTS_N_REF_PARS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_FUNC_DS

    INTERFACE MR_FUNC_DS
      MODULE PROCEDURE MR_FUNC_DS_PARD_KIND
      MODULE PROCEDURE MR_FUNC_DS_FDRD_KIND
    END INTERFACE

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
  FUNCTION MR_FUNC_DS_PARD_KIND( D0 ) RESULT( DS )

    IMPLICIT NONE

    REAL(PARD_KIND) , INTENT(IN ) :: D0

    REAL(PARD_KIND) :: DS

    DS = D0 / ( DSPAR * ZR )

  END FUNCTION MR_FUNC_DS_PARD_KIND

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
  FUNCTION MR_FUNC_DS_FDRD_KIND( D0 ) RESULT( DS )

    IMPLICIT NONE

    REAL(FDRD_KIND) , INTENT(IN ) :: D0

    REAL(FDRD_KIND) :: DS

    DS = D0 / ( DSPAR * ZR )

  END FUNCTION MR_FUNC_DS_FDRD_KIND

  END MODULE MR_MOD_FUNC_DS
