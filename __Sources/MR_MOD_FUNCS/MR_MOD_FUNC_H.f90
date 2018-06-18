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
  MODULE MR_MOD_FUNC_H

    USE MR_KINDS

    USE MR_DEF_CONSTS_N_REF_PARS

    USE MR_MOC_BED_STOR

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_FUNC_H

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
  FUNCTION MR_FUNC_H( HSTOR , ZS , ZB ) RESULT( H )
   !DIR$ ATTRIBUTES VECTOR :: MR_FUNC_H
    IMPLICIT NONE

    REAL(FDRD_KIND) , INTENT(IN ) :: HSTOR , ZS , ZB

    REAL(FDRD_KIND) :: HBELOW

    REAL(FDRD_KIND) :: H

    HBELOW = HSTOR * ( 1.0 - 2.0 * CFACTSTOR * CBASESTOR / (1.0-CBASESTOR) )

    IF( ZS * ALPAR <= ZB - CFACTSTOR*HSTOR ) THEN
      H = EPSILON(H)
    ELSE IF( ZS * ALPAR <= ZB - HBELOW ) THEN
      H = CBASESTOR * ( ZS * ALPAR - ZB + CFACTSTOR*HSTOR )
    ELSE IF( ZS * ALPAR <= ZB + HSTOR  ) THEN
      H = CBASESTOR * ( ZS * ALPAR - ZB + CFACTSTOR*HSTOR )   &
      & + 0.5 * (1.0-CBASESTOR) * ( ZS * ALPAR - ZB + HBELOW )**2 / MAX( HSTOR + HBELOW , EPSILON(H) )
    ELSE 
      H = ZS * ALPAR - ZB
    END IF

  END FUNCTION MR_FUNC_H

  END MODULE MR_MOD_FUNC_H