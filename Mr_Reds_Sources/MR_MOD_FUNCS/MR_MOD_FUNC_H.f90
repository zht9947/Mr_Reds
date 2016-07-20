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
  FUNCTION MR_FUNC_H( ZS , ZB ) RESULT( H )
  
    IMPLICIT NONE
    
    REAL(FDRD_KIND) , INTENT(IN ) :: ZS , ZB
    
    REAL(FDRD_KIND) :: H
    
    H = MAX( ( ZS * ALPAR - ZB ) , EPSILON(H) )
    
  END FUNCTION MR_FUNC_H
  
  END MODULE MR_MOD_FUNC_H