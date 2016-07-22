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
  MODULE MR_MOD_FUNC_TBUV
    
    USE MR_KINDS
    
    USE MR_DEF_CONSTS_N_REF_PARS
    
    IMPLICIT NONE
    
    PRIVATE
    
    PUBLIC :: MR_FUNC_TBUV_COMP
    
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
  FUNCTION MR_FUNC_TBUV_COMP( KS , H , UVA_COMP , UVA_MAG ) RESULT( TBUV_COMP )
   !DIR$ ATTRIBUTES VECTOR : UNIFORM(KS) :: MR_FUNC_TBUV_COMP
    IMPLICIT NONE
    
    REAL(FDRD_KIND) , INTENT(IN ) :: KS , H , UVA_COMP , UVA_MAG
    
    REAL(FDRD_KIND) :: TBUV_COMP
    
   !DIR$ INLINE
    TBUV_COMP = RBT * UVA_COMP * UVA_MAG / MR_FUNC_CHEZY( KS , H )**2
    
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
  FUNCTION MR_FUNC_CHEZY( KS , H ) RESULT( CHEZY )
   !DIR$ ATTRIBUTES VECTOR : UNIFORM(KS) :: MR_FUNC_CHEZY
    IMPLICIT NONE
    
    REAL(FDRD_KIND) , INTENT(IN ) :: KS , H
    
    REAL(FDRD_KIND) :: CHEZY
    
    CHEZY = LOG( 0.368 * (H*ZR) / KS ) / KAR + 8.5
    
  END FUNCTION MR_FUNC_CHEZY
    
  END FUNCTION MR_FUNC_TBUV_COMP
  
  END MODULE MR_MOD_FUNC_TBUV
  
  
! Manning's Formula:
! TBUV_COMP = RBT * GR * ( MANNING * MANNING / (H*ZR)**(1.0/3.0) ) * UVA_COMP * UVA_MAG
  
  
! Logarithmic Formula:
! TBUV_COMP = RBT * UVA_COMP * UVA_MAG / MR_FUNC_CHEZY( KS , H )**2
! CHEZY = LOG( 0.368 * (H*ZR) / KS ) / KAR + 8.5
  
! Laminar :
! TBUV_COMP = 3.0 * V0 * UVA_COMP / H
  