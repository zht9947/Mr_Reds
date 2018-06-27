!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE)
!
! PURPOSE:
!
!
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
  MODULE MR_MOD_GET_RANKS

    USE XMDF

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_GET_RANKS

!***********************************************************************************************************************************

  CONTAINS

!***********************************************************************************************************************************
! UNIT:
!
!  (SUBROUTINE)
!
! PURPOSE:
!
!
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
  SUBROUTINE MR_GET_RANKS( MESH_IN_XMDF_ID , NND , NEM , NI , NJ , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MESH_IN_XMDF_ID

    INTEGER(NDID_KIND) , INTENT(OUT) :: NND
    INTEGER(EMID_KIND) , INTENT(OUT) :: NEM
    INTEGER(EMID_KIND) , INTENT(OUT) :: NI , NJ

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    REAL   (CARD_KIND)               :: CTMP1 , CTMP2

    ERRMSG = ""

  ! GET FROM MESH NND AND NEM
    CALL XF_GET_NUMBER_OF_NODES( MESH_IN_XMDF_ID , NND , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in getting the number of nodes from mesh"
      RETURN
    END IF
    CALL XF_GET_NUMBER_OF_ELEMENTS( MESH_IN_XMDF_ID , NEM , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in getting the number of elements from mesh"
      RETURN
    END IF

  ! CALCULATE NI AND NJ
    CTMP1 = 0.25*(NND-1) - NEM
    CTMP2 = SQRT( CTMP1*CTMP1 - NEM )
  ! ASSUME NI IS ALWAYS GREATER THAN OR EQUAL TO NJ
    NI = NINT( CTMP1 + CTMP2 )
    NJ = NINT( CTMP1 - CTMP2 )

  ! CHECK CONSISTENCY
    IF( NND /= ((2*NI+1)*(2*NJ+1)) .AND. NEM /= (NI*NJ) ) THEN
      ERROR = - 5500
      ERRMSG = "Inconsistent both number of nodes (NND) and number of elements (NEM) gotten from mesh"
      RETURN
    ELSE IF( NND /= ((2*NI+1)*(2*NJ+1)) ) THEN
      ERROR = - 5501
      ERRMSG = "Inconsistent number of nodes (NND) gotten from mesh"
      RETURN
    ELSE IF( NEM /= (NI*NJ) ) THEN
      ERROR = - 5502
      ERRMSG = "Inconsistent number of elements (NEM) gotten from mesh"
      RETURN
    END IF

  END SUBROUTINE MR_GET_RANKS

  END MODULE MR_MOD_GET_RANKS