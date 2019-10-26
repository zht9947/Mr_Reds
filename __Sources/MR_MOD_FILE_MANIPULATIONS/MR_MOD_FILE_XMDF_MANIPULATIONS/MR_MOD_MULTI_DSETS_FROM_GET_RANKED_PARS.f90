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
  MODULE MR_MOD_GET_RANKED_PARS

    USE XMDF

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_GET_D0

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
  SUBROUTINE MR_GET_D0( MULTI_DSETS_ID , NKS , D0 , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MULTI_DSETS_ID

    INTEGER                          :: PROP_ID , GROUP_ID

    INTEGER(KSID_KIND) , INTENT(IN ) :: NKS

    REAL   (PARD_KIND) , INTENT(OUT) :: D0
    REAL   (8)                       :: D0_ARRAY(1:NKS)

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY

    ERRMSG = ""

    CALL XF_OPEN_PROPERTY_GROUP( MULTI_DSETS_ID , PROP_ID , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in opening /PROPERTIES"
    ELSE

      CALL XF_OPEN_GROUP( PROP_ID , "By Sediment Sizes" , GROUP_ID , ERROR )
      IF( ERROR < 0 ) THEN
        ERRMSG = "Error in opening /PROPERTIES/By Sediment Sizes"
      ELSE

        CALL XF_READ_PROPERTY_DOUBLE( GROUP_ID , "Sediment Sizes" , NKS , D0_ARRAY , ERROR )
        IF( ERROR < 0 ) THEN
          ERRMSG = "Error in getting the value of sediment sizes from /PROPERTIES/By Sediment Sizes"
        ELSE
          D0 = D0_ARRAY(1) / 1000.0
        END IF

        CALL XF_CLOSE_GROUP( GROUP_ID , ERROR_DUMMY )
        IF( ERROR_DUMMY < 0 .AND. ERROR >= 0 ) THEN
          ERROR = ERROR_DUMMY
          ERRMSG = "Error in closing /PROPERTIES/By Sediment Sizes"
        END IF

      END IF

      CALL XF_CLOSE_GROUP( PROP_ID , ERROR_DUMMY )
      IF( ERROR_DUMMY < 0 .AND. ERROR >= 0 ) THEN
        ERROR = ERROR_DUMMY
        ERRMSG = "Error in closing /PROPERTIES"
      END IF

    END IF
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" in multiple datasets"
      RETURN
    END IF

  END SUBROUTINE MR_GET_D0

  END MODULE MR_MOD_GET_RANKED_PARS