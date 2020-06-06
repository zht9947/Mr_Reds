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
  MODULE MR_MOD_GET_SLOPE

    USE XMDF

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_GET_SLOPE

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
  SUBROUTINE MR_GET_SLOPE( MULTI_DSETS_ID , SLOPE , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MULTI_DSETS_ID

    INTEGER                          :: PROP_ID

    REAL   (SPRD_KIND) , INTENT(OUT) :: SLOPE
    REAL   (8)                       :: SLOPE_ARRAY(1:1)

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY

    ERRMSG = ""

    CALL XF_OPEN_PROPERTY_GROUP( MULTI_DSETS_ID , PROP_ID , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in opening /PROPERTIES"
    ELSE

      !BLOCK

        CALL XF_READ_PROPERTY_DOUBLE( PROP_ID , "Slope" , 1 , SLOPE_ARRAY , ERROR )
        IF( ERROR < 0 ) THEN
          ERRMSG = "Error in getting the value of slope from /PROPERTIES"
        ELSE
          SLOPE = SLOPE_ARRAY(1)
        END IF

      !END BLOCK

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

  END SUBROUTINE MR_GET_SLOPE

  END MODULE MR_MOD_GET_SLOPE