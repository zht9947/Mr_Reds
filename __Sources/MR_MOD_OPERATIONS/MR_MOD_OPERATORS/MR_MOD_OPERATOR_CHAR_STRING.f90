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
  MODULE MR_MOD_OPERATOR_CHAR_STRING

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: OPERATOR( .MRCHARUPPER. )

    INTERFACE OPERATOR( .MRCHARUPPER. )
      MODULE PROCEDURE MR_CHAR_STRING_UPPER
    END INTERFACE

!***********************************************************************************************************************************

  CONTAINS

!***********************************************************************************************************************************
! UNIT:
!
!  (FUNCTION)
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
  FUNCTION MR_CHAR_STRING_UPPER( STRING ) RESULT( STRING_UPPER )

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: STRING

    CHARACTER( LEN(STRING) )         :: STRING_UPPER

    INTEGER :: IDX_ASCII_CHAR
    INTEGER :: I_CHAR

    DO I_CHAR = 1 , LEN(STRING)

      IDX_ASCII_CHAR = IACHAR( STRING(I_CHAR:I_CHAR) )

      IF( IDX_ASCII_CHAR >= 97 .AND. IDX_ASCII_CHAR <= 122 ) THEN
        STRING_UPPER(I_CHAR:I_CHAR) = ACHAR( IDX_ASCII_CHAR - 32 )
      ELSE
        STRING_UPPER(I_CHAR:I_CHAR) = STRING(I_CHAR:I_CHAR)
      END IF

    END DO

  END FUNCTION MR_CHAR_STRING_UPPER

  END MODULE MR_MOD_OPERATOR_CHAR_STRING