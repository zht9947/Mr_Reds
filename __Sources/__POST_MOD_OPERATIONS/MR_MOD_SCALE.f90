#INCLUDE 'MR_H_ALIGN_PADDING.H'
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
  MODULE MR_MOD_SCALE

    USE MR_KINDS

    USE MR_DEF_FIELD_VARS_DSET_NAMES

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_SCALE_UV
    PUBLIC :: MR_SCALE_SS

    PUBLIC :: MR_SCALE_XY

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
  SUBROUTINE MR_DETER_SCALE_DSET( PATH_DSET , SCALE , SCALE_DSET )

    CHARACTER(   *   ) , INTENT(IN   ) :: PATH_DSET

    REAL   (PARD_KIND) , INTENT(IN   ) :: SCALE
    REAL   (PARD_KIND) , INTENT(OUT  ) :: SCALE_DSET

    IF( PATH_DSET == "" ) THEN
      RETURN
    ELSE IF( INDEX( TRIM(PATH_DSET) , "/"//TRIM(DSET_NAME_TBFUV) ) /= 0 ) THEN
      SCALE_DSET =  SCALE
    ELSE IF( INDEX( TRIM(PATH_DSET) , "/"//TRIM(DSET_NAME_TBUV)  ) /= 0 ) THEN
      SCALE_DSET =  SCALE
    ELSE IF( INDEX( TRIM(PATH_DSET) , "/"//TRIM(DSET_NAME_CSS)   ) /= 0 ) THEN
      SCALE_DSET =  1.0
    ELSE IF( INDEX( TRIM(PATH_DSET) , "/"//TRIM(DSET_NAME_QSBUV) ) /= 0 ) THEN
      SCALE_DSET =  SCALE**1.5
    ELSE IF( INDEX( TRIM(PATH_DSET) , "/"//TRIM(DSET_NAME_KI)    ) /= 0 ) THEN
      SCALE_DSET =  SCALE
    ELSE IF( INDEX( TRIM(PATH_DSET) , "/"//TRIM(DSET_NAME_DI)    ) /= 0 ) THEN
      SCALE_DSET =  SQRT( SCALE )
    ELSE IF( INDEX( TRIM(PATH_DSET) , "/"//TRIM(DSET_NAME_ZB)    ) /= 0 ) THEN
      SCALE_DSET =  SCALE
    ELSE IF( INDEX( TRIM(PATH_DSET) , "/"//TRIM(DSET_NAME_ZS)    ) /= 0 ) THEN
      SCALE_DSET =  SCALE
    ELSE IF( INDEX( TRIM(PATH_DSET) , "/"//TRIM(DSET_NAME_UVA)   ) /= 0 ) THEN
      SCALE_DSET =  SQRT( SCALE )
    ELSE IF( INDEX( TRIM(PATH_DSET) , "/"//TRIM(DSET_NAME_UV)    ) /= 0 ) THEN
      SCALE_DSET =  SQRT( SCALE )
    ELSE IF( INDEX( TRIM(PATH_DSET) , "/"//TRIM(DSET_NAME_WW)    ) /= 0 ) THEN
      SCALE_DSET =  SQRT( SCALE )
  ! THE DETECTION OF H MUST BE AFTER THE DETECTION OF UVA
  ! FOR THE NAME OF H IS CONTAINED IN THE NAME OF UVA
    ELSE IF( INDEX( TRIM(PATH_DSET) , "/"//TRIM(DSET_NAME_H)     ) /= 0 ) THEN
      SCALE_DSET =  SCALE
  ! SEE MR_DEF_FIELD_VARS_DSET_NAMES
    ELSE IF( INDEX( TRIM(PATH_DSET) , "/"//TRIM(DSET_NAME_R)     ) /= 0 ) THEN
      SCALE_DSET =  1.0
  ! THE DETECTION OF VZWW MUST BE AFTER THE DETECTION OF KI AND/OR DI
  ! FOR THE NAME OF VZWW IS CONTAINED IN THE PATH OF KI AND/OR DI
    ELSE IF( INDEX( TRIM(PATH_DSET) , "/"//TRIM(DSET_NAME_VZWW)  ) /= 0 ) THEN
      SCALE_DSET =  SCALE**1.5
  ! SEE MR_MOD_INIT_OUTPUT
    ELSE IF( INDEX( TRIM(PATH_DSET) , "/"//TRIM(DSET_NAME_DZWW)  ) /= 0 ) THEN
      SCALE_DSET =  SCALE**1.5
    END IF

  END SUBROUTINE MR_DETER_SCALE_DSET

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
  SUBROUTINE MR_SCALE_UV( PATH_UV , SCALE , NI , NJ , UV , UU , VV , UVO )

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN   ) :: PATH_UV

    REAL   (PARD_KIND) , INTENT(IN   ) :: SCALE

    INTEGER(IJID_KIND) , INTENT(IN   ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(INOUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ,1:2) :: UV
    REAL   (FDRD_KIND) , INTENT(INOUT) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ,1:2) :: UU
    REAL   (FDRD_KIND) , INTENT(INOUT) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ,1:2) :: VV
    REAL   (FDRD_KIND) , INTENT(INOUT) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ,1:2) :: UVO

    REAL   (PARD_KIND) :: SCALE_UV

    CALL MR_DETER_SCALE_DSET( PATH_UV , SCALE , SCALE_UV )

   !DIR$ FORCEINLINE
    CALL MR_SCALE_UV_FOR_W_NODES
   !DIR$ FORCEINLINE
    CALL MR_SCALE_UV_FOR_U_NODES
   !DIR$ FORCEINLINE
    CALL MR_SCALE_UV_FOR_V_NODES
   !DIR$ FORCEINLINE
    CALL MR_SCALE_UV_FOR_O_NODES
   !END$ FORCEINLINE

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
  SUBROUTINE MR_SCALE_UV_FOR_W_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    DO DIM = 1 , 2

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          UV( I , J ,DIM) = UV( I , J ,DIM) * SCALE_UV
        END DO
      END DO

    END DO

  END SUBROUTINE MR_SCALE_UV_FOR_W_NODES

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
  SUBROUTINE MR_SCALE_UV_FOR_U_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    DO DIM = 1 , 2

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 0 , NI
          UU( I , J ,DIM) = UU( I , J ,DIM) * SCALE_UV
        END DO
      END DO

    END DO

  END SUBROUTINE MR_SCALE_UV_FOR_U_NODES

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
  SUBROUTINE MR_SCALE_UV_FOR_V_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    DO DIM = 1 , 2

      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          VV( I , J ,DIM) = VV( I , J ,DIM) * SCALE_UV
        END DO
      END DO

    END DO

  END SUBROUTINE MR_SCALE_UV_FOR_V_NODES

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
  SUBROUTINE MR_SCALE_UV_FOR_O_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    DO DIM = 1 , 2

      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 0 , NI
          UVO( I , J ,DIM) = UVO( I , J ,DIM) * SCALE_UV
        END DO
      END DO

    END DO

  END SUBROUTINE MR_SCALE_UV_FOR_O_NODES

  END SUBROUTINE MR_SCALE_UV

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
  SUBROUTINE MR_SCALE_SS( PATH_SS , SCALE , NI , NJ , SS , SU , SV , SO )

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN   ) :: PATH_SS

    REAL   (PARD_KIND) , INTENT(IN   ) :: SCALE

    INTEGER(IJID_KIND) , INTENT(IN   ) :: NI , NJ

    REAL   (FDRD_KIND) , INTENT(INOUT) , DIMENSION(1:NI1(NI,FDRD_KIND),1:NJ) :: SS
    REAL   (FDRD_KIND) , INTENT(INOUT) , DIMENSION(0:NI0(NI,FDRD_KIND),1:NJ) :: SU
    REAL   (FDRD_KIND) , INTENT(INOUT) , DIMENSION(1:NI1(NI,FDRD_KIND),0:NJ) :: SV
    REAL   (FDRD_KIND) , INTENT(INOUT) , DIMENSION(0:NI0(NI,FDRD_KIND),0:NJ) :: SO

    REAL   (PARD_KIND) :: SCALE_SS

    CALL MR_DETER_SCALE_DSET( PATH_SS , SCALE , SCALE_SS )

   !DIR$ FORCEINLINE
    CALL MR_SCALE_SS_FOR_W_NODES
   !DIR$ FORCEINLINE
    CALL MR_SCALE_SS_FOR_U_NODES
   !DIR$ FORCEINLINE
    CALL MR_SCALE_SS_FOR_V_NODES
   !DIR$ FORCEINLINE
    CALL MR_SCALE_SS_FOR_O_NODES
   !END$ FORCEINLINE

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
  SUBROUTINE MR_SCALE_SS_FOR_W_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        SS( I , J ) = SS( I , J ) * SCALE_SS
      END DO
    END DO

  END SUBROUTINE MR_SCALE_SS_FOR_W_NODES

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
  SUBROUTINE MR_SCALE_SS_FOR_U_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    DO J = 1 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 0 , NI
        SU( I , J ) = SU( I , J ) * SCALE_SS
      END DO
    END DO

  END SUBROUTINE MR_SCALE_SS_FOR_U_NODES

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
  SUBROUTINE MR_SCALE_SS_FOR_V_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    DO J = 0 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 1 , NI
        SV( I , J ) = SV( I , J ) * SCALE_SS
      END DO
    END DO

  END SUBROUTINE MR_SCALE_SS_FOR_V_NODES

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
  SUBROUTINE MR_SCALE_SS_FOR_O_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J

    DO J = 0 , NJ
     !DIR$ VECTOR ALIGNED
      DO I = 0 , NI
        SO( I , J ) = SO( I , J ) * SCALE_SS
      END DO
    END DO

  END SUBROUTINE MR_SCALE_SS_FOR_O_NODES

  END SUBROUTINE MR_SCALE_SS

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
  SUBROUTINE MR_SCALE_XY( SCALE , NI , NJ , XYUV , XYUU , XYVV , XYOO )

    IMPLICIT NONE

    REAL   (PARD_KIND) , INTENT(IN   ) :: SCALE

    INTEGER(IJID_KIND) , INTENT(IN   ) :: NI , NJ

    REAL   (XYRD_KIND) , INTENT(INOUT) , DIMENSION(1:NI1(NI,XYRD_KIND),1:NJ,1:2) :: XYUV
    REAL   (XYRD_KIND) , INTENT(INOUT) , DIMENSION(0:NI0(NI,XYRD_KIND),1:NJ,1:2) :: XYUU
    REAL   (XYRD_KIND) , INTENT(INOUT) , DIMENSION(1:NI1(NI,XYRD_KIND),0:NJ,1:2) :: XYVV
    REAL   (XYRD_KIND) , INTENT(INOUT) , DIMENSION(0:NI0(NI,XYRD_KIND),0:NJ,1:2) :: XYOO

   !DIR$ FORCEINLINE
    CALL MR_SCALE_XY_FOR_W_NODES
   !DIR$ FORCEINLINE
    CALL MR_SCALE_XY_FOR_U_NODES
   !DIR$ FORCEINLINE
    CALL MR_SCALE_XY_FOR_V_NODES
   !DIR$ FORCEINLINE
    CALL MR_SCALE_XY_FOR_O_NODES
   !END$ FORCEINLINE

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
  SUBROUTINE MR_SCALE_XY_FOR_W_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    DO DIM = 1 , 2

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          XYUV( I , J ,DIM) = XYUV( I , J ,DIM) * SCALE
        END DO
      END DO

    END DO

  END SUBROUTINE MR_SCALE_XY_FOR_W_NODES

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
  SUBROUTINE MR_SCALE_XY_FOR_U_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    DO DIM = 1 , 2

      DO J = 1 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 0 , NI
          XYUU( I , J ,DIM) = XYUU( I , J ,DIM) * SCALE
        END DO
      END DO

    END DO

  END SUBROUTINE MR_SCALE_XY_FOR_U_NODES

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
  SUBROUTINE MR_SCALE_XY_FOR_V_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    DO DIM = 1 , 2

      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 1 , NI
          XYVV( I , J ,DIM) = XYVV( I , J ,DIM) * SCALE
        END DO
      END DO

    END DO

  END SUBROUTINE MR_SCALE_XY_FOR_V_NODES

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
  SUBROUTINE MR_SCALE_XY_FOR_O_NODES

    IMPLICIT NONE

    INTEGER(IJID_KIND) :: I , J
    INTEGER            :: DIM

    DO DIM = 1 , 2

      DO J = 0 , NJ
       !DIR$ VECTOR ALIGNED
        DO I = 0 , NI
          XYOO( I , J ,DIM) = XYOO( I , J ,DIM) * SCALE
        END DO
      END DO

    END DO

  END SUBROUTINE MR_SCALE_XY_FOR_O_NODES

  END SUBROUTINE MR_SCALE_XY

  END MODULE MR_MOD_SCALE