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
  MODULE MR_MOD_READ_GRID_SYS

    USE XMDF

    USE MR_KINDS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_READ_NDID

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
  SUBROUTINE MR_READ_NDID( MESH_IN_XMDF_ID , NEM , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO , ERROR , ERRMSG )

    IMPLICIT NONE

    INTEGER            , INTENT(IN ) :: MESH_IN_XMDF_ID

    INTEGER(EMID_KIND) , INTENT(IN ) :: NEM
    INTEGER(IJID_KIND) , INTENT(IN ) :: NI , NJ
    INTEGER            , PARAMETER   :: NND_IN_EM = 8 , NND_CD_IN_EM = NND_IN_EM + 1

    INTEGER(EMID_KIND) , INTENT(IN ) , DIMENSION(1:NI1(NI,EMID_KIND),1:NJ) :: EMIDW

    INTEGER(NDID_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,NDID_KIND),1:NJ) :: NDIDW
    INTEGER(NDID_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,NDID_KIND),1:NJ) :: NDIDU
    INTEGER(NDID_KIND) , INTENT(OUT) , DIMENSION(1:NI1(NI,NDID_KIND),0:NJ) :: NDIDV
    INTEGER(NDID_KIND) , INTENT(OUT) , DIMENSION(0:NI0(NI,NDID_KIND),0:NJ) :: NDIDO

    INTEGER(NDID_KIND) , DIMENSION( 1:NND_CD_IN_EM , 1:NEM ) :: NDID_ARRAY

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER            , PARAMETER   :: CDID_IN_EM = 9
    INTEGER            :: NDID_IN_EM , NDID_IN_ANOTHER_EM

    INTEGER(IJID_KIND) :: I , J

    ERRMSG = ""
    CALL XF_READ_ELEM_NODE_IDS( MESH_IN_XMDF_ID , NEM , NND_CD_IN_EM , NDID_ARRAY , ERROR )
    IF( ERROR < 0 ) THEN
      ERRMSG = "Error in reading node ids of each element from mesh"
      RETURN
    END IF

    J = 1
      I = 1
        INIT_SHARE_SEARCH :   &
        DO NDID_IN_EM = 1 , NND_IN_EM
          DO NDID_IN_ANOTHER_EM = 1 , NND_IN_EM
            IF( NDID_ARRAY( NDID_IN_EM , EMIDW( I , J ) ) == NDID_ARRAY( NDID_IN_ANOTHER_EM , EMIDW(I+1,J+1) ) ) THEN
              EXIT INIT_SHARE_SEARCH
            END IF
          END DO
        END DO INIT_SHARE_SEARCH
       !DIR$ FORCEINLINE
        CALL MR_READ_NDID_I1_J1( I , J , NDID_IN_EM )
      !END I = 1
     !DIR$ NOVECTOR
      DO I = 2 , NI
        DO NDID_IN_EM = 1 , NND_IN_EM
          IF( NDIDO(I-1,J-1) == NDID_ARRAY( NDID_IN_EM , EMIDW( I , J ) ) ) THEN
            EXIT
          END IF
        END DO
        NDID_IN_EM = MOD( NDID_IN_EM , NND_IN_EM ) + 1
       !DIR$ FORCEINLINE
        CALL MR_READ_NDID_II_J1( I , J , NDID_IN_EM )
      END DO
    !END J = 1

   !DIR$ NOVECTOR
    DO J = 2 , NJ
      I = 1
        DO NDID_IN_EM = 1 , NND_IN_EM
          IF( NDIDO( I ,J-1) == NDID_ARRAY( NDID_IN_EM , EMIDW( I , J ) ) ) THEN
            EXIT
          END IF
        END DO
        NDID_IN_EM = MOD( NDID_IN_EM , NND_IN_EM ) + 1
       !DIR$ FORCEINLINE
        CALL MR_READ_NDID_I1_JJ( I , J , NDID_IN_EM )
      !END I = 1
     !DIR$ NOVECTOR
      DO I = 2 , NI
        DO NDID_IN_EM = 1 , NND_IN_EM
          IF( NDIDO( I ,J-1) == NDID_ARRAY( NDID_IN_EM , EMIDW( I , J ) ) ) THEN
            EXIT
          END IF
        END DO
        NDID_IN_EM = MOD( NDID_IN_EM , NND_IN_EM ) + 1
       !DIR$ FORCEINLINE
        CALL MR_READ_NDID_II_JJ( I , J , NDID_IN_EM )
      END DO
    END DO

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
  SUBROUTINE MR_READ_NDID_I1_J1( I , J , NDID_IN_EM_TO_START )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: I , J

    INTEGER            , INTENT(IN ) :: NDID_IN_EM_TO_START

    INTEGER            :: NDID_IN_EM

    NDID_IN_EM = NDID_IN_EM_TO_START

    NDIDO( I , J ) = NDID_ARRAY( NDID_IN_EM , EMIDW( I , J ) )
    NDID_IN_EM = MOD( NDID_IN_EM , NND_IN_EM ) + 1
    NDIDV( I , J ) = NDID_ARRAY( NDID_IN_EM , EMIDW( I , J ) )
    NDID_IN_EM = MOD( NDID_IN_EM , NND_IN_EM ) + 1
    NDIDO(I-1, J ) = NDID_ARRAY( NDID_IN_EM , EMIDW( I , J ) )
    NDID_IN_EM = MOD( NDID_IN_EM , NND_IN_EM ) + 1
    NDIDU(I-1, J ) = NDID_ARRAY( NDID_IN_EM , EMIDW( I , J ) )
    NDID_IN_EM = MOD( NDID_IN_EM , NND_IN_EM ) + 1
    NDIDO(I-1,J-1) = NDID_ARRAY( NDID_IN_EM , EMIDW( I , J ) )
    NDID_IN_EM = MOD( NDID_IN_EM , NND_IN_EM ) + 1
    NDIDV( I ,J-1) = NDID_ARRAY( NDID_IN_EM , EMIDW( I , J ) )
    NDID_IN_EM = MOD( NDID_IN_EM , NND_IN_EM ) + 1
    NDIDO( I ,J-1) = NDID_ARRAY( NDID_IN_EM , EMIDW( I , J ) )
    NDID_IN_EM = MOD( NDID_IN_EM , NND_IN_EM ) + 1
    NDIDU( I , J ) = NDID_ARRAY( NDID_IN_EM , EMIDW( I , J ) )

    NDIDW( I , J ) = NDID_ARRAY( CDID_IN_EM , EMIDW( I , J ) )

  END SUBROUTINE MR_READ_NDID_I1_J1

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
  SUBROUTINE MR_READ_NDID_II_J1( I , J , NDID_IN_EM_TO_START )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: I , J

    INTEGER            , INTENT(IN ) :: NDID_IN_EM_TO_START

    INTEGER            :: NDID_IN_EM

    NDID_IN_EM = NDID_IN_EM_TO_START

    NDIDV( I ,J-1) = NDID_ARRAY( NDID_IN_EM , EMIDW( I , J ) )
    NDID_IN_EM = MOD( NDID_IN_EM , NND_IN_EM ) + 1
    NDIDO( I ,J-1) = NDID_ARRAY( NDID_IN_EM , EMIDW( I , J ) )
    NDID_IN_EM = MOD( NDID_IN_EM , NND_IN_EM ) + 1
    NDIDU( I , J ) = NDID_ARRAY( NDID_IN_EM , EMIDW( I , J ) )
    NDID_IN_EM = MOD( NDID_IN_EM , NND_IN_EM ) + 1
    NDIDO( I , J ) = NDID_ARRAY( NDID_IN_EM , EMIDW( I , J ) )
    NDID_IN_EM = MOD( NDID_IN_EM , NND_IN_EM ) + 1
    NDIDV( I , J ) = NDID_ARRAY( NDID_IN_EM , EMIDW( I , J ) )

    NDIDW( I , J ) = NDID_ARRAY( CDID_IN_EM , EMIDW( I , J ) )

  END SUBROUTINE MR_READ_NDID_II_J1

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
  SUBROUTINE MR_READ_NDID_I1_JJ( I , J , NDID_IN_EM_TO_START )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: I , J

    INTEGER            , INTENT(IN ) :: NDID_IN_EM_TO_START

    INTEGER            :: NDID_IN_EM

    NDID_IN_EM = NDID_IN_EM_TO_START

    NDIDU( I , J ) = NDID_ARRAY( NDID_IN_EM , EMIDW( I , J ) )
    NDID_IN_EM = MOD( NDID_IN_EM , NND_IN_EM ) + 1
    NDIDO( I , J ) = NDID_ARRAY( NDID_IN_EM , EMIDW( I , J ) )
    NDID_IN_EM = MOD( NDID_IN_EM , NND_IN_EM ) + 1
    NDIDV( I , J ) = NDID_ARRAY( NDID_IN_EM , EMIDW( I , J ) )
    NDID_IN_EM = MOD( NDID_IN_EM , NND_IN_EM ) + 1
    NDIDO(I-1, J ) = NDID_ARRAY( NDID_IN_EM , EMIDW( I , J ) )
    NDID_IN_EM = MOD( NDID_IN_EM , NND_IN_EM ) + 1
    NDIDU(I-1, J ) = NDID_ARRAY( NDID_IN_EM , EMIDW( I , J ) )

    NDIDW( I , J ) = NDID_ARRAY( CDID_IN_EM , EMIDW( I , J ) )

  END SUBROUTINE MR_READ_NDID_I1_JJ

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
  SUBROUTINE MR_READ_NDID_II_JJ( I , J , NDID_IN_EM_TO_START )

    IMPLICIT NONE

    INTEGER(IJID_KIND) , INTENT(IN ) :: I , J

    INTEGER            , INTENT(IN ) :: NDID_IN_EM_TO_START

    INTEGER            :: NDID_IN_EM

    NDID_IN_EM = NDID_IN_EM_TO_START

    NDIDU( I , J ) = NDID_ARRAY( NDID_IN_EM , EMIDW( I , J ) )
    NDID_IN_EM = MOD( NDID_IN_EM , NND_IN_EM ) + 1
    NDIDO( I , J ) = NDID_ARRAY( NDID_IN_EM , EMIDW( I , J ) )
    NDID_IN_EM = MOD( NDID_IN_EM , NND_IN_EM ) + 1
    NDIDV( I , J ) = NDID_ARRAY( NDID_IN_EM , EMIDW( I , J ) )

    NDIDW( I , J ) = NDID_ARRAY( CDID_IN_EM , EMIDW( I , J ) )

  END SUBROUTINE MR_READ_NDID_II_JJ

  END SUBROUTINE MR_READ_NDID

  END MODULE MR_MOD_READ_GRID_SYS