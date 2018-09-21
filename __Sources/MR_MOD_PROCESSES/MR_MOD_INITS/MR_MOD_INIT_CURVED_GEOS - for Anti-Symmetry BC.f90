#INCLUDE 'MR_H_ALIGN_PADDING.H'
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
  MODULE MR_MOD_INIT_CURVED_GEOS

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_COORS
    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_GRID_SYS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_INIT_CURVED_GEOS

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
  SUBROUTINE MR_INIT_CURVED_GEOS( FILE_XMDF_NAME , ERROR , ERRMSG )

    USE MR_MOD_OPERATOR_XUV

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_XMDF_NAME

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    REAL   (XYRD_KIND) ,               DIMENSION(          1:2) , PARAMETER :: FACTOR = (/+1.0,-1.0/)

    INTEGER(IJID_KIND) :: I , J
    INTEGER(IJID_KIND) :: P , Q

    INTEGER            :: DIM

    ALLOCATE( XYUV(1:NI1(NI,XYRD_KIND),1:NJ,1:2) )
    ALLOCATE( XYUU(0:NI0(NI,XYRD_KIND),1:NJ,1:2) , XYVV(1:NI1(NI,XYRD_KIND),0:NJ,1:2) )
    ALLOCATE( XYOO(0:NI0(NI,XYRD_KIND),0:NJ,1:2) )

    CALL MR_INIT_COORS( FILE_XMDF_NAME , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" when initializing curved geometry"
    ELSE

      DO DIM = 1 , 2
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            JUV( I , J ,DIM,1) = XYUU( I , J ,DIM) - XYUU(I-1, J ,DIM)
          END DO
        END DO
      END DO
      DO DIM = 1 , 2
        DO J = 1 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            JUV( I , J ,DIM,2) = XYVV( I , J ,DIM) - XYVV( I ,J-1,DIM)
          END DO
        END DO
      END DO
      JUV = JUV / XYR
      IF( ALLOCATED(GUV) ) GUV = ( .MRXUVTPS. JUV ) .MRXUVMAT. JUV
      IF( ALLOCATED(MW) ) THEN
        MW = .MRXUVDTM. JUV
        IF( ALLOCATED(IUV) ) THEN
          IUV = MW .MRXUVIVS. JUV
          IF( ALLOCATED(FUV) ) THEN
            FUV = IUV .MRXUVMAT. ( .MRXUVTPS. IUV )
          END IF
        END IF
      END IF

      DO DIM = 1 , 2
        DO J = 1 , NJ
          I = 0
            P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1
            JUU( I , J ,DIM,1) =   &
            ( ( XYUV(I+1, J ,DIM) - XYUU( I , J ,DIM) )   &
            + ( XYUU( P , Q ,DIM) - XYUV( P , Q ,DIM) ) * FACTOR(DIM)   &
            )
          !END I = 0
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI-1
            JUU( I , J ,DIM,1) = XYUV(I+1, J ,DIM) - XYUV( I , J ,DIM)
          END DO
          I = NI
            P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1
            JUU( I , J ,DIM,1) =   &
            ( ( XYUV(P+1, Q ,DIM) - XYUU( P , Q ,DIM) ) * FACTOR(DIM)   &
            + ( XYUU( I , J ,DIM) - XYUV( I , J ,DIM) )   &
            )
          !END I = NI
        END DO
      END DO
      DO DIM = 1 , 2
        DO J = 1 , NJ
          I = 0
            P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1
            JUU( I , J ,DIM,2) =   &
            ( ( XYOO( I , J ,DIM) - XYOO( I ,J-1,DIM) )   &
            + ( XYOO( P ,Q-1,DIM) - XYOO( P , Q ,DIM) ) * FACTOR(DIM)   &
            ) / 2.0
          !END I = 0
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI-1
            JUU( I , J ,DIM,2) = XYOO( I , J ,DIM) - XYOO( I ,J-1,DIM)
          END DO
          I = NI
            P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1
            JUU( I , J ,DIM,2) =   &
            ( ( XYOO( P ,Q-1,DIM) - XYOO( P , Q ,DIM) ) * FACTOR(DIM)   &
            + ( XYOO( I , J ,DIM) - XYOO( I ,J-1,DIM) )   &
            ) / 2.0
          !END I = NI
        END DO
      END DO
      JUU = JUU / XYR
      IF( ALLOCATED(GUU) ) GUU = ( .MRXUVTPS. JUU ) .MRXUVMAT. JUU
      IF( ALLOCATED(MU) ) THEN
        MU = .MRXUVDTM. JUU
        IF( ALLOCATED(IUU) ) THEN
          IUU = MU .MRXUVIVS. JUU
          IF( ALLOCATED(FUU) ) THEN
            FUU = IUU .MRXUVMAT. ( .MRXUVTPS. IUU )
          END IF
        END IF
      END IF

      DO DIM = 1 , 2
        DO J = 0 , NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            JVV( I , J ,DIM,1) = XYOO( I , J ,DIM) - XYOO(I-1, J ,DIM)
          END DO
        END DO
      END DO
      DO DIM = 1 , 2
        J = 0
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            JVV( I , J ,DIM,2) = 2.0 * ( XYUV( I ,J+1,DIM) - XYVV( I , J ,DIM) )
          END DO
        !END J = 0
        DO J = 1 , NJ-1
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            JVV( I , J ,DIM,2) = XYUV( I ,J+1,DIM) - XYUV( I , J ,DIM)
          END DO
        END DO
        J = NJ
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI
            JVV( I , J ,DIM,2) = 2.0 * ( XYVV( I , J ,DIM) - XYUV( I , J ,DIM) )
          END DO
        !END J = NJ
      END DO
      JVV = JVV / XYR
      IF( ALLOCATED(GVV) ) GVV = ( .MRXUVTPS. JVV ) .MRXUVMAT. JVV
      IF( ALLOCATED(MV) ) THEN
        MV = .MRXUVDTM. JVV
        IF( ALLOCATED(IVV) ) THEN
          IVV = MV .MRXUVIVS. JVV
          IF( ALLOCATED(FVV) ) THEN
            FVV = IVV .MRXUVMAT. ( .MRXUVTPS. IVV )
          END IF
        END IF
      END IF

      DO DIM = 1 , 2
        DO J = 0 , NJ
          I = 0
            P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1
            JOO( I , J ,DIM,1) =   &
            ( ( XYVV(I+1, J ,DIM) - XYOO( I , J ,DIM) )   &
            + ( XYOO( P ,Q-1,DIM) - XYVV( P ,Q-1,DIM) ) * FACTOR(DIM)   &
            )
          !END I = 0
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI-1
            JOO( I , J ,DIM,1) = XYVV(I+1, J ,DIM) - XYVV( I , J ,DIM)
          END DO
          I = NI
            P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1
            JOO( I , J ,DIM,1) =   &
            ( ( XYVV(P+1,Q-1,DIM) - XYOO( P ,Q-1,DIM) ) * FACTOR(DIM)   &
            + ( XYOO( I , J ,DIM) - XYVV( I , J ,DIM) )   &
            )
          !END I = NI
        END DO
      END DO
      DO DIM = 1 , 2
        J = 0
          I = 0
            P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1
            JOO( I , J ,DIM,2) =   &
            ( ( XYUU( I ,J+1,DIM) - XYOO( I , J ,DIM) )   &
            + ( XYUU( P ,Q-1,DIM) - XYOO( P ,Q-1,DIM) ) * FACTOR(DIM)   &
            )
          !END I = 0
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI-1
            JOO( I , J ,DIM,2) = 2.0 * ( XYUU( I ,J+1,DIM) - XYOO( I , J ,DIM) )
          END DO
          I = NI
            P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1
            JOO( I , J ,DIM,2) =   &
            ( ( XYUU( I ,J+1,DIM) - XYOO( I , J ,DIM) )   &
            + ( XYUU( P ,Q-1,DIM) - XYOO( P ,Q-1,DIM) ) * FACTOR(DIM)   &
            )
          !END I = NI
        !END J = 0
        DO J = 1 , NJ-1
          I = 0
            P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1
            JOO( I , J ,DIM,2) =   &
            ( ( XYUU( I ,J+1,DIM) - XYUU( I , J ,DIM) )   &
            + ( XYUU( P ,Q-1,DIM) - XYUU( P , Q ,DIM) ) * FACTOR(DIM)   &
            ) / 2.0
          !END I = 0
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI-1
            JOO( I , J ,DIM,2) = XYUU( I ,J+1,DIM) - XYUU( I , J ,DIM)
          END DO
          I = NI
            P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1
            JOO( I , J ,DIM,2) =   &
            ( ( XYUU( P ,Q-1,DIM) - XYUU( P , Q ,DIM) ) * FACTOR(DIM)   &
            + ( XYUU( I ,J+1,DIM) - XYUU( I , J ,DIM) )   &
            ) / 2.0
          !END I = NI
        END DO
        J = NJ
          I = 0
            P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1
            JOO( I , J ,DIM,2) =   &
            ( ( XYOO( P ,Q-1,DIM) - XYUU( P , Q ,DIM) ) * FACTOR(DIM)   &
            + ( XYOO( I , J ,DIM) - XYUU( I , J ,DIM) )   &
            )
          !END I = 0
         !DIR$ VECTOR ALIGNED
          DO I = 1 , NI-1
            JOO( I , J ,DIM,2) = 2.0 * ( XYOO( I , J ,DIM) - XYUU( I , J ,DIM) )
          END DO
          I = NI
            P = MOD(NI+I+NI/2,2*NI)-NI/2 ; Q = NJ-J+1
            JOO( I , J ,DIM,2) =   &
            ( ( XYOO( P ,Q-1,DIM) - XYUU( P , Q ,DIM) ) * FACTOR(DIM)   &
            + ( XYOO( I , J ,DIM) - XYUU( I , J ,DIM) )   &
            )
          !END I = NI
        !END J = NJ
      END DO
      JOO = JOO / XYR
      IF( ALLOCATED(GOO) ) GOO = ( .MRXUVTPS. JOO ) .MRXUVMAT. JOO
      IF( ALLOCATED(MO) ) THEN
        MO = .MRXUVDTM. JOO
        IF( ALLOCATED(IOO) ) THEN
          IOO = MO .MRXUVIVS. JOO
          IF( ALLOCATED(FOO) ) THEN
            FOO = IOO .MRXUVMAT. ( .MRXUVTPS. IOO )
          END IF
        END IF
      END IF

    END IF

    DEALLOCATE( XYUV , XYUU , XYVV , XYOO )

  END SUBROUTINE MR_INIT_CURVED_GEOS

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
  SUBROUTINE MR_INIT_COORS( FILE_XMDF_NAME , ERROR , ERRMSG )

    USE MR_MOD_OPEN_N_CLOSE_FILE_XMDF
    USE MR_MOD_OPEN_N_CLOSE_MESH_IN_XMDF

    USE MR_MOD_READ_COORS

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_XMDF_NAME

    INTEGER                          :: FILE_XMDF_ID , MESH_IN_XMDF_ID

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    INTEGER                          :: ERROR_DUMMY
    CHARACTER( 2**10 )               :: ERRMSG_DUMMY

    CALL MR_OPEN_FILE_XMDF( FILE_XMDF_NAME , FILE_XMDF_ID , ERROR , ERRMSG , READONLY=.TRUE. )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF_NAME)
      RETURN
    END IF

    CALL MR_OPEN_MESH_IN_XMDF( FILE_XMDF_ID , MESH_IN_XMDF_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MESH_IN_XMDF)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_READ_XY( MESH_IN_XMDF_ID , NND , NI , NJ , EMIDW , NDIDW , NDIDU , NDIDV , NDIDO ,   &
    & XYUV , XYUU , XYVV , XYOO , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MESH_IN_XMDF)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_MESH_IN_XMDF( MESH_IN_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_CLOSE_MESH_IN_XMDF( MESH_IN_XMDF_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" /"//TRIM(XF_PATH_MESH_IN_XMDF)//" in file "//TRIM(FILE_XMDF_NAME)
      CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR_DUMMY , ERRMSG_DUMMY )
      RETURN
    END IF

    CALL MR_CLOSE_FILE_XMDF( FILE_XMDF_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_XMDF_NAME)
      RETURN
    END IF

  END SUBROUTINE MR_INIT_COORS

  END MODULE MR_MOD_INIT_CURVED_GEOS