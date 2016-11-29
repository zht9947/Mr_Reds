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
!   2015-03-26    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_MOD_OUTPUT_AVERAGE

    USE MR_KINDS

    USE MR_DEF_RANKS
    USE MR_DEF_CONSTS_N_REF_PARS
    USE MR_DEF_CURVED_GEOS
    USE MR_DEF_FIELD_VARS
    USE MR_DEF_FIELD_VARS_DSET_NAMES
    USE MR_DEF_FIELD_VARS_UNITS

    IMPLICIT NONE

    PRIVATE

    PUBLIC :: MR_OUTPUT_AVERAGE

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:  ) :: NUM_FR
    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:  ) :: NUM_FR_STAR

    REAL   (FDRD_KIND) , ALLOCATABLE , DIMENSION(:,:    ) :: UV_MOD

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
  SUBROUTINE MR_OUTPUT_AVERAGE( FILE_AVERAGE_NAME , T , ERROR , ERRMSG )

    USE MR_MOD_CREATE_OPEN_N_CLOSE_FILE_DEFAULT

    USE MR_MOD_AVERAGE

    USE MR_MOD_OPERATOR_UV
    USE MR_MOD_OPERATOR_SS

    IMPLICIT NONE

    CHARACTER(   *   ) , INTENT(IN ) :: FILE_AVERAGE_NAME

    REAL   (TMRD_KIND) , INTENT(IN ) :: T

    INTEGER                          :: FILE_AVERAGE_ID

    INTEGER            , INTENT(OUT) :: ERROR
    CHARACTER(   *   ) , INTENT(OUT) :: ERRMSG

    REAL   (FDRD_KIND)               :: SS_AVERAGE
    REAL   (FDRD_KIND)               :: UV_AVERAGE(1:2)

    REAL   (PARD_KIND)               :: DUMMY_BASE , DUMMY_REF

    INTEGER                          :: DUMMY_CHAR_LEN

    ERRMSG = ""

    CALL MR_OPEN_FILE_DEFAULT( FILE_AVERAGE_NAME , "WRITE" , FILE_AVERAGE_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_AVERAGE_NAME)
      RETURN
    END IF

    WRITE( FILE_AVERAGE_ID , '("------------------------------------------------------------------------")'   )
    WRITE( FILE_AVERAGE_ID , '("T = ",F15.5)' ) T

  ! WRITE AVERAGED H
    DUMMY_BASE = 0.0 ; DUMMY_REF = ZR
    CALL MR_AVERAGE_SS( NI , NJ , H , SS_AVERAGE )
    SS_AVERAGE = SS_AVERAGE * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE
    DUMMY_CHAR_LEN = LEN_TRIM(DSET_NAME_H)
    WRITE( FILE_AVERAGE_ID , '(T66,TL<DUMMY_CHAR_LEN>,A," [",A,"] ",T88," : ",ES13.6)' )   &
    & TRIM(DSET_NAME_H)    , TRIM(UNIT_H)    ,   &
    & SS_AVERAGE

  ! WRITE AVERAGED UVA
    DUMMY_BASE = 0.0 ; DUMMY_REF = UVR
    CALL MR_AVERAGE_UV( NI , NJ , UVA , UV_AVERAGE )
    UV_AVERAGE = UV_AVERAGE * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE
    CALL MR_AVERAGE_SS( NI , NJ , ( .MRSSQRT. ( .MRUVSQR. ( JUV .MRUVTFM. UVA ) ) ) , SS_AVERAGE )
    SS_AVERAGE = SS_AVERAGE * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE
    DUMMY_CHAR_LEN = LEN_TRIM(DSET_NAME_UVA)
    WRITE( FILE_AVERAGE_ID , '(T66,TL<DUMMY_CHAR_LEN>,A," [",A,"] ",T88," : ",ES13.6," , ","(",ES13.6,",",ES13.6,")" )' )   &
    & TRIM(DSET_NAME_UVA)  , TRIM(UNIT_UVA)  ,   &
    & SS_AVERAGE , UV_AVERAGE

  ! WRITE AVERAGED TBUV
    DUMMY_BASE = 0.0 ; DUMMY_REF = TUVR
    CALL MR_AVERAGE_UV( NI , NJ , TBUV , UV_AVERAGE )
    UV_AVERAGE = UV_AVERAGE * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE
    CALL MR_AVERAGE_SS( NI , NJ , ( .MRSSQRT. ( .MRUVSQR. ( JUV .MRUVTFM. TBUV ) ) ) , SS_AVERAGE )
    SS_AVERAGE = SS_AVERAGE * ( DUMMY_REF - DUMMY_BASE ) + DUMMY_BASE
    DUMMY_CHAR_LEN = LEN_TRIM(DSET_NAME_TBUV)
    WRITE( FILE_AVERAGE_ID , '(T66,TL<DUMMY_CHAR_LEN>,A," [",A,"] ",T88," : ",ES13.6," , ","(",ES13.6,",",ES13.6,")" )' )   &
    & TRIM(DSET_NAME_TBUV) , TRIM(UNIT_TBUV) ,   &
    & SS_AVERAGE , UV_AVERAGE

  ! WRITE NUM_FR_STAR
    ALLOCATE( NUM_FR_STAR(1:NI1(FDRD_KIND),1:NJ,1:2) )

    NUM_FR_STAR = FR2 / RBT * ( TBUV .MRUVDIV. H )

    CALL MR_AVERAGE_UV( NI , NJ , NUM_FR_STAR , UV_AVERAGE )
    CALL MR_AVERAGE_SS( NI , NJ , ( .MRSSQRT. ( .MRUVSQR. ( JUV .MRUVTFM. NUM_FR_STAR ) ) ) , SS_AVERAGE )
    WRITE( FILE_AVERAGE_ID , '(4X,"Fr* = ",ES13.6," , ","(",ES13.6,",",ES13.6,")" )' )   &
    & SS_AVERAGE , UV_AVERAGE
    
    DEALLOCATE( NUM_FR_STAR )

  ! WRITE NUM_FR
    ALLOCATE( NUM_FR(1:NI1(FDRD_KIND),1:NJ,1:2) , UV_MOD(1:NI1(FDRD_KIND),1:NJ) )

    UV_MOD =  .MRSSQRT. ( .MRUVSQR. ( JUV .MRUVTFM. UVA ) )
    NUM_FR = FR2 * ( ( UVA .MRUVMTP. UV_MOD ) .MRUVDIV. H )

    DEALLOCATE( UV_MOD )

    CALL MR_AVERAGE_UV( NI , NJ , NUM_FR , UV_AVERAGE )
    CALL MR_AVERAGE_SS( NI , NJ , ( .MRSSQRT. ( .MRUVSQR. ( JUV .MRUVTFM. NUM_FR ) ) ) , SS_AVERAGE )
    WRITE( FILE_AVERAGE_ID , '(4X,"Fr  = ",ES13.6," , ","(",ES13.6,",",ES13.6,")" )' )   &
    & SS_AVERAGE , UV_AVERAGE

    DEALLOCATE( NUM_FR )

    CALL MR_CLOSE_FILE_DEFAULT( FILE_AVERAGE_ID , ERROR , ERRMSG )
    IF( ERROR < 0 ) THEN
      ERRMSG = TRIM(ERRMSG)//" "//TRIM(FILE_AVERAGE_NAME)
      RETURN
    END IF

  END SUBROUTINE MR_OUTPUT_AVERAGE

  END MODULE MR_MOD_OUTPUT_AVERAGE