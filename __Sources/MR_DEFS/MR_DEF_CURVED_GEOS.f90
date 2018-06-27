!***********************************************************************************************************************************
! UNIT:
!
!  (MODULE) MR_DEF_CURVED_GEOS
!
! PURPOSE:
!
!   TO DEFINE THE CURVED GEOMETRIC MATRICES, WHICH EXPRESS THE CURVATURE
!   OF THE GRID, INCLUDING JACOBIAN AND GEOMETRIC TENSORS AND THEIR INVERSES.
!
! DEFINITION OF VARIABLES:
!
!                                             ©°©¤      ©¤©´
!              JUV, JUU, JVV, JOO    :        ©¦ dx  dx ©¦
!                                             ©¦ ©¤©¤  ©¤©¤ ©¦
!                   IUV, IUU, IVV    :        ©¦ d¦Î  d¦Ç ©¦
!                                         J = ©¦        ©¦ , I = INVERSE(J) , G = TRANSPOSE(J) * J , F = INVERSE(G) = I * TRANSPOSE(I)
!                   GUV, GUU, GVV    :        ©¦ dy  dy ©¦
!                                             ©¦ ©¤©¤  ©¤©¤ ©¦
!                   FUV, FUU, FVV    :        ©¦ d¦Î  d¦Ç ©¦
!                                             ©¸©¤      ©¤©¼
!
!                      MW, MU, MV    :    DETERMINANT OF JACOBIAN MATRIX
!
! RECORD OF REVISIONS:
!
!      DATE       |    PROGRAMMER    |    DESCRIPTION OF CHANGE
!      ====       |    ==========    |    =====================
!   20XX-XX-XX    |     DR. HYDE     |    ORIGINAL CODE.
!
!***********************************************************************************************************************************
  MODULE MR_DEF_CURVED_GEOS

    USE MR_KINDS

    IMPLICIT NONE

    REAL   (GJRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:,:) :: JUV , JUU , JVV , JOO
    REAL   (GJRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:,:) :: IUV , IUU , IVV , IOO
    REAL   (GJRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:,:) :: GUV , GUU , GVV , GOO
    REAL   (GJRD_KIND) , ALLOCATABLE , DIMENSION(:,:,:,:) :: FUV , FUU , FVV , FOO

    REAL   (GJRD_KIND) , ALLOCATABLE , DIMENSION(:,:) :: MW , MU , MV , MO

  END MODULE MR_DEF_CURVED_GEOS