#ifndef EXTERN_H
#define EXTERN_H
%{

extern "C" void dsytrd_(char *uplo, int *n, double *a, int *lda, double *d, double *e, double *tau, double *work, int *lwork, int *info);
extern "C" void ssytrd_(char *uplo, int *n, float *a, int *lda, float *d, float *e, float *tau, float *work, int *lwork, int *info);
void sytrd_wrap(char *uplo, int *n, double *a, int *lda, double *d, double *e, double *tau, double *work, int *lwork, int *info) {
  dsytrd_(uplo, n, a, lda, d, e, tau, work, lwork, info);
}
void sytrd_wrap(char *uplo, int *n, float *a, int *lda, float *d, float *e, float *tau, float *work, int *lwork, int *info) {
  ssytrd_(uplo, n, a, lda, d, e, tau, work, lwork, info);
}

extern "C" void dstebz_(char *range, char *order, int *n, double *vl, double *vu, int *il, int *iu, double *abstol, double *d, double *e, int *m, int *nsplit, double *w, int *iblock, int *isplit, double *work, int *iwork, int *info);
extern "C" void sstebz_(char *range, char *order, int *n, float *vl, float *vu, int *il, int *iu, float *abstol, float *d, float *e, int *m, int *nsplit, float *w, int *iblock, int *isplit, float *work, int *iwork, int *info);
void stebz_wrap(char *range, char *order, int *n, double *vl, double *vu, int *il, int *iu, double *abstol, double *d, double *e, int *m, int *nsplit, double *w, int *iblock, int *isplit, double *work, int *iwork, int *info) {
  dstebz_(range, order, n, vl, vu, il, iu, abstol, d, e, m, nsplit, w, iblock, isplit, work, iwork, info);
}
void stebz_wrap(char *range, char *order, int *n, float *vl, float *vu, int *il, int *iu, float *abstol, float *d, float *e, int *m, int *nsplit, float *w, int *iblock, int *isplit, float *work, int *iwork, int *info) {
  sstebz_(range, order, n, vl, vu, il, iu, abstol, d, e, m, nsplit, w, iblock, isplit, work, iwork, info);
}

extern "C" void dstein_(int *n, double *d, double *e, int *m, double *w, int *iblock, int *isplit, double *z, int *ldz, double *work, int *iwork, int *ifail, int *info);
extern "C" void sstein_(int *n, float *d, float *e, int *m, float *w, int *iblock, int *isplit, float *z, int *ldz, float *work, int *iwork, int *ifail, int *info);
void stein_wrap(int *n, double *d, double *e, int *m, double *w, int *iblock, int *isplit, double *z, int *ldz, double *work, int *iwork, int *ifail, int *info) {
  dstein_(n, d, e, m, w, iblock, isplit, z, ldz, work, iwork, ifail, info);
}
void stein_wrap(int *n, float *d, float *e, int *m, float *w, int *iblock, int *isplit, float *z, int *ldz, float *work, int *iwork, int *ifail, int *info) {
  sstein_(n, d, e, m, w, iblock, isplit, z, ldz, work, iwork, ifail, info);
}

extern "C" void dormtr_(char *side, char *uplo, char *trans, int *m, int *n, double *a, int *lda, double *tau, double *c, int *ldc, double *work, int *lwork, int *info);
extern "C" void sormtr_(char *side, char *uplo, char *trans, int *m, int *n, float *a, int *lda, float *tau, float *c, int *ldc, float *work, int *lwork, int *info);
void ormtr_wrap(char *side, char *uplo, char *trans, int *m, int *n, double *a, int *lda, double *tau, double *c, int *ldc, double *work, int *lwork, int *info) {
  dormtr_(side, uplo, trans, m, n, a, lda, tau, c, ldc, work, lwork, info);
}
void ormtr_wrap(char *side, char *uplo, char *trans, int *m, int *n, float *a, int *lda, float *tau, float *c, int *ldc, float *work, int *lwork, int *info) {
  sormtr_(side, uplo, trans, m, n, a, lda, tau, c, ldc, work, lwork, info);
}

extern "C" void dsteqr_(char *compz, int *n, double *D, double *E, double *Z, int *ldz, double *work, int *info);
extern "C" void ssteqr_(char *compz, int *n, float *D, float *E, float *Z, int *ldz, float *work, int *info);
void steqr_wrap(char *compz, int *n, double *D, double *E, double *Z, int *ldz, double *work, int *info) {
  dsteqr_(compz, n, D, E, Z, ldz, work, info);
}
void steqr_wrap(char *compz, int *n, float *D, float *E, float *Z, int *ldz, float *work, int *info) {
  ssteqr_(compz, n, D, E, Z, ldz, work, info);
}

extern "C" void dsyevd_(char *jobz, char *uplo, int *n, double *A, int *lda, double *W, double *work, int *lwork, int *iwork, int *liwork, int *info);
extern "C" void ssyevd_(char *jobz, char *uplo, int *n, float *A, int *lda, float *W, float *work, int *lwork, int *iwork, int *liwork, int *info);
void syevd_wrap(char *jobz, char *uplo, int *n, double *A, int *lda, double *W, double *work, int *lwork, int *iwork, int *liwork, int *info) {
  dsyevd_(jobz, uplo, n, A, lda, W, work, lwork, iwork, liwork, info);
}
void syevd_wrap(char *jobz, char *uplo, int *n, float *A, int *lda, float *W, float *work, int *lwork, int *iwork, int *liwork, int *info) {
  ssyevd_(jobz, uplo, n, A, lda, W, work, lwork, iwork, liwork, info);
}

extern "C" void dlaed1_(int *n, double *D, double *Q, int *ldq, int *indxq, double *rho, int *cutpnt, double *work, int *iwork, int *info);
extern "C" void slaed1_(int *n, float *D, float *Q, int *ldq, int *indxq, float *rho, int *cutpnt, float *work, int *iwork, int *info);
void laed1_wrap(int *n, double *D, double *Q, int *ldq, int *indxq, double *rho, int *cutpnt, double *work, int *iwork, int *info) {
  dlaed1_(n, D, Q, ldq, indxq, rho, cutpnt, work, iwork, info);
}
void laed1_wrap(int *n, float *D, float *Q, int *ldq, int *indxq, float *rho, int *cutpnt, float *work, int *iwork, int *info) {
  slaed1_(n, D, Q, ldq, indxq, rho, cutpnt, work, iwork, info);
}

extern "C" void dstevd_(char *jobz, int *n, double *D, double *E, double *Z, int *ldz, double *work, int *lwork, int *iwork, int *liwork, int *info);
extern "C" void sstevd_(char *jobz, int *n, float *D, float *E, float *Z, int *ldz, float *work, int *lwork, int *iwork, int *liwork, int *info);
void stevd_wrap(char *jobz, int *n, double *D, double *E, double *Z, int *ldz, double *work, int *lwork, int *iwork, int *liwork, int *info) {
  dstevd_(jobz, n, D, E, Z, ldz, work, lwork, iwork, liwork, info);
}
void stevd_wrap(char *jobz, int *n, float *D, float *E, float *Z, int *ldz, float *work, int *lwork, int *iwork, int *liwork, int *info) {
  sstevd_(jobz, n, D, E, Z, ldz, work, lwork, iwork, liwork, info);
}

%}
#endif
