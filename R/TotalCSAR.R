TotalCSAR <- function(a1, a2, a3){
  sp.AG<-CountrysideSAR2(a1, a2, a3, coef(csar.AG))
  sp.UB<-CountrysideSAR2(a1, a2, a3, coef(csar.UB))
  sp.LS<-CountrysideSAR2(a1, a2, a3, coef(csar.LS))
  sp.QF<-CountrysideSAR2(a1, a2, a3, coef(csar.QF))
  sp.AG + sp.UB + sp.LS + sp.QF
}