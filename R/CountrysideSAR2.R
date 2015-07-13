CountrysideSAR2 <- function(a1, a2, a3, par){
  species <- (a1*par[1] + par[2]*a2 + par[3]*a3)^par[4]
  names(species) <- NULL
  species
}