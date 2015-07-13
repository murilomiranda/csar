CountrysideSAR <- function(a1, a2, a3, h1, h2, h3, z){
  species <- (h1*a1 + h2*a2 + h3*a3)^z
  #dh1<-a1*(a1*h1 + a2*h2 + a3*h3 + a4*h4)^(-1 + z)*z
  #dh2<-a2*(a1*h1 + a2*h2 + a3*h3 + a4*h4)^(-1 + z)*z
  #dh3<-a3*(a1*h1 + a2*h2 + a3*h3 + a4*h4)^(-1 + z)*z
  #dh4<-a4*(a1*h1 + a2*h2 + a3*h3 + a4*h4)^(-1 + z)*z   
  #dz<-(a1*h1 + a2*h2 + a3*h3 + a4*h4)^z*log(a1*h1 + a2*h2 + a3*h3 + a4*h4)
  #attr(species,"gradient")<-cbind(dh1,dh2,dh3,dh4,dz)
  species
}