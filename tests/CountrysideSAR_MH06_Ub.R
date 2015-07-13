# escolher a directoria onde estao os dados

# tem que estar instaladas no sistema as bibliotecas gdata e gtools
library(gdata)

# carrega a matriz com dados
# nao pode haver caracteres com acentos no ficheiro Excel
matMHUb <- read.table("MatrixMHUb.txt", head = TRUE)
head(matMHUb)

# define SAR clássica

source('SAR.R')
source('SS.R')


# define SAR multihabitat
source('CountrysideSAR.R')
source('CountrysideSAR2.R')

#CLASSIC SAR

# ajusta os dados à SAR tradicional mas só para as espécies agricolas em zonas agricolas, SAR single-habitat, 
#alterei o subset, de modo a não usar os dados de quadrados de 64x64 com 2 habitats
sar.AGAG <- nls(Spcs_AG ~ SAR(Area, c, z), data = matMHUb, start = list(c = 1, z = .25), lower = list(c = 0, z = 0), algorithm = "port", subset = (Hbt == "A"))
summary(sar.AGAG)
sumofsquares(sar.AGAG)
AIC(sar.AGAG)

# ajusta os dados à SAR tradicional mas só para as espécies agricolas em zonas matos, SAR single-habitat
sar.AGSH <- nls(Spcs_AG ~ SAR(Area, c, z), data = matMHUb, start = list(c = 1, z = .25), lower = list(c = 0, z = 0), algorithm = "port", subset = (Hbt == "M"))
summary(sar.AGSH)
sumofsquares(sar.AGSH)
AIC(sar.AGSH)

#outro modo de definir o modelo, sem ter que definir a SAR
mod.nonlin_test <- nls(Spcs_AG ~ c * Area^z, data = matMHUb, start = list(c = 1,z = 0.2), subset = (Hbt == "M"))
summary(mod.nonlin_test)

# ajusta os dados à SAR tradicional mas só para as espécies agricolas em zonas floresta, SAR single-habitat
sar.AGQF <- nls(Spcs_AG ~ SAR(Area, c, z), data = matMHUb, start = list(c = 1, z = .25), lower = list(c = 0, z = 0), algorithm = "port", subset = (Hbt == "Q"))
summary(sar.AGQF)
sumofsquares(sar.AGQF)
AIC(sar.AGQF)

# ajusta os dados à SAR tradicional mas só para as espécies matos em zonas agricolas, SAR single-habitat
sar.SHAG <- nls(Spcs_SH ~ SAR(Area, c, z), data = matMHUb, start = list(c = 1, z = .25), lower = list(c = 0, z = 0), algorithm = "port", subset = (Hbt == "A"))
summary(sar.SHAG)
sumofsquares(sar.SHAG)
AIC(sar.SHAG)

# ajusta os dados à SAR tradicional mas só para as espécies matos em zonas matos, SAR single-habitat
sar.SHSH <- nls(Spcs_SH ~ SAR(Area, c, z), data = matMHUb, start = list(c = 1, z = .25), lower = list(c = 0, z = 0), algorithm = "port", subset = (Hbt == "M"))
summary(sar.SHSH)
sumofsquares(sar.SHSH)
AIC(sar.SHSH)

# ajusta os dados à SAR tradicional mas só para as espécies matos em zonas floresta, SAR single-habitat
sar.SHQF <- nls(Spcs_SH ~ SAR(Area, c, z), data = matMHUb, start = list(c = 1, z = .25), lower = list(c = 0, z = 0), algorithm = "port", subset = (Hbt == "Q"))
summary(sar.SHQF)
sumofsquares(sar.SHQF)
AIC(sar.SHQF)

# ajusta os dados à SAR tradicional mas só para as espécies floresta em zonas agricolas, SAR single-habitat
sar.QFAG <- nls(Spcs_QF ~ SAR(Area, c, z), data = matMHUb, start = list(c = 1, z = .25), lower = list(c = 0, z = 0), algorithm = "port", subset = (Hbt == "A"))
summary(sar.QFAG)
sumofsquares(sar.QFAG)
AIC(sar.QFAG)

# ajusta os dados à SAR tradicional mas só para as espécies floresta em zonas matos, SAR single-habitat
sar.QFSH <- nls(Spcs_QF ~ SAR(Area, c, z), data = matMHUb, start = list(c = 1, z = .25), lower = list(c = 0, z = 0), algorithm = "port", subset = (Hbt == "M"))
summary(sar.QFSH)
sumofsquares(sar.QFSH)
AIC(sar.QFSH)

# ajusta os dados à SAR tradicional mas só para as espécies floresta em zonas floresta, SAR single-habitat
sar.QFQF <- nls(Spcs_QF ~ SAR(Area, c, z), data = matMHUb, start = list(c = 1, z = .25), lower = list(c = 0, z = 0), algorithm = "port", subset = (Hbt == "Q"))
summary(sar.QFQF)
sumofsquares(sar.QFQF)
AIC(sar.QFQF)


#grafico single-habitat
plot(Spcs_QF ~ Area_QF, data = matMHUb, log = "xy", subset = (Hbt == "Q"))
abline(log10(coef(sar.QFQF)["c"]), coef(sar.QFQF)["z"], col = "red")


# ajusta os dados à SAR tradicional mas só para as espécies agricolas, SAR multihabitat
sar.AG <- nls(Spcs_AG ~ SAR(Area, c, z), data = matMHUb, start = list(c = 1, z = .25), lower = list(c = 0, z = 0), algorithm = "port")
summary(sar.AG)
sumofsquares(sar.AG)
AIC(sar.AG)

# ajusta os dados à SAR tradicional mas só para as espécies matos, SAR multihabitat
sar.SH <- nls(Spcs_SH ~ SAR(Area, c, z), data = matMHUb, start = list(c = 1, z = .25), lower = list(c = 0, z = 0), algorithm = "port")
summary(sar.SH)
sumofsquares(sar.SH)
AIC(sar.SH)

# ajusta os dados à SAR tradicional mas só para as espécies floresta, SAR multihabitat
sar.QF <- nls(Spcs_QF ~ SAR(Area, c, z), data = matMHUb, start = list(c = 1, z = .25), lower = list(c = 0, z = 0), algorithm = "port")
summary(sar.QF)
sumofsquares(sar.QF)
AIC(sar.QF)

# ajusta os dados à SAR tradicional mas só para as espécies ubiquas, SAR multihabitat
sar.UB <- nls(Spcs_UB ~ SAR(Area, c, z), data = matMHUb, start = list(c = 1, z = .25), lower = list(c = 0, z = 0), algorithm = "port")
summary(sar.UB)
sumofsquares(sar.UB)
AIC(sar.UB)

# ajusta os dados à SAR tradicional em multihabitat
sar <- nls(Spcs_Tt ~ SAR(Area, c, z), data = matMHUb, start = list(c = 1, z = .25), trace = TRUE, algorithm = "port")
summary(sar)
sumofsquares(sar)
AIC(sar)

#desenha o gráfico da SAR tradicional
plot(Spcs_Tt ~ Area, data = matMHUb, log = "xy")
abline(log10(coef(sar)["c"]), coef(sar)["z"])
#lines(unique(Area),SAR(unique(Area),coef=coef(sar))) #alternativa ao abline
plot(Spcs_Tt ~ Area, data = matMHUb, ylim = c(0, 100))
x = seq(1, 10^8, 100)
lines(x, SAR(x, coef(sar)["c"], coef(sar)["z"]))


#COUNTRYSIDE
# ajusta os dados à SAR countryside em multi-habitat
csar.AG <- nls(Spcs_AG ~ CountrysideSAR(Area_AG, Area_SH, Area_QF, h1, h2, h3, z), data = matMHUb, start = list(h1 = 1, h2 = .005, h3 = .005, z = .15), lower = list(h1 = 10^-12, h2 = 10^-12, h3 = 10^-12, h4 = 10^-12, z = 0.01),
               trace = TRUE, algorithm = "port")
summary(csar.AG)
sumofsquares(csar.AG)
AIC(csar.AG)

csar.SH <- nls(Spcs_SH ~ CountrysideSAR(Area_AG, Area_SH, Area_QF, h1, h2, h3, z), data = matMHUb, start = list(h1 = .005, h2 = 1, h3 = .005, z = .15), lower = list(h1 = 10^-12, h2 = 10^-12, h3 = 10^-12, h4 = 10^-12, z = 0.01),
               trace = TRUE, algorithm = "port")
summary(csar.SH)
sumofsquares(csar.SH)
AIC(csar.SH)

csar.QF <- nls(Spcs_QF ~ CountrysideSAR(Area_AG, Area_SH, Area_QF, h1, h2, h3, z), data = matMHUb, start = list(h1 = .005, h2 = .005, h3 = 1, z = .15), lower=list(h1 = 10^-12, h2 = 10^-12, h3 = 10^-12, h4 = 10^-12, z = 0.01),
               trace = TRUE, algorithm = "port")
summary(csar.QF)
sumofsquares(csar.QF)
AIC(csar.QF)

csar.UB <- nls(Spcs_UB ~ CountrysideSAR(Area_AG, Area_SH, Area_QF, h1, h2, h3, z), data = matMHUb, start = list(h1 = .005, h2 = .005, h3 = .005, z = .15), lower = list(h1 = 10^-12, h2 = 10^-12, h3 = 10^-12, h4 = 10^-12, z = 0.01),
               trace = TRUE, algorithm = "port")
summary(csar.UB)
sumofsquares(csar.UB)
AIC(csar.UB)


TotalCSAR <- function (a1, a2, a3){
 sp.AG<-CountrysideSAR2(a1, a2, a3, coef(csar.AG))
 sp.UB<-CountrysideSAR2(a1, a2, a3, coef(csar.UB))	
 sp.SH<-CountrysideSAR2(a1, a2, a3, coef(csar.SH))
 sp.QF<-CountrysideSAR2(a1, a2, a3, coef(csar.QF))
 sp.AG + sp.SH + sp.QF + sp.UB
}

tcsar <- TotalCSAR(matMHUb$Area_AG, matMHUb$Area_SH, matMHUb$Area_QF)
sum((tcsar - matMHUb$Spcs_Tt)^2)
plot(Spcs_Tt ~ Area, data = matMHUb, log = "xy")
abline(log10(coef(sar)["c"]), coef(sar)["z"])
points(matMHUb$Area, tcsar, col = "red")
plot(matMHUb$Spcs_Tt, tcsar, col = "red")
points(matMHUb$Spcs_Tt, SAR(matMHUb$Area, coef(sar)["c"], coef(sar)["z"]))
abline(0, 1)
 