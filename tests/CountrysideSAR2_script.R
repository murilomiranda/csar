# escolher a directoria onde estao os dados

# tem que estar instaladas no sistema as bibliotecas gdata e gtools
library(gdata)

# carrega a matriz com dados
# nao pode haver caracteres com acentos no ficheiro Excel
mat <- read.table("Matriz.txt", head = TRUE)

# define SAR clássica
source('SAR.R')

source('SS.R')

# define SAR multihabitat
source('CountrysideSAR.R')

source('CountrysideSAR2.R')

# ajusta os dados à SAR tradicional
sar <- nls(Sp_T ~ SAR(Area, c, z), data = mat, start = list(c = 1, z = .25), trace = TRUE, algorithm = "port")
summary(sar)
sumofsquares(sar)
AIC(sar)

#desenha o gráfico da SAR tradicional
plot(Sp_T ~ Area, data = mat, log = "xy")
abline(log10(coef(sar)["c"]), coef(sar)["z"])
#lines(unique(Area),SAR(unique(Area),coef=coef(sar))) #alternativa ao abline
plot(Sp_T ~ Area, data = mat, ylim = c(0, 50))
x = seq(1, 10^8, 100)
lines(x, SAR(x, coef(sar)["c"], coef(sar)["z"]))

# ajusta os dados à SAR tradicional mas só para as espécies agricolas
sar.AG <- nls(Sp_AG ~ SAR(Area, c, z), data = mat, start = list(c = 1, z = .25), lower = list(c = 0, z = 0), algorithm = "port")
summary(sar.AG)
sumofsquares(sar.AG)
AIC(sar.AG)

# ajusta os dados à SAR tradicional mas só para as espécies agricolas em zonas agricolas
sar.AGAG <- nls(Sp_AG ~ SAR(Area, c, z), data = mat, start = list(c = 1, z = .25), lower = list(c = 0, z = 0), algorithm = "port", subset = Ar_AG > 0)
summary(sar.AGAG)
sumofsquares(sar.AGAG)
AIC(sar.AGAG)

# ajusta os dados à SAR tradicional mas só para as espécies ubiquas
sar.UB <- nls(Sp_Ub ~ SAR(Area, c, z), data = mat, start = list(c = 1, z = .25), lower = list(c = 0, z = 0), algorithm = "port")
summary(sar.UB)
sumofsquares(sar.UB)
AIC(sar.UB)

# ajusta os dados à SAR tradicional mas só para as espécies de carvalhal
sar.QF <- nls(Sp_QF ~ SAR(Area, c, z), data = mat, start = list(c = 1, z = .25))
summary(sar.QF)
plot(Sp_QF ~ Area, data = mat,log = "xy")
abline(log10(coef(sar.QF)["c"]), coef(sar.QF)["z"])
summary(sar.QF)
sumofsquares(sar.QF)
AIC(sar.QF)

# ajusta os dados à SAR tradicional mas só para as espécies de carvalhal em carvalhal
sar.QFQF <- nls(Sp_QF ~ SAR(Ar_QF, c, z), data = mat, start = list(c = 1, z = .25), subset = Ar_QF > 0)
summary(sar.QFQF)
#sel<-Quad %in% grep("C",Quad,value="TRUE")
#subset = sel & (AR_QF>0)
plot(Sp_QF ~ Ar_QF, data = mat, log = "xy", subset = Ar_QF > 0)
abline(log10(coef(sar.QFQF)["c"]),coef(sar.QFQF)["z"])

# ajusta os dados à SAR tradicional mas só para as espécies arbustos
sar.LS <- nls(Sp_SH ~ SAR(Area, c, z), data = mat, start = list(c = 1, z = .25), lower = list(c = 0, z = 0), algorithm = "port")
summary(sar.LS)
sumofsquares(sar.LS)
AIC(sar.LS)

# ajusta os dados à SAR countryside / multi-habitat
csar.AG <- nls(Sp_AG ~ CountrysideSAR(Ar_AG, Ar_SH, Ar_QF, h1, h2, h3, z), data = mat, start = list(h1 = .1, h2 = .1, h3 = .1, z = .25), lower = list(h1 = 10^-12, h2 = 10^-12, h3 = 10^-12, h4 = 10^-12, z = 0.01),
               trace = TRUE, algorithm = "port")
summary(csar.AG)
sumofsquares(csar.AG)
AIC(csar.AG)

csar.UB <- nls(Sp_Ub ~ CountrysideSAR(Ar_AG, Ar_SH, Ar_QF, h1, h2, h3, z), data = mat, start = list(h1 = .1, h2 = .1, h3 = .1, z = .25), lower = list(h1 = 10^-12, h2 = 10^-12, h3 = 10^-12, h4 = 10^-12, z = 0.01),
               trace = TRUE, algorithm = "port")
summary(csar.UB)
sumofsquares(csar.UB)
AIC(csar.UB)

csar.QF <- nls(Sp_QF ~ CountrysideSAR(Ar_AG, Ar_SH, Ar_QF, h1, h2, h3, z), data = mat, start = list(h1 = .1, h2 = .1, h3 = .1, z = .25), lower = list(h1 = 10^-12, h2 = 10^-12, h3 = 10^-12, h4 = 10^-12, z = 0.01),
               trace = TRUE, algorithm = "port")
summary(csar.QF)
sumofsquares(csar.QF)
AIC(csar.QF)

csar.LS <- nls(Sp_SH ~ CountrysideSAR(Ar_AG, Ar_SH, Ar_QF, h1, h2, h3, z), data = mat, start = list(h1 = .1, h2 = .1, h3 = .1, z = .25), lower = list(h1 = 10^-12, h2 = 10^-12, h3 = 10^-12, h4 = 10^-12, z = 0.01),
               trace = TRUE, algorithm = "port")
summary(csar.LS)
sumofsquares(csar.LS)
AIC(csar.LS)

source('TotalCSAR.R')

tcsar <- TotalCSAR(mat$Ar_AG, mat$Ar_SH, mat$Ar_QF)
sum((tcsar - mat$Sp_T)^2)
plot(Sp_T ~ Area, data = mat, log = "xy")
abline(log10(coef(sar)["c"]), coef(sar)["z"])
points(mat$Area, tcsar, col="red")
plot(mat$Sp_T, tcsar, col = "red")
points(mat$Sp_T, SAR(mat$Area, coef(sar)["c"], coef(sar)["z"]))
abline(0, 1)
 