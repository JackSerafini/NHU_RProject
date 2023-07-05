
# -------
## CACCA
# -------


## ANALISI DESCRITTIVA DEI DATI
# Vediamo come si distribuisce ciascuna variabile e poi quali sono le relazioni delle variabili
# con la variabile risposta


# Creazione del dataset da WiscNursingHome

Data <- read.csv("WiscNursingHome.csv", header = TRUE)


# Fattorizzazione delle variabili categoriali

Data$CRYEAR <- factor(Data$CRYEAR)
Data$URBAN <- factor(Data$URBAN)
Data$PRO <- factor(Data$PRO)
Data$TAXEXEMPT <- factor(Data$TAXEXEMPT)
Data$SELFFUNDINS <- factor(Data$SELFFUNDINS)
Data$MCERT <- factor(Data$MCERT)
Data$ORGSTR <- factor(Data$ORGSTR)
Data$MSA <- factor(Data$MSA)


# Richiamo delle librerie

library(corrplot)
library(ggplot2)
library(cowplot)
library(dplyr)
library(factoextra)

# Secondo me potremmo anche aggiungere una parte dove introduciamo brevemente il dataset (Jack)

# Il dataset 'WiscNursingHome' racchiude le informazioni riguardo diverse centinaia
# di case di riposo, informazioni che poi saranno utilizzate per prevedere il costo
# della struttura negli anni successivi.
# Nel dataset sono presenti 12 variabili:
# hospID: l'ID rappresentativo della singola struttura
# CRYEAR: il rapporto dei costi annuo
# TPY: il totale pazienti annui
# NUMBED: il numero di letti
# SQRFOOT: il numero di piedi quadrati della struttura
# MSA: codice dell'area statistica metropolitana (area divisa in 13 zone, più lo 0 se rurale)
# URBAN: 1 se urbana, O se rurale
# PRO: 0 se non-profit, 1 altrimenti
# TAXEXEMPT: 1 se esente dalle tasse
# SELFFUNDINS: 1 se autofinanziato per l'assicurazione
# MCERT: 1 se certificato Medicare
# ORGSTR: 1 se con finalità di lucro, 2 se esente dalle tasse, 3 se unità governativa

# L'obiettivo della nostra analisi sarà quindi quello di capire come le varie
# caratteristica di una casa di riposo sono tra loro collegate (se collegate in
# alcun modo), per poter così prevedere tutto ciò che concerne una struttura negli
# anni successivi, o perfino prevedere il futuro di una nuova struttura.

## Forse da aggiungere anche una parte di analisi del database (una roba piccolina)
summary(Data)
#

# Ci sono alcuni dati mancanti?
na.id.Data <- apply(is.na(Data), 2, which) 
# Abbiamo 10 dati mancanti in SQRFOOT
na.SQRFOOT <- na.id.Data$SQRFOOT
# Data frame senza NA
DataNa <- Data[-na.SQRFOOT,]

# Proposta di grafici di correlazione (forse più visibili)

# Dalle relazioni andiamo a togliere anche HospID perché ovviamente non c'è
# relazione tra l'ID del singolo ospedale e i vari risultati
pairs(DataNa[ , -c(1, 2, 6, 7, 8, 9, 10, 11, 12)], panel = panel.smooth)
cormat <- cor(DataNa[, -c(1, 2, 6, 7, 8, 9, 10, 11, 12)]) 
corrplot(cormat, method="number")

# Si può subito vedere come ci sia una correlazione quasi totale tra il totale
# patienti annui e il numero di letti. Inoltre, si può osservare anche un'ottima
# correlazione tra il totale pazienti annui e i piedi quadrati della struttura, 
# così come tra i piedi quadrati della struttura e il numero di letti.
# Ciò suggerisce che tutte queste variabili saranno legate tra loro da una
# relazione lineare.


## MESSO A COMMENTO TUTTA QUESTA PARTE CHE ORMAI È SUPERFLUA (ENRICO SCEGLI SE ELIMINARE)

# # Creazione dei grafici (orribili)
# # Parto con le variabili quantitative
# 
# p1 <- ggplot(data = Data, aes(x = TPY)) +
#   geom_histogram(aes(y = after_stat(density)), col = "black", fill = "yellow", bins = 20) +
#   theme_classic() +
#   xlab("") +
#   ylab("TPY") +
#   geom_density(col = "black", lwd = 0.75)
# 
# p2 <- ggplot(data = Data, aes(x = NUMBED)) +
#   geom_histogram(aes(y = after_stat(density)), col = "black", fill = "yellow", bins = 20) +
#   theme_classic() +
#   xlab("") +
#   ylab("") +
#   geom_density(col = "black", lwd = 0.75)
# 
# p3 <- ggplot(data = na.omit(Data), aes(x = SQRFOOT)) +
#   geom_histogram(aes(y = after_stat(density)), col = "black", fill = "yellow", bins = 20) +
#   theme_classic() +
#   xlab("SQRFOOT") +
#   ylab("") +
#   geom_density(col = "black", lwd = 0.75)
# 
# p1.2 <- ggplot(data = Data, aes(x = NUMBED, y = TPY)) +
#   geom_point() +
#   theme_classic() +
#   xlab("") +
#   ylab("")
# 
# p1.3 <- ggplot(data = na.omit(Data), aes(x = SQRFOOT, y = TPY)) +
#   geom_point() +
#   theme_classic() +
#   xlab("") +
#   ylab("")
# 
# p2.3 <- ggplot(data = na.omit(Data), aes(x = SQRFOOT, y = NUMBED)) +
#   geom_point() +
#   theme_classic() +
#   xlab("") +
#   ylab("")
# 
# p2.1 <- ggplot(data = Data, aes(x = TPY, y = NUMBED)) +
#   geom_point() +
#   theme_classic() +
#   xlab("") +
#   ylab("NUMBED")
# 
# p3.1 <- ggplot(data = na.omit(Data), aes(x = TPY, y = SQRFOOT)) +
#   geom_point() +
#   theme_classic()  +
#   xlab("TPY") +
#   ylab("SQRFOOT")
# 
# p3.2 <- ggplot(data = na.omit(Data), aes(x = NUMBED, y = SQRFOOT)) +
#   geom_point() +
#   theme_classic() +
#   xlab("NUMBED") +
#   ylab("")
# 
# 
# # Metto nella griglia tutti i grafici brutti
# 
# plot_grid(p1, p1.2, p1.3,  
#           p2.1, p2, p2.3,
#           p3.1, p3.2, p3,
#           nrow = 3, ncol = 3)

# Queste sono le relazioni tra le varie variabili

# Correlazione molto alta tra tutte e 3 le variabili
# In ottica di analisi di regressione occhio a multicollinearità
# (non so se si dice così forse me lo sono inventato)(penso di sì)


# Variabili qualitative

p4 <- ggplot(data = Data, aes(x = CRYEAR, fill = CRYEAR)) +
  geom_bar() +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("")

p5 <- ggplot(data = Data, aes(x = MSA, fill = MSA)) +
  geom_bar() +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("")

p6 <- ggplot(data = Data, aes(x = URBAN, fill = URBAN)) +
  geom_bar() +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("")

p7 <- ggplot(data = Data, aes(x = PRO, fill = PRO)) +
  geom_bar() +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("")

p8 <- ggplot(data = Data, aes(x = TAXEXEMPT, fill = TAXEXEMPT)) +
  geom_bar() +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("")

p9 <- ggplot(data = Data, aes(x = SELFFUNDINS, fill = SELFFUNDINS)) +
  geom_bar() +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("")

p10 <- ggplot(data = Data, aes(x = MCERT, fill = MCERT)) +
  geom_bar() +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("")

p11 <- ggplot(data = Data, aes(x = ORGSTR, fill = ORGSTR)) +
  geom_bar() +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("")

# Grafico a torta della variabile MSA

ppie <- ggplot(Data, aes(x="", y="", fill=MSA)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void()


# Grafico contemporaneamente tutti i grafici delle variabili categoriali

plot_grid(p4,p6,p7,p11,
          p8,p9,p10,ppie,
          nrow = 2)

# NB per non vedere sminchiata la legenda bisogna mettere il grafico a schermo intero
# ancora da capire come sistemare questo problema

# Vediamo le distribuzioni condizionate della variabile risposta

c4 <- ggplot(data = Data, aes(x = CRYEAR, y = TPY, fill = CRYEAR)) +
  geom_boxplot() +
  theme_classic() +
  theme(legend.position = "")

c5 <- ggplot(data = Data, aes(x = URBAN, y = TPY, fill = URBAN)) +
  geom_boxplot() +
  theme_classic() +
  theme(legend.position = "")

c6 <- ggplot(data = Data, aes(x = PRO, y = TPY, fill = PRO)) +
  geom_boxplot() +
  theme_classic() +
  theme(legend.position = "")

c7 <- ggplot(data = Data, aes(x = ORGSTR, y = TPY, fill = ORGSTR)) +
  geom_boxplot() +
  theme_classic() +
  theme(legend.position = "")

c8 <- ggplot(data = Data, aes(x = TAXEXEMPT, y = TPY, fill = TAXEXEMPT)) +
  geom_boxplot() +
  theme_classic() +
  theme(legend.position = "")

c9 <- ggplot(data = Data, aes(x = SELFFUNDINS, y = TPY, fill = SELFFUNDINS)) +
  geom_boxplot() +
  theme_classic() +
  theme(legend.position = "")

c10 <- ggplot(data = Data, aes(x = MCERT, y = TPY, fill = MCERT)) +
  geom_boxplot() +
  theme_classic() +
  theme(legend.position = "")

c11 <- ggplot(data = Data, aes(x = MSA, y = TPY, fill = MSA)) +
  geom_boxplot() +
  theme_classic() +
  theme(legend.position = "")

plot_grid(c4, c5, c6, c7,
          c8, c9, c10, c11,
          nrow = 2)

# Come sempre il grafico di MSA è strano perché ha troppe determinazioni




## REGRESSIONI LINEARI PER LA STIMA DI TPY
# Proviamo prima le regressioni semplici con NUMBED e SQRFOOT
# Bisogna probabilmente scegliere una sola delle due variabili perché sono troppo correlate
# Poi proviamo ad aggiungere tutte le variabili qualitative e vediamo come incidono
# Modello ideale: 1 quantitativa + 1/2 qualitative (non è detto per forza)


# Costruzione del modello
fit_NUMBED <- lm(Data$TPY ~ Data$NUMBED)
# plot
par(mfrow = c(2,2))
plot(fit_NUMBED)
par(mfrow = c(1,1))
# Il grafico dei residui è tipo perfetto
# Viene violata un po la condizione di normalità sulle code
# Ci sono un paio di valori estremi: sopreatutto il 564
Data[564,]
#interpretazione dei valori
summary(fit_NUMBED)
#' Residui di nuovo molto buoni, hanno la mediana praticamente sullo zero
#' intercetta vicino allo zero: ha senso ovvero se hai zero posti letto ti 
#' aspetti un numero di pazienti vicino allo zero
#' Secondo coefficiente vicino all'1 ciò significa che per ogni posto letto in più
#' ci si aspetta un paziente in più all'anno
#' Attenzione il t value dell'intercetta non è motlo significativo al contrario dell'
#' altro (che non mi viene il nome)
#' L'R quadro si avvicina ad 1 quindi molto buono
#' se non fosse per la poca significatività dell'intercetta potrebbe essere un 
#' modello quasi perfetto

#Diagramma di dispersione + retta di regressione lineare
#Metto in evidenza il punto che era estremo sul grafico dei residui
ggplot(data = Data, aes(x = NUMBED, y = TPY)) +
  geom_point() +
  geom_smooth(se = F, method = 'lm') +
  theme_classic()+
  geom_point(aes(x = Data[564,'NUMBED'], y = Data[564,'TPY']), colour = "red")

# Costruzione del modello
fit_SQRFOOT <- lm(Data$TPY ~ Data$SQRFOOT)
# plot
par(mfrow = c(2,2))
plot(fit_SQRFOOT)
par(mfrow = c(1,1))
#' anche qui il grafico dei resuidi è molto buono
#' la condizione di normalità viene meglio rispettata
#' nell'ultimo grafico ci sono più valori estremi: 564, 200, 557

# interpretazione dei valori
summary(fit_SQRFOOT)
#' Residui variano un bel po': la differenza fra il minimo e il massimo è di 140, ma
#' la maggior parte dei valori sono compresi fra -15, e 15. Quindi come già visto prima
#' oltre ad alcuni valori estremi i residui sono abbastanza piccoli
#' I parametrei stimati sono entrambi molto significativi
#' intercetta a 33
#' la pendenza della retta vicino a 1 quindi circa per ogni piede quadrato c'è un paziente
#' in più all'anno
#' l'Rquadro è 0.6751, ovvero vengono spiegati il 67% dei dati

# Diagramma di dispersione + retta di regressione lineare
# Metto in evidenza i punti che era esrtemo sul grafico dei residui
# Problema mi da dei warnins message
ggplot(data = Data, aes(x = SQRFOOT, y = TPY)) +
  geom_point() +
  geom_smooth(se = F, method = 'lm') +
  theme_classic() + 
  geom_point(aes(x = Data[564,'SQRFOOT'], y = Data[564,'TPY']), colour = "red")  +
  geom_point(aes(x = Data[200,'SQRFOOT'], y = Data[200,'TPY']), colour = "red")  +
  geom_point(aes(x = Data[557,'SQRFOOT'], y = Data[557,'TPY']), colour = "red")  

#Proviamo Un modello logaritmico
# Costruzione del modello
fit_loglogSQRFOOT <- lm(log(Data$TPY) ~ log(Data$SQRFOOT))
# plot
par(mfrow = c(2,2))
plot(fit_loglogSQRFOOT)
par(mfrow = c(1,1))
#' con il doppio log si va alla vittoria
#' il grafico dei residui è bellissimo e anche quello della qq-norm
#' ci sono pochi valori estremi e sono meno estremi di prima

# interpretazione dei valori
summary(fit_loglogSQRFOOT)
#' La varianza dei residui è molto bassa
#' R^2 spiega il 53% dei dati, ma magari aggiungendo una variabile si va a migliorare

cor(log(Data$TPY), log(Data$SQRFOOT))

#scater plot
ggplot(data = Data, aes(x = log(SQRFOOT), y = log(TPY))) +
  geom_point() +
  geom_smooth(se = F, method = 'lm') +
  theme_classic()
# Niente da dire bellisimo


# Così per provare magari dopo non si aggiunge aggiungiamo anche l'alra variabile
# quantitativa

fit_loglogSQRFOOT_NUMBED <- lm(log(Data$TPY) ~ log(Data$SQRFOOT) + Data$NUMBED)

summary(fit_loglogSQRFOOT_NUMBED)

ggplot(data = DataNa, aes(x = log(SQRFOOT), y = log(TPY))) +
  geom_point(na.rm = T) +
  geom_smooth(se = F, method = 'lm',formula = y ~ x * DataNa$NUMBED) +
  theme_classic()



## INFERENZA SUI RISULTATI
# Boh ho aggiunto questa sezione perché può essere simpatico e farci prendere qualche voto in più
# Penso che sia sufficiente cercare su internet quali sono i test di ipotesi migliori
# in base al modello di regressione che ci uscirà
### Assolutamente d'accordo (Jack)



## TRAIN TEST E TEST SET
# Dividere il dataset in due parti, il train set e il test set
# Applicare il modello di regressione individuato al train set
# Vedere se riusciamo a prevedere così il test set
# Tipo una sorta di test per vedere se funziona



## CLUSTERING

# Provo a fare del clustering
library(cluster)

# Stampo di nuovo le quantitative
ggplot(data = Data, aes(x = NUMBED, y = TPY)) +
  geom_point() +
  theme_classic()
ggplot(data = na.omit(Data), aes(x = SQRFOOT, y = TPY)) +
  geom_point() +
  theme_classic()

# Provo a farlo utilizzando solo le variabili quantitative (per ora)

# Costruisco le distanze (euclidee per variabili quantitative)
dist.numbed <- daisy(scale(Data[,c("NUMBED", "TPY")]))
as.matrix(dist.numbed)[1:5,1:5] #matrice delle distanze
dist.sqrfoot <- daisy(scale(Data[,c("SQRFOOT", "TPY")]))
as.matrix(dist.sqrfoot)[1:5,1:5] #matrice delle distanze
# NB la funzione scale serve a standardizzare il dataset

# Provo il clustering con il metodo delle k medie
km.numbed <- kmeans(scale(Data[,c("NUMBED", "TPY")]), centers = 2)
km.sqrfoot <- kmeans(scale(na.omit(Data[,c("SQRFOOT", "TPY")])), centers = 2)
# Numerosità nei due cluster
table(km.numbed$cluster)
table(km.sqrfoot$cluster)
# Numerosità molto asimmetriche

# Dataframe di supporto per la rappresentazione grafica
data.centers.numbed <- data.frame(NUMBED = km.numbed$centers[,1]*sd(Data$NUMBED)+mean(Data$NUMBED),
                                  TPY = km.numbed$centers[,2]*sd(Data$TPY)+mean(Data$TPY))
data.centers.sqrfoot <- data.frame(SQRFOOT = km.sqrfoot$centers[,1]*sd(na.omit(Data$SQRFOOT))+mean(na.omit(Data$SQRFOOT)),
                                  TPY = km.sqrfoot$centers[,2]*sd(Data$TPY)+mean(Data$TPY))
# Visualizzo graficamente i clustering
ggplot(Data, aes(x = NUMBED, y = TPY, col = factor(km.numbed$cluster))) +
  geom_point() + 
  theme_classic() +
  theme(legend.position = "") +
  geom_point(data = data.centers.numbed, aes(x = NUMBED, y = TPY), col = "black")
ggplot(na.omit(Data), aes(x = SQRFOOT, y = TPY, col = factor(km.sqrfoot$cluster))) +
  geom_point() + 
  theme_classic() +
  theme(legend.position = "") +
  geom_point(data = data.centers.sqrfoot, aes(x = SQRFOOT, y = TPY), col = "black")

# Varianze between
km.numbed$tot.withinss
km.numbed$betweenss
km.sqrfoot$tot.withinss
km.sqrfoot$betweenss
# Teoricamente il primo clustering (con numbed) è migliore perché ha varianza within minore

# Provo a fare cluster con dimensioni diverse da 2 a 10
# Li valuto stampando le varianze within
crit.n<-0
crit.s<-0
for (i in 2:10) {
  set.seed(7)
  group.numbed<-kmeans(scale(Data[,c("NUMBED", "TPY")]), i, nstart=10)
  group.sqrfoot<-kmeans(scale(na.omit(Data[,c("SQRFOOT", "TPY")])), i, nstart=10)
  crit.n[i-1]<-group.numbed$tot.withinss
  crit.s[i-1]<-group.sqrfoot$tot.withinss
}
clsdb <- data.frame(index = 2:10, crit.n, crit.s)
# Visualizzazione grafica dei risultati
ggplot(clsdb, aes(index, crit.n)) +
  geom_point() +
  geom_line() +
  theme_classic()
ggplot(clsdb, aes(index, crit.s)) +
  geom_point() +
  geom_line() +
  theme_classic()
# In entrambi i casi il "gomito" sembra essere sul 3
# Ad occhio NUMBED sembra meglio, ma nessuno dei due sembra tanta roba

# Provo il clustering con metodo PAM
pam.numbed <- pam(scale(Data[,c("NUMBED", "TPY")]), 2, metric = "euclidean")
pam.numbed$medoids
pam.sqrfoot <- pam(scale(na.omit(Data[,c("SQRFOOT", "TPY")])), 2, metric = "euclidean")
pam.sqrfoot$medoids

# Dataframe di supporto per la rappresentazione grafica
data.medoids.numbed <- data.frame(NUMBED = pam.numbed$medoids[,1]*sd(Data$NUMBED)+mean(Data$NUMBED),
                                  TPY = pam.numbed$medoids[,2]*sd(Data$TPY)+mean(Data$TPY))
data.medoids.sqrfoot <- data.frame(SQRFOOT = pam.sqrfoot$medoids[,1]*sd(na.omit(Data$SQRFOOT))+mean(na.omit(Data$SQRFOOT)),
                                   TPY = pam.sqrfoot$medoids[,2]*sd(Data$TPY)+mean(Data$TPY))
# Visualizzazione grafica
ggplot(Data, aes(x = NUMBED, y = TPY, col = factor(pam.numbed$cluster))) +
  geom_point() + 
  theme_classic() +
  theme(legend.position = "") +
  geom_point(data = data.medoids.numbed, aes(x = NUMBED, y = TPY), col = "black")
ggplot(na.omit(Data), aes(x = SQRFOOT, y = TPY, col = factor(km.sqrfoot$cluster))) +
  geom_point() + 
  theme_classic() +
  theme(legend.position = "") +
  geom_point(data = data.medoids.sqrfoot, aes(x = SQRFOOT, y = TPY), col = "black")

# Grafico della silhouette
sil <- silhouette(pam.numbed$cluster, dist(scale(Data[,c("NUMBED", "TPY")])))
fviz_silhouette(sil)
sil <- silhouette(pam.sqrfoot$cluster, dist(scale(na.omit(Data[,c("SQRFOOT", "TPY")]))))
fviz_silhouette(sil)
# Silhouette terribili, forse era meglio il metodo delle k-means

# Per il clustering cumulato e condizonato alle variabili categoriale aspettiamo regressione