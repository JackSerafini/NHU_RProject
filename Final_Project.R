
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

library(ggplot2)
library(cowplot)
library(dplyr)


# Creazione dei grafici (orribili)
# Parto con le variabili quantitative

p1 <- ggplot(data = Data, aes(y = TPY)) +
  geom_boxplot(fill = "yellow") +
  theme_classic() +
  xlab("") +
  ylab("TPY")

p2 <- ggplot(data = Data, aes(y = NUMBED)) +
  geom_boxplot(fill = "yellow") +
  theme_classic() +
  xlab("") +
  ylab("")

p3 <- ggplot(data = na.omit(Data), aes(y = SQRFOOT)) +
  geom_boxplot(fill = "yellow") +
  theme_classic() +
  xlab("SQRFOOT") +
  ylab("")

p1.2 <- ggplot(data = Data, aes(x = NUMBED, y = TPY)) +
  geom_point() +
  theme_classic() +
  xlab("") +
  ylab("")

p1.3 <- ggplot(data = na.omit(Data), aes(x = SQRFOOT, y = TPY)) +
  geom_point() +
  theme_classic() +
  xlab("") +
  ylab("")

p2.3 <- ggplot(data = na.omit(Data), aes(x = SQRFOOT, y = NUMBED)) +
  geom_point() +
  theme_classic() +
  xlab("") +
  ylab("")

p2.1 <- ggplot(data = Data, aes(x = TPY, y = NUMBED)) +
  geom_point() +
  theme_classic() +
  xlab("") +
  ylab("NUMBED")

p3.1 <- ggplot(data = na.omit(Data), aes(x = TPY, y = SQRFOOT)) +
  geom_point() +
  theme_classic()  +
  xlab("TPY") +
  ylab("SQRFOOT")

p3.2 <- ggplot(data = na.omit(Data), aes(x = NUMBED, y = SQRFOOT)) +
  geom_point() +
  theme_classic() +
  xlab("NUMBED") +
  ylab("")


# Metto nella griglia tutti i grafici brutti

plot_grid(p1, p1.2, p1.3,  
          p2.1, p2, p2.3,
          p3.1, p3.2, p3,
          nrow = 3, ncol = 3)

# Queste sono le relazioni tra le varie variabili

# Stampo le correlazioni

cor(Data$TPY, Data$NUMBED)
cor(na.omit(Data)$TPY, na.omit(Data)$SQRFOOT)
cor(na.omit(Data)$NUMBED, na.omit(Data)$SQRFOOT)

# Correlazione molto alta tra tutte e 3 le variabili
# In ottica di analisi di regressione occhio a multicollinearit?
# (non so se si dice cos? forse me lo sono inventato)


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

# Grafico a torta gnam

ppie <- ggplot(Data, aes(x="", y="", fill=MSA)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void()


# Metto nella griglia tutti i grafici brutti

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

# Come sempre il grafico di MSA ? strano perch? ha troppe determinazioni


## REGRESSIONI LINEARI PER LA STIMA DI TPY
# Proviamo prima le regressioni semplici con NUMBED e SQRFOOT
# Bisogna probabilmente scegliere una sola delle due variabili perch? sono troppo correlate
# Poi proviamo ad aggiungere tutte le variabili qualitative e vediamo come incidono
# Modello ideale: 1 quantitativa + 1/2 qualitative (non ? detto per forza)


## INFERENZA SUI RISULTATI
# Boh ho aggiunto questa sezione perch? pu? essere simpatico e farci prendere qualche voto in pi?
# Penso che sia sufficiente cercare su internet quali sono i test di ipotesi migliori
# in base al modello di regressione che ci uscir?


## CLUSTERING
# Pu? essere figo provare a fare qualcosa, ma devo ancora recuperarmi la teoria quindi non so

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

# Visualizzo graficamente i clustering
ggplot(Data, aes(x = NUMBED, y = TPY, col = factor(km.numbed$cluster))) +
  geom_point() + 
  theme_classic() +
  theme(legend.position = "") 
ggplot(na.omit(Data), aes(x = SQRFOOT, y = TPY, col = factor(km.sqrfoot$cluster))) +
  geom_point() + 
  theme_classic() +
  theme(legend.position = "") 

# Varianze between
km.numbed$tot.withinss
km.numbed$betweenss
km.sqrfoot$tot.withinss
km.sqrfoot$betweenss
# Teoricamente il primo clustering (con numbed) è migliore perché ha varianza within minore
