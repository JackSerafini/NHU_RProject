
## ANALISI DESCRITTIVA DEI DATI ------------------------------------------------
# Vediamo come si distribuisce ciascuna variabile e poi quali sono le relazioni delle variabili
# con la variabile risposta

# Creazione del dataset da WiscNursingHome
Data <- read.csv("WiscNursingHome.csv", header = TRUE)

# Fattorizzazione delle variabili categoriali
Data$CRYEAR <- factor(Data$CRYEAR)
Data$MSA <- factor(Data$MSA)
Data$URBAN <- factor(Data$URBAN)
Data$PRO <- factor(Data$PRO)
Data$TAXEXEMPT <- factor(Data$TAXEXEMPT)
Data$SELFFUNDINS <- factor(Data$SELFFUNDINS)
Data$MCERT <- factor(Data$MCERT)
Data$ORGSTR <- factor(Data$ORGSTR)

# Richiamo delle librerie
library(corrplot)
library(ggplot2)
library(cowplot)
library(dplyr)
library(factoextra)

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
# caratteristiche di una casa di riposo sono tra loro collegate (se collegate in
# alcun modo), per poter così prevedere tutto ciò che concerne una struttura negli
# anni successivi, o perfino prevedere il futuro di una nuova struttura.
# In maniera particolare, vogliamo analizzare quali sono le principali peculiarità
# di una struttura che portano ad avere un numero maggiore di pazienti.

## Forse da aggiungere anche una parte di analisi del database (una roba piccolina)
summary(Data)
#

# Ci sono alcuni dati mancanti?
na.id.Data <- apply(is.na(Data), 2, which) 
# Abbiamo 10 dati mancanti in SQRFOOT
na.SQRFOOT <- na.id.Data$SQRFOOT
# Data frame senza NA
DataNa <- Data[-na.SQRFOOT,]

# DataNa è significativo per il dataset?
cor(DataNa$NUMBED, DataNa$SQRFOOT)
# Le due variabili sono estremamente correlate
# Vediamo quali sono i dati di NUMBED che corrispondono ai valori mancanti
ggplot(data = DataNa, aes(x = NUMBED, y = TPY)) +
  geom_point() +
  geom_point(data = Data[na.SQRFOOT,], aes(x = NUMBED, y = TPY), col = "red") +
  theme_bw()
# I dati mancanti sembrano essere effettivamente dei dati "a caso"
# Anche se concentrati negli ospedali più piccoli


## Grafici di correlazione -----------------------------------------------------

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


# In ottica di analisi di regressione occhio a multicollinearità
# (non so se si dice così forse me lo sono inventato)(penso di sì)


# Istogrammi delle variabili qualitative

p1 <- ggplot(data = Data, aes(x = CRYEAR, fill = CRYEAR)) +
  geom_bar() +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("")

p2 <- ggplot(data = Data, aes(x = MSA, fill = MSA)) +
  geom_bar() +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("")

p3 <- ggplot(data = Data, aes(x = URBAN, fill = URBAN)) +
  geom_bar() +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("")

p4 <- ggplot(data = Data, aes(x = PRO, fill = PRO)) +
  geom_bar() +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("")

p5 <- ggplot(data = Data, aes(x = TAXEXEMPT, fill = TAXEXEMPT)) +
  geom_bar() +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("")

p6 <- ggplot(data = Data, aes(x = SELFFUNDINS, fill = SELFFUNDINS)) +
  geom_bar() +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("")

p7 <- ggplot(data = Data, aes(x = MCERT, fill = MCERT)) +
  geom_bar() +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("")

p8 <- ggplot(data = Data, aes(x = ORGSTR, fill = ORGSTR)) +
  geom_bar() +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("")

# Grafico a torta della variabile MSA
ppie <- ggplot(Data, aes(x="", y="", fill=MSA)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() 

# Grafico di tutti i grafici delle variabili categoriali
plot_grid(p1,p3,p4,p8,
          p5,p6,p7,ppie,
          nrow = 2)

# NB per non vedere sminchiata la legenda bisogna mettere il grafico a schermo intero
# ancora da capire come sistemare questo problema (il problema è dato dal fatto che la 
# finestra di visualizzazione è troppo piccola, non c'è una vera risoluzione)

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

# mostrano la distribuzione della variabile risposta rispetto alle determinazioni della variabile su x
# se i boxplot sono moolto simili tra le determinazioni probailmente la variabile sulle x è ininfluente riguardo TPY
# se invece sono traslati si possono valutare per l'inserimento in un modello
# i vari puntini sono outliers


# Funzione per visualizzare la bontà dei residui graficamente
resiplot <- function(fit, p) {
  #p è il primo grafico in alto a sinistra, così non c'è il rischio che gli si passi un p a caso
  
  
  #per capire che data set utilizzare
  if(length(fitted.values(fit)) == 717){
    d = Data
    print("resi plot utilizzando Data")
  }
  if(length(fitted.values(fit)) == 707){
    d = DataNa
    print("resi plot utilizzando DataNa")
  }
  
  f <- ggplot(data = d, aes(x = fitted.values(fit), y = resid(fit))) +
    geom_point(shape=1) +
    theme_bw() +
    xlab("Valori fittati") +
    ylab("Residui") +
    geom_hline(yintercept = 0, col = "black", lty = 2) +
    geom_smooth(se = F, method = 'loess', formula = 'y ~ x', lwd = 0.75, col = "red")
  f1 <- ggplot(data = d, mapping = aes(resid(fit))) +
    geom_histogram(aes(y =after_stat(density)),bins = 20, col = "black", fill = "yellow", alpha = 1) + 
    geom_density(linewidth = 0.8, fill = "pink", alpha = 0.3) +
    theme_bw() +
    xlab("Residui") +
    ylab("Densità")
  f2 <- ggplot(data.frame(resid = rstandard(fit)),aes(sample = resid)) + 
    stat_qq(shape=1) +
    stat_qq_line(color = "red", linewidth = 1) +
    theme_bw() +
    xlab("Quantili teorici normale") +
    ylab("Quantili empirici")

  plot_grid(p, f,
            f1, f2,
            nrow = 2)
}

## REGRESSIONI LINEARI PER LA STIMA DI TPY -------------------------------------

# Proviamo prima le regressioni semplici con NUMBED e SQRFOOT
# Bisogna probabilmente scegliere una sola delle due variabili perché sono troppo correlate
# Poi proviamo ad aggiungere tutte le variabili qualitative e vediamo come incidono
# Modello ideale: 1 quantitativa + 1/2 qualitative (non è detto per forza)


# Costruzione del modello TPY ~ NUMBED -----
fit_NUMBED <- lm(TPY ~ NUMBED, data = Data)

# Summary
summary(fit_NUMBED)
# Studiando i valori ritornati dal summary, si può subito vedere come
# questo modello sia ottimo: per quanto riguarda i residui, la mediana è molto
# vicina allo zero, il primo e il terzo quartile sono disposti abbastanza
# simmetricamente rispetto lo zero e discorso simile può essere fatto per i
# valori di minimo e massimo (magari il minimo è spostato più verso il basso,
# ma stiamo comunque parlando di valori "piccoli").
# Dal summary possiamo poi conoscere l'intercetta e il coefficiente angolare: l'intercetta
# vale -0.8778 (valore vicino allo zero, il che ha senso perché nel caso di 0
# letti ci si aspetta un numero di pazienti vicino allo zero) mentre il
# coefficiente angolare è uguale a 0.9272 (valore vicino all'1, infatti per ogni
# posto letto in più ci si aspetta un paziente in più). Grazie a questi valori
# possiamo costruirci l'equazione della retta di regressione lineare:
# TPY = -0.8778 + 0.9272 * NUMBED.
# Infine, studiando la statistica R^2, possiamo vedere un valore pari a 0.9678,
# il quale suggerisce un'aderenza del modello ai dati molto alta, quasi totale, circa del 97%.
# (Attenzione il t value dell'intercetta non è motlo significativo al contrario dell'altro (che non mi viene il nome (coefficiente?)))
# (se non fosse per la poca significatività dell'intercetta potrebbe essere un modello quasi perfetto)

# Plot
par(mfrow = c(2,2))
plot(fit_NUMBED)
par(mfrow = c(1,1))
# Analizzando i grafici, dal primo dei Residuals vs Fitted si può notare come, nonostante
# siano presenti degli outliers (sono outliers??) al di sotto della curva di regressione, 
# i residui si dispongono in maniera per lo più simmetrica, suggerendo la linearità
# del modello. 
# Guardando il grafico qqnorm dei residui, possiamo nuovamente osservare un'ottima 
# disposizione dei residui lungo la retta tratteggiata, il che significa che il nostro
# modello soddisfa l'assunzione di gaussianità.

# Grafico della regressione
p <- ggplot(data = Data, aes(x = NUMBED, y = TPY)) +
   geom_point(shape=1) +
   theme_bw() +
   xlab("NUMBED") +
   ylab("TPY") +
   geom_smooth(se = F, method = 'lm', formula = 'y ~ x', lwd = 0.75, col = "red")
# Residui:
resiplot(fit_NUMBED, p)

# Il grafico dei residui è tipo perfetto
# Viene violata un po la condizione di normalità sulle code
# Ci sono un paio di valori estremi: sopreatutto il 564
Data[564,]

#Diagramma di dispersione + retta di regressione lineare
#Metto in evidenza il punto che era estremo sul grafico dei residui
ggplot(data = Data, aes(x = NUMBED, y = TPY)) +
  geom_point(shape=1) +
  geom_smooth(se = F, method = 'lm', formula = 'y ~ x', lwd = 0.75, col = "red") +
  theme_classic()+
  geom_point(aes(x = Data[564,'NUMBED'], y = Data[564,'TPY']), colour = "red", size = 2)
# Secondo me si potrebbe direttamente metterlo nel grafico sopra (JACK)


# Costruzione del modello TPY ~ SQRFOOT -----
fit_SQRFOOT <- lm(TPY ~ SQRFOOT, data = Data)

# Summary
summary(fit_SQRFOOT)
# Studiando i residui dal summary, si può vedere questi si dispongano simmetricamente
# rispetto lo zero, nonostante fra il minimo ed il massimo ci sia una differenza di
# 240, molto maggiore rispetto a quella riscontrata nel modello TPY ~ NUMBED. La
# mediana si dispone comunque vicino allo 0, ed il primo ed il terzo quartile valgono
# rispettivamente -15.391 e 15.615, oltre ad essere assolutamente simmetrici, ci
# dicono anche che la maggior parte dei valori si troverà proprio in questo intervallo.
# In questo modello l'intercetta vale 33.5475 mentre il coefficiente angolare è
# pari a 1.1179 (molto vicino a 1, quindi circa per ogni piede quadrato c'è un
# paziente in più) (entrambi molto significativi?). Quindi, la retta di regressione
# lineare risulta: TPY = 33.5475 + 1.1179 * SQRFOOT.
# Infine, l'R^2 risulta pari a 0.6756, quindi abbiamo un'aderenza del modello di
# circa il 68%, quindi di nuovo alta nonostante minore rispetto a quella del modello precedente.

# Plot
par(mfrow = c(2,2))
plot(fit_SQRFOOT)
par(mfrow = c(1,1))
#

# Con ggplot
p <- ggplot(data = DataNa, aes(x = SQRFOOT, y = TPY)) +
  geom_point(shape=1) +
  theme_bw() +
  xlab("Piedi quadrati") +
  ylab("Posti occupati all'anno") +
  geom_smooth(se = F, method = 'lm', formula = 'y ~ x', lwd = 0.75, col = "red")

# Residui:
resiplot(fit_SQRFOOT, p)
#' anche qui il grafico dei resuidi è molto buono
#' la condizione di normalità viene meglio rispettata
#' nell'ultimo grafico ci sono più valori estremi: 564, 200, 557


# Diagramma di dispersione + retta di regressione lineare
# Metto in evidenza i punti che era esrtemo sul grafico dei residui
# Problema mi da dei warnins message
ggplot(data = DataNa, aes(x = SQRFOOT, y = TPY)) +
  geom_point(shape=1) +
  geom_smooth(se = F, method = 'lm', col = "red") +
  theme_classic() + 
  geom_point(aes(x = Data[564,'SQRFOOT'], y = Data[564,'TPY']), colour = "red")  +
  geom_point(aes(x = Data[200,'SQRFOOT'], y = Data[200,'TPY']), colour = "red")  +
  geom_point(aes(x = Data[557,'SQRFOOT'], y = Data[557,'TPY']), colour = "red")  

#Proviamo Un modello logaritmico
# Costruzione del modello
fit_loglogSQRFOOT <- lm(log(TPY) ~ log(SQRFOOT), data = Data)

# interpretazione dei valori
summary(fit_loglogSQRFOOT)
#' La varianza dei residui è molto bassa
#' R^2 spiega il 53% dei dati, ma magari aggiungendo una variabile si va a migliorare

# Plot
par(mfrow = c(2,2))
plot(fit_loglogSQRFOOT)
par(mfrow = c(1,1))

#' con il doppio log si va alla vittoria
#' il grafico dei residui è bellissimo e anche quello della qq-norm
#' ci sono pochi valori estremi e sono meno estremi di prima

cor(log(Data$TPY), log(Data$SQRFOOT))

# scatter plot
ggplot(data = Data, aes(x = log(SQRFOOT), y = log(TPY))) +
  geom_point() +
  geom_smooth(se = F, method = 'lm') +
  theme_classic()
# Niente da dire bellissimo


# Così per provare magari dopo non si aggiunge aggiungiamo anche l'alra variabile
# quantitativa

fit_loglogSQRFOOT_NUMBED <- lm(log(Data$TPY) ~ log(Data$SQRFOOT) + Data$NUMBED)

summary(fit_loglogSQRFOOT_NUMBED)


#' Proviamo a vedere i modelli con anche le variabili qualitative
#' Analisi preliminare grafica:
#' Stampo ogni fit colorando i pallini in base alle determinazioni delle variabili categoriche
#' Probabilmente seguono tante righe di codice (inutili perché stampano solo grafici)
ggplot(data = Data, aes(x = NUMBED, y = TPY, col = CRYEAR)) +
  geom_point() +
  theme_bw()
ggplot(data = Data, aes(x = NUMBED, y = TPY, col = URBAN)) +
  geom_point() +
  theme_bw()
ggplot(data = Data, aes(x = NUMBED, y = TPY, col = PRO)) +
  geom_point() +
  theme_bw()
ggplot(data = Data, aes(x = NUMBED, y = TPY, col = TAXEXEMPT)) +
  geom_point() +
  theme_bw()
ggplot(data = Data, aes(x = NUMBED, y = TPY, col = SELFFUNDINS)) +
  geom_point() +
  theme_bw()
ggplot(data = Data, aes(x = NUMBED, y = TPY, col = MCERT)) +
  geom_point() +
  theme_bw()
ggplot(data = Data, aes(x = NUMBED, y = TPY, col = ORGSTR)) +
  geom_point() +
  theme_bw()
ggplot(data = Data, aes(x = NUMBED, y = TPY, col = MSA)) +
  geom_point() +
  theme_bw()
#' Non sembrano emergere relazioni interessanti
#' Vediamo i grafici rispetto a SQRFOOT
ggplot(data = Data, aes(x = SQRFOOT, y = TPY, col = CRYEAR)) +
  geom_point() +
  theme_bw()
ggplot(data = Data, aes(x = SQRFOOT, y = TPY, col = URBAN)) +
  geom_point() +
  theme_bw()
ggplot(data = Data, aes(x = SQRFOOT, y = TPY, col = PRO)) +
  geom_point() +
  theme_bw()
ggplot(data = Data, aes(x = SQRFOOT, y = TPY, col = TAXEXEMPT)) +
  geom_point() +
  theme_bw()
ggplot(data = Data, aes(x = SQRFOOT, y = TPY, col = SELFFUNDINS)) +
  geom_point() +
  theme_bw()
ggplot(data = Data, aes(x = SQRFOOT, y = TPY, col = MCERT)) +
  geom_point() +
  theme_bw()
ggplot(data = Data, aes(x = SQRFOOT, y = TPY, col = ORGSTR)) +
  geom_point() +
  theme_bw()
ggplot(data = Data, aes(x = SQRFOOT, y = TPY, col = MSA)) +
  geom_point() +
  theme_bw()
#' Anche ora non sono chiarissimi, ma sembra esserci qualcosa
#' Vediamo la trasformazione del doppio logaritmo
ggplot(data = Data, aes(x = log(SQRFOOT), y = log(TPY), col = CRYEAR)) +
  geom_point() +
  theme_bw()
ggplot(data = Data, aes(x = log(SQRFOOT), y = log(TPY), col = URBAN)) +
  geom_point() +
  theme_bw()
ggplot(data = Data, aes(x = log(SQRFOOT), y = log(TPY), col = PRO)) +
  geom_point() +
  theme_bw()
# PRO sembra avere un po' di incidenza (concordo leo)
ggplot(data = Data, aes(x = log(SQRFOOT), y = log(TPY), col = TAXEXEMPT)) +
  geom_point() +
  theme_bw()
# Anche TAXEXEMPT
ggplot(data = Data, aes(x = log(SQRFOOT), y = log(TPY), col = SELFFUNDINS)) +
  geom_point() +
  theme_bw()
ggplot(data = Data, aes(x = log(SQRFOOT), y = log(TPY), col = MCERT)) +
  geom_point() +
  theme_bw()
ggplot(data = Data, aes(x = log(SQRFOOT), y = log(TPY), col = ORGSTR)) +
  geom_point() +
  theme_bw()
ggplot(data = Data, aes(x = log(SQRFOOT), y = log(TPY), col = MSA)) +
  geom_point() +
  theme_bw()
# Provo a fare i due modelli con PRO e TAXEXEMPT
fitcacca1 <- lm(log(TPY) ~ log(SQRFOOT)*PRO, Data)
summary(fitcacca1)
# Il summary è decente
p <- ggplot(data = Data, aes(x = log(SQRFOOT), y = log(TPY), col = PRO)) +
  geom_point(show.legend = F) +
  theme_bw() +
  xlab("Logaritmo dei piedi quadrati") +
  ylab("Logaritmo dei posti occupati all'anno") +
  geom_smooth(data = Data[Data$PRO == 0,], se = F, method = 'lm', formula = 'y ~ x', lwd = 0.75, col = "red")+
  geom_smooth(data = Data[Data$PRO == 1,], se = F, method = 'lm', formula = 'y ~ x', lwd = 0.75, col = "blue") 
# Ci può stare, si può valutare anche solo il modello additivo

resiplot(fitcacca1, p)

#Modello TAXEXEMPT
fitcacca2 <- lm(log(TPY) ~ log(SQRFOOT)*TAXEXEMPT, Data)
summary(fitcacca2)
# Il summary è decente
p <- ggplot(data = Data, aes(x = log(SQRFOOT), y = log(TPY), col = TAXEXEMPT)) +
  geom_point(show.legend = F) +
  theme_bw() +
  xlab("Logaritmo dei piedi quadrati") +
  ylab("Logaritmo dei posti occupati all'anno") +
  geom_smooth(data = Data[Data$TAXEXEMPT == 0,], se = F, method = 'lm', formula = 'y ~ x', lwd = 0.75, col = "red")+
  geom_smooth(data = Data[Data$TAXEXEMPT == 1,], se = F, method = 'lm', formula = 'y ~ x', lwd = 0.75, col = "blue") 
resiplot(fitcacca2, p)


# Provo PRO però solo additivo
fitcacca3 <- lm(log(TPY) ~ log(SQRFOOT) + PRO, Data)
summary(fitcacca3)
# Il summary è decente
p <- ggplot(data = Data, aes(x = log(SQRFOOT), y = log(TPY), col = PRO)) +
  geom_point(show.legend = F) +
  theme_bw() +
  xlab("Logaritmo dei piedi quadrati") +
  ylab("Logaritmo dei posti occupati all'anno") +
  geom_abline(slope = coef(fitcacca3)[2], intercept = coef(fitcacca3)[1], col = "darkgoldenrod1", linewidth = 1) +
  geom_abline(slope = coef(fitcacca3)[2],intercept = coef(fitcacca3)[3] + coef(fitcacca3)[1], col = "deepskyblue1", linewidth = 1)
# Ci può stare, si può valutare anche solo il modello additivo
resiplot(fitcacca3, p)

# Alla fine secondo me il modello migliore è solo NUMBED oppure SQRT(SQRFOOT)*PRO
#(Enrico)
# Io fare le conclusioni e i test su questi 2



## Predizione anno 2002
summary(Data$CRYEAR)
ggplot(data = DataNa, aes(x = log(SQRFOOT), y = log(TPY), col = PRO)) +
  geom_point() +
  facet_wrap(vars(CRYEAR))+
  theme_bw() +
  xlab("Logaritmo dei piedi quadrati") +
  ylab("Logaritmo pazienti annui")


 
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

# Un'idea è quella di farlo runnare diverse volte senza set seed e poi si 
# misura quanti dati ha azzeccato e si fa una media di come performa
set.seed(69)

#SPOILER NON SO FARE STA ROBA

#Divisione data set
# 70% dei dati nel training e 30 nel test
sample <- sample(c(TRUE, FALSE), nrow(DataNa), replace=TRUE, prob=c(0.8,0.2))
train  <- Data[sample, ]
test   <- Data[!sample, ]

#Faccio il fit con il modello che abbiamo selezionato (che in caso si può cambiare)
fit_train <- lm(log(TPY) ~ log(SQRFOOT)*PRO, train)
summary(fit_train)

#Dati predetti
pred <- predict.lm(fit_train, test)

#Questo non so bene cosa rappresenta
ggplot()+
  geom_point(aes(x = test[,'SQRFOOT'], y = pred))

#calcolo residui
res <- test[,'TPY'] - pred
summary(res)
#Non so bene che variabile utilizzare per valutare il fit


# provo a fare training - test set per il modello con NUMBED
set.seed(69)
sample <- sample(c(TRUE, FALSE), nrow(DataNa), replace=TRUE, prob=c(0.9,0.1))
train  <- Data[sample, ]
test   <- Data[!sample, ]
fit_train <- lm(TPY ~ NUMBED, train)
pred <- predict.lm(fit_train, test)
# Scatterplot ma i punti rossi sono i predict
ggplot(Data, aes(x = NUMBED, y = TPY))+
  geom_point(data = train, aes(x = NUMBED, y = TPY)) +
  geom_point(data = test, aes(x = NUMBED, y = pred), col = "red") +
  theme_bw()
# Vediamo la differenza tra i dati predictati e i dati reali
ggplot(data = test, aes(x = TPY, y = pred)) +
  geom_point() +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1)



## CLUSTERING ------------------------------------------------------------------

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


# Provo il clusering con il metodo delle k medie però con 3 nuclei
km.numbed <- kmeans(scale(Data[,c("NUMBED", "TPY")]), centers = 3)
km.sqrfoot <- kmeans(scale(na.omit(Data[,c("SQRFOOT", "TPY")])), centers = 3)
# Numerosità nei due cluster
table(km.numbed$cluster)
table(km.sqrfoot$cluster)

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

# A livello interpretativo può avere senso tenere un modello che tiene conto dei tre cluster
# Potrebbe avere senso per definire ospedali di piccole, medie e grandi dimensioni
