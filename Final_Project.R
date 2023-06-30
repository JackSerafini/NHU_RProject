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

# Creazione dei grafici (orribili)
p1 <- ggplot(data = Data, aes(y = TPY)) +
    geom_boxplot(fill = "yellow") +
    theme_classic() 

p2 <- ggplot(data = Data, aes(y = NUMBED)) +
    geom_boxplot(fill = "yellow") +
    theme_classic() 

p3 <- ggplot(data = na.omit(Data), aes(y = SQRFOOT)) +
    geom_boxplot(fill = "yellow") +
    theme_classic() 

p4 <- ggplot(data = Data, aes(x = CRYEAR, fill = CRYEAR)) +
    geom_bar() +
    theme_classic() +
    theme(legend.position = "none")

p5 <- ggplot(data = Data, aes(x = MSA, fill = MSA)) +
    geom_bar() +
    theme_classic() +
    theme(legend.position = "none")

p6 <- ggplot(data = Data, aes(x = URBAN, fill = URBAN)) +
    geom_bar() +
    theme_classic() +
    theme(legend.position = "none")

p7 <- ggplot(data = Data, aes(x = PRO, fill = PRO)) +
    geom_bar() +
    theme_classic() +
    theme(legend.position = "none")

p8 <- ggplot(data = Data, aes(x = TAXEXEMPT, fill = TAXEXEMPT)) +
    geom_bar() +
    theme_classic() +
    theme(legend.position = "none")

p9 <- ggplot(data = Data, aes(x = SELFFUNDINS, fill = SELFFUNDINS)) +
    geom_bar() +
    theme_classic() +
    theme(legend.position = "none")

p10 <- ggplot(data = Data, aes(x = MCERT, fill = MCERT)) +
    geom_bar() +
    theme_classic() +
    theme(legend.position = "none")

p11 <- ggplot(data = Data, aes(x = ORGSTR, fill = ORGSTR)) +
    geom_bar() +
    theme_classic() +
    theme(legend.position = "none")

plot_grid(p1, p2, p3,
          p4, p5, p6,
          p7, p8, p9,
          p10, p11,
          nrow = 4, ncol = 3)

datatorta <- data.frame(group = Data$MSA, value = length(Data$MSA))

ggplot(datatorta, aes(x="", y=value, fill=group)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    theme_void()

# Prova contemporanea
