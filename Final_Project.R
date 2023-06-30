Data <- read.csv("~/Documents/Uni/Basi di Dati B/data/WiscNursingHome.csv", header = TRUE)
# Probabilmente anche CRYEAR: Data$CRYEAR <- factor(Data$CRYEAR)
Data$URBAN <- factor(Data$URBAN)
Data$PRO <- factor(Data$PRO)
Data$TAXEXEMPT <- factor(Data$TAXEXEMPT)
Data$SELFFUNDINS <- factor(Data$SELFFUNDINS)
Data$MCERT <- factor(Data$MCERT)
Data$ORGSTR <- factor(Data$ORGSTR)

