library(ggridges)
library(pROC)
library(rpart)
library(keras)
library(ggplot2)
library(ade4)
library(caret)
library(plyr)
library(dplyr)
library(VIM)
library(ggcorrplot)
library(lsr)
library(randomForestSRC) 
library(ggRandomForests)
library(gridExtra)
library(ggridges)
library(pROC)
library(rpart)
library(keras)
library(ggplot2)
library(ade4)
library(caret)
library(dplyr)
library(VIM)
library(ggcorrplot)
library(lsr)
library(randomForestSRC) 
library(ggRandomForests)
library(gridExtra)
library(arules)
library(gmodels)
library(ggthemes)
library(plotly)
library(survival)
library(rms)
library(randomForest)
library(tidyr)
library(forcats)
library(dygraphs)
library(xts)
library(latex2exp)
library(MASS)
library(ggmosaic)


# Creazione Variabili ####

#carico i dati
rm(list= ls())
setwd('C:/Users/banab/Desktop/Puz')
#dati <- read.csv2("DATABASE COMPLETO.csv", header=T)
dati <- read.csv("C:/Users/banab/Desktop/Puz/DATABASE COMPLETO.csv")
# nomi <- names(dati)
# nomi[5] <- "Age"
# names(dati) <- nomi
a <- discretize( dati$Age, method = "interval", breaks = 4)
levels(a) <- c("Giovane", "Adulto", "Anziano", "Grandanziano")
dati$Age_disc <- a
dati[, c("Age_disc", "Age")]
#dati[dati$Pat_Emat == "LInfoma", "Pat_Emat" ] = "Linfoma"
#b <- as.character(dati$Pat_Emat)
#dati$Pat_Emat <- as.factor(b)
dati$Pat_Emat %>%  str()
dati$PERIODO_POSTBDG <- as.factor(dati$PERIODO_POSTBDG)
#periodo post, terapia fatta,  genere, ddgalterato, asagalterato, altroasagalterato, colonizzazione, age, ifi, isolamentoM
#patem, alterazioni radiologiche, outcome
dati$ALTERAZIONI.SPECIFICHE.PER.IFI..GENERALE. <- dati$ALTERAZIONI.SPECIFICHE.PER.IFI..GENERALE. %>% as.factor()
dati$TERAPIA_COMPLESSIVA <- dati$TERAPIA_COMPLESSIVA %>% as.factor()
dati$INFEZIONE_PROBABILE_SENZA_BDG <- dati$INFEZIONE_PROBABILE_SENZA_BDG %>% as.factor()
dati$BDG.ALTERNATO <- dati$BDG.ALTERNATO %>% as.factor()
dati$DOPPIO.BDG.DEFINITIVO <- dati$DOPPIO.BDG.DEFINITIVO %>% as.factor()
dati$TERAPIA_ANTFUN <- as.factor(dati$TERAPIA_ANTFUN)
dati$BDG_ALTER <- as.factor(dati$BDG_ALTER)
dati$S_ASAG_OLTRE <- as.factor(dati$S_ASAG_OLTRE)
dati$ALTR_ASAG_OLTRE <- as.factor(dati$ALTR_ASAG_OLTRE)
dati$Colonizzazato <- as.factor(dati$Colonizzazato)
dati$IFI <- as.factor(dati$IFI)
dati$FUNG_ISOL <- as.factor(dati$FUNG_ISOL)
dati$OUTCOME <- as.factor(dati$OUTCOME)
dati$NEOFUO..retrospettiva. <- as.factor(dati$NEOFUO..retrospettiva.)
dati$Profilassiover5 <- as.factor(dati$Profilassiover5)
dati$Classificazione.FUO.IFI.ASPECIFICO <- as.factor(dati$Classificazione.FUO.IFI.ASPECIFICO)
dati$Colonizzazato <- as.factor(dati$Colonizzazato)
dati$Alterazioni_radiologiche_polmonari <- as.factor(dati$Alterazioni_radiologiche_polmonari)
dati$TERAPIA_TRICOMPLESSIVA <- as.factor(dati$TERAPIA_TRICOMPLESSIVA)
dati$DOPPIO_BDG_ALTER <- dati$DOPPIO_BDG_ALTER %>%  as.factor()
dati$BDGCONASAG <- dati$BDGCONASAG %>% as.factor()

dati$OUTCOME %>% str()
trainqual <- dati[, c("PERIODO_POSTBDG", "TERAPIA_ANTFUN", "Gender", "BDG_ALTER", "S_ASAG_OLTRE", "ALTR_ASAG_OLTRE", "Colonizzazato",
                    "Age_disc", "IFI", "FUNG_ISOL", "Pat_Emat", "OUTCOME", "Profilassiover5", 
                    "Classificazione.FUO.IFI.ASPECIFICO", "NEOFUO..retrospettiva.", 'Alterazioni_radiologiche_polmonari' )]
trainqualrid <- dati[, c("ALTERAZIONI.SPECIFICHE.PER.IFI..GENERALE.", "BDG_ALTER", "S_ASAG_OLTRE", "TERAPIA_ANTFUN"   )]
traincramer <-  dati[, c("ALTERAZIONI.SPECIFICHE.PER.IFI..GENERALE.", "BDG_ALTER", "S_ASAG_OLTRE", "TERAPIA_ANTFUN", "ALTR_ASAG_FATTO"   )]

#infezione fungina certa
dati$INFEZIONE.FUNGINA.CERTA[is.na(dati$INFEZIONE.FUNGINA.CERTA)] <- 0
dati$INFEZIONE.FUNGINA.CERTA <- as.factor(dati$INFEZIONE.FUNGINA.CERTA)

#infezione fungina probabile
dati$INFEZIONE.FUNGINA.PROBABILE[is.na(dati$INFEZIONE.FUNGINA.PROBABILE)] <- 0
dati$INFEZIONE.FUNGINA.PROBABILE <- as.factor(dati$INFEZIONE.FUNGINA.PROBABILE)

#anni

table(dati$PERIODO_POSTBDG)/c(25, 12)
a <- as.POSIXct(dati$Data_inizio, format = "%d/%m/%Y" )
dati$serie <- a
#datimod
datimod <- dati
datimod [is.na(datimod$TOT_GIORN), 'TOT_GIORN'] = 0
#datineofuo
datineofuo1 <- subset(dati, dati$NEOFUO..retrospettiva. ==1)
datineofuo1mod <- datimod[datimod$NEOFUO..retrospettiva. == 1, ]

#amfotericina
datambi <- dati[!(is.na(dati$Days_Ambisome)),]
Amfotericina <- ifelse(dati$Abelcet == 1 | dati$Ambisome == 1, 1, 0 )
dati$Amfotericina <- as.factor(Amfotericina)
Albe <-  dati$Days_ABELCET
Ambi <- dati$Days_Ambisome
Albe[is.na(Albe)] <- 0
Ambi[is.na(Ambi)] <- 0
dati$Days_Amfotericina <- Albe + Ambi
dati$Days_Amfotericina[dati$Days_Amfotericina == 0 ] <- NA

#bdg e asper
datibdg <- dati[dati$BDG_FATTO==1,]
datiasp <- dati[dati$S_ASAG_FATTO==1,]
datiasp[is.na(datiasp$S_ASAG_OLTRE), "S_ASAG_OLTRE"] <- 0
datitest <- dati[dati$BDG_FATTO == 1 & dati$S_ASAG_FATTO == 1, ]

#sicuri malati
indice <- dati$OUTCOME == 2 | dati$IFI == 3 | 
  dati$CAMP_ISOL %in% c("AUTOPSIA", "BIOPSIA DI TUMEFAZIONE SOTTOCLAVICOLARE", "LIQUIDO PERITONEALE", "SANGUE", 
                        "SANGUE (E URINE IN DATA 06/08/2018)", "SANGUE (elongisporus) URINA (candida)", 
                        "SANGUE (in precedenza isolamnto il 05/06/2017  dello stesso micete su tampone ferita chirurgica e isolamento di Candida tropicalis su urina il 26/05/2017)",
                        "SANGUE (isolamento anche in data 12/07/2016 di Candida albicans sulle urine)") 
sicurimalati <- ifelse(indice == T, 1, 0)
sicurimalati[sicurimalati %>%  is.na()] <- 0
sicurimalati <- as.factor(sicurimalati)
dati$sicurimalati <- sicurimalati


#terapia empirica
dati$Terapia_empirica_fever.driven[is.na(dati$Terapia_empirica_fever.driven)] <- 0
dati$Terapia_pre.emptive[is.na(dati$Terapia_pre.emptive)] <- 0
dati$Terapiaempirica <- as.factor(dati$Terapia_empirica_fever.driven + dati$Terapia_pre.emptive)

#terapia tricomplessiva
datiemp <- dati[dati$TERAPIA_TRICOMPLESSIVA == 1,] 

#neoperiodo
dati$neoperiodo <- ifelse(dati$PERIODO_POSTBDG == 0, 'Ante', 'Post')

#serire storica
tab <- table(cut(a, 'month'))
b <- data.frame(Mese =format(as.Date(names(tab)), '%m/%Y'),
                Numero =as.vector(tab))

#neooutcome
dati$neooutcome <- ifelse(dati$OUTCOME == 2, 1, 0)

#IFI
datisiifi <- dati[dati$IFI!=0, ]
datiifi0 <- dati[dati$IFI==0, ]
datiifi1 <- dati[dati$IFI==1, ]
datiifi2 <- dati[dati$IFI==2, ]
datiifi3 <- dati[dati$IFI==3, ]

#Bdg per diagnosi
ciao <- dati$BDG_per_diagnosi
ciao[is.na(ciao)] <- 5
datibdgD <- dati[ciao==1,]

#terapia complessiva
datiter1 <- dati[dati$TERAPIA_COMPLESSIVA==1,]
datiter2 <- dati[dati$TERAPIA_COMPLESSIVA==2,]
datiter3 <- dati[dati$TERAPIA_COMPLESSIVA==3,]

#boxplot
Giorni <- c(dati$Days_Amfotericina[!is.na(dati$Days_Amfotericina)], dati$Days_Cancidas[!is.na(dati$Days_Cancidas)], 
            dati$Days_Fluconazolo[!is.na(dati$Days_Fluconazolo)], dati$Days_Isavuconazolo[!is.na(dati$Days_Isavuconazolo)],
            dati$Days_Voriconazolo[!is.na(dati$Days_Voriconazolo)])
Medicinale <- c( rep ('Amfotericina', dati$Days_Amfotericina[!is.na(dati$Days_Amfotericina)] %>% length() ),
                 rep('Cancidas', dati$Days_Cancidas[!is.na(dati$Days_Cancidas)] %>%  length()),
                 rep('Fluconazolo', dati$Days_Fluconazolo[!is.na(dati$Days_Fluconazolo)] %>% length()),
                 rep('Isavuconazolo', dati$Days_Isavuconazolo[!is.na(dati$Days_Isavuconazolo)] %>% length()),
                 rep('Voriconazolo', dati$Days_Voriconazolo[!is.na(dati$Days_Voriconazolo)] %>% length()))
PeriodoBDG <- c(dati$PERIODO_POSTBDG[!is.na(dati$Days_Amfotericina)] %>% as.character(), dati$PERIODO_POSTBDG[!is.na(dati$Days_Cancidas)] %>% as.character(), 
                dati$PERIODO_POSTBDG[!is.na(dati$Days_Fluconazolo)] %>% as.character(), dati$PERIODO_POSTBDG[!is.na(dati$Days_Isavuconazolo)] %>% as.character(),
                dati$PERIODO_POSTBDG[!is.na(dati$Days_Voriconazolo)] %>% as.character())

graficobox <- data.frame(Giorni, Medicinale, PeriodoBDG)

#dati morti
datimorti <- dati %>% 
  filter(diverso_paziente_mortalità_totale==1)

#associzione
trainqualridpost <-  trainqualrid[dati$PERIODO_POSTBDG == '1', ]
trainqualridpre <-  trainqualrid[dati$PERIODO_POSTBDG == '0', ]
trainqualridpre <- trainqualridpre[, names(trainqualridpre) != "BDG_ALTER" ]
traincramerpost <-  traincramer[dati$PERIODO_POSTBDG == '1', ]
traincramerpre <-  traincramer[dati$PERIODO_POSTBDG == '0', ]
traincramerpre <- traincramerpre[, names(traincramerpre) != "BDG_ALTER" ]

#datti doppio bdg fatto
datidopbdg <- dati[dati$DOPPIO_BDG_FATTO == 1,]


#Cramer####

#associazione tante cose
cramercorr=matrix(ncol = ncol(trainqual), nrow=ncol(trainqual))
nomifact <- names(trainqual)
colnames(cramercorr) <- rownames(cramercorr) <- nomifact
for(i in 1:ncol(cramercorr))
{for(j in 1:ncol(cramercorr))
{cramercorr[i,j]=cramersV(table(trainqual[,i],trainqual[,j]))}
}
cramercorr[is.na(cramercorr)] = 0
cramercorr
ggcorrplot(cramercorr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="square", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Associazione tra variabili", 
           ggtheme=theme_bw
)+
  scale_fill_gradientn(colours=c( "yellow", "red"),na.value = "transparent",
                       breaks=c(0,0.5,1),
                       limits=c(0,1))


#Associazione poche cose 
cramercorr=matrix(ncol = ncol(trainqualrid), nrow=ncol(trainqualrid))
nomifact <- names(trainqualrid)
colnames(cramercorr) <- rownames(cramercorr) <- nomifact
for(i in 1:ncol(cramercorr))
{for(j in 1:ncol(cramercorr))
{cramercorr[i,j]=cramersV(table(trainqualrid[,i],trainqualrid[,j]))}
}
cramercorr[is.na(cramercorr)] = 0
nuovinomi <- c('Alterazioni Cliniche \n indicative per IFI', 'BDG Positivo', 'S-Asag Positivo', 'Terapia Antifungina')
colnames(cramercorr) <- rownames(cramercorr) <- nuovinomi
cramercorr
ggcorrplot(cramercorr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="square", 
           ggtheme=theme_bw
)+
  scale_fill_gradientn(colours=c( "yellow", "red"),na.value = "transparent",
                       breaks=c(0,0.5,1),
                       limits=c(0,1)) +
  labs(title="Associzione base",
       subtitle='Sottotitolo',
       fill = 'Intensità') 



#cramer con asag subal

cramercorr=matrix(ncol = ncol(traincramer), nrow=ncol(traincramer))
nomifact <- names(traincramer)
colnames(cramercorr) <- rownames(cramercorr) <- nomifact
for(i in 1:ncol(cramercorr))
{for(j in 1:ncol(cramercorr))
{cramercorr[i,j]=cramersV(table(traincramer[,i],traincramer[,j]))}
}
cramercorr[is.na(cramercorr)] = 0
nuovinomi <- c('Alterazioni Cliniche \n indicative per IFI', 'BDG Positivo', 'S-Asag Positivo', 'Terapia Antifungina', 'Asag bal')
colnames(cramercorr) <- rownames(cramercorr) <- nuovinomi
cramercorr
ggcorrplot(cramercorr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="square", 
           ggtheme=theme_bw
)+
  scale_fill_gradientn(colours=c( "yellow", "red"),na.value = "transparent",
                       breaks=c(0,0.5,1),
                       limits=c(0,1)) +
  labs(title="Associzione base",
       subtitle='Sottotitolo',
       fill = 'Intensità') 


#Associazione poche cose prima


cramercorr=matrix(ncol = ncol(trainqualridpre), nrow=ncol(trainqualridpre))
nomifact <- names(trainqualridpre)
colnames(cramercorr) <- rownames(cramercorr) <- nomifact
for(i in 1:ncol(cramercorr))
{for(j in 1:ncol(cramercorr))
{cramercorr[i,j]=cramersV(table(trainqualridpre[,i],trainqualridpre[,j]))}
}
cramercorr[is.na(cramercorr)] = 0
nuovinomi <- c('Alterazioni Cliniche \n indicative per IFI', 'S-Asag Positivo', 'Terapia Antifungina')
colnames(cramercorr) <- rownames(cramercorr) <- nuovinomi
cramercorr
ggcorrplot(cramercorr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="square", 
           ggtheme=theme_bw
)+
  scale_fill_gradientn(colours=c( "yellow", "red"),na.value = "transparent",
                       breaks=c(0,0.5,1),
                       limits=c(0,1)) +
  labs(title="Associzione prima",
       subtitle='Sottotitolo',
       fill = 'Intensità') 

#cramer con asag subal prima

cramercorr=matrix(ncol = ncol(traincramerpre), nrow=ncol(traincramerpre))
nomifact <- names(traincramerpre)
colnames(cramercorr) <- rownames(cramercorr) <- nomifact
for(i in 1:ncol(cramercorr))
{for(j in 1:ncol(cramercorr))
{cramercorr[i,j]=cramersV(table(traincramerpre[,i],traincramerpre[,j]))}
}
cramercorr[is.na(cramercorr)] = 0
nuovinomi <- c('Alterazioni Cliniche \n indicative per IFI', 'S-Asag Positivo', 'Terapia Antifungina', 'Asag bal')
colnames(cramercorr) <- rownames(cramercorr) <- nuovinomi
cramercorr
ggcorrplot(cramercorr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="square", 
           ggtheme=theme_bw
)+
  scale_fill_gradientn(colours=c( "yellow", "red"),na.value = "transparent",
                       breaks=c(0,0.5,1),
                       limits=c(0,1)) +
  labs(title="Associzione base",
       subtitle='Sottotitolo',
       fill = 'Intensità') 




#Associazione poche cose dopo


cramercorr=matrix(ncol = ncol(trainqualridpost), nrow=ncol(trainqualridpost))
nomifact <- names(trainqualridpost)
colnames(cramercorr) <- rownames(cramercorr) <- nomifact
for(i in 1:ncol(cramercorr))
{for(j in 1:ncol(cramercorr))
{cramercorr[i,j]=cramersV(table(trainqualridpost[,i],trainqualridpost[,j]))}
}
cramercorr[is.na(cramercorr)] = 0
nuovinomi <- c('Alterazioni Cliniche \n indicative per IFI', 'BDG Positivo', 'S-Asag Positivo', 'Terapia Antifungina')
colnames(cramercorr) <- rownames(cramercorr) <- nuovinomi
cramercorr
ggcorrplot(cramercorr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="square", 
           ggtheme=theme_bw
)+
  scale_fill_gradientn(colours=c( "yellow", "red"),na.value = "transparent",
                       breaks=c(0,0.5,1),
                       limits=c(0,1)) +
  labs(title="Associzione Post",
       subtitle='Sottotitolo',
       fill = 'Intensità') 
 

#cramer con asag subal dopo

cramercorr=matrix(ncol = ncol(traincramerpost), nrow=ncol(traincramerpost))
nomifact <- names(traincramerpost)
colnames(cramercorr) <- rownames(cramercorr) <- nomifact
for(i in 1:ncol(cramercorr))
{for(j in 1:ncol(cramercorr))
{cramercorr[i,j]=cramersV(table(traincramerpost[,i],traincramerpost[,j]))}
}
cramercorr[is.na(cramercorr)] = 0
nuovinomi <- c('Alterazioni Cliniche \n indicative per IFI', 'BDG Positivo',  'S-Asag Positivo', 'Terapia Antifungina', 'Asag bal')
colnames(cramercorr) <- rownames(cramercorr) <- nuovinomi
cramercorr
ggcorrplot(cramercorr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="square", 
           ggtheme=theme_bw
)+
  scale_fill_gradientn(colours=c( "yellow", "red"),na.value = "transparent",
                       breaks=c(0,0.5,1),
                       limits=c(0,1)) +
  labs(title="Associzione base",
       subtitle='Sottotitolo',
       fill = 'Intensità') 






#ANALISI GENERALE CAMPIONE E CONFERMA DEI FATTORI DI RISCHIO NEL CAMPIONE (NON CORRELATO AL PERIODO)####

#istogrammi giorni terapia

cbind(table(dati$Pat_Emat), round(table(dati$Pat_Emat)/472 *100, 2))

table(dati$BDG_ALTER, useNA = "always")

cbind(table(dati$BDG_ALTER, useNA = "always"), round(table(dati$BDG_ALTER, useNA = "always")/472 *100, 2))

hist(dati$TOT_GIORN, col = "red", probability =   T, density=20, breaks=20,)
mu <- sd(dati$TOT_GIORN, na.rm = T)
sigma <- mean(dati$TOT_GIORN, na.rm = T)
curve(dnorm(x, mean=mu, sd=sigma), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

totgio <- dati$TOT_GIORN
totgio[is.na(totgio)] <- 0

hist(totgio, col = "red", probability =   T, density=20, breaks=20,)
mu <- sd(totgio, na.rm = T)
sigma <- mean(totgio, na.rm = T)
curve(dnorm(x, mean=mu, sd=sigma), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

shap <- shapiro.test(dati$TOT_GIORN)
shap

dati %>% 
  ggplot( aes(x = TOT_GIORN)) +
  geom_histogram(bins = 50, fill = 'green', color = 'white'  ) +
  theme_economist() +
  labs(title="Titolo",
       subtitle=paste("Il p-value dello Shapiro test: " , round(shap$p.value,14)),
       y="Numero", 
       x="",
       caption = "Dataset Niguarda") + 
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))

shapiro.test(dati$TOT_GIORN)


dati %>%
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = TOT_GIORN, stat(density),  color = PERIODO_POSTBDG)) +
  geom_freqpoly(alpha= 0.8) +
  labs(title="Titolo",
       #subtitle=paste("Il p-value del t-test è: " , round(testt$p.value,4)), #wilcoU$p.value
       y="Densita", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Terapia antifungina') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))

dati %>% 
  ggplot( aes(x = TOT_GIORN)) +
    geom_histogram(data=dati %>% filter(PERIODO_POSTBDG == '0'), aes(y = ..density.. , fill = ..density..), col = 'green' , bins = 50) +
    geom_label( aes(x=50, y=0.03, label="Prima"), color="orange") +
    geom_histogram(data=dati %>% filter(PERIODO_POSTBDG == '1'), aes(y = -..density.., fill = -..density..),col = 'red', bins = 50) +
  geom_label( aes(x=50, y=-0.03, label="Dopo"), color="blue") +
    scale_fill_gradient("Density", low="blue", high="orange") +
  labs(title="Titolo",
       #subtitle=paste("Il p-value del t-test è: " , round(testt$p.value,4)), #wilcoU$p.value
       y="Densita", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Terapia antifungina') +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10),
    legend.position = "none")

dati %>% 
  ggplot( aes(x = TOT_GIORN)) +
  geom_histogram(data=dati %>% filter(PERIODO_POSTBDG == '0'), aes(y = ..density.. ), fill = 'orange', col = 'blue' , bins = 50) +
  geom_label( aes(x=50, y=0.03, label="Prima"), color="orange") +
  geom_histogram(data=dati %>% filter(PERIODO_POSTBDG == '1'), aes(y = -..density..),fill = 'red', col = 'green', bins = 50) +
  geom_label( aes(x=50, y=-0.03, label="Dopo"), color="blue") +
  #scale_fill_gradient("Density", low="blue", high="orange") +
  labs(title="Titolo",
       #subtitle=paste("Il p-value del t-test è: " , round(testt$p.value,4)), #wilcoU$p.value
       y="Densita", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Terapia antifungina') +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10),
    legend.position = "none")


#colonizzato e terapia

Colter <- table(dati$TERAPIA_ANTFUN, dati$Colonizzazato, dnn = c("Terapia", "Colonizzato"))
Colter
chiquadro <- chisq.test(Colter) # sono dipendenti
chiquadro
fisher.test(Colter)



dati %>%
  mutate(Colonizzazione = fct_relevel(Colonizzazato, '1', '0')) %>%  #ordine
  mutate(Colonizzazione =  revalue(Colonizzazione, c("0"="Non colonizzato", "1"="Colonizzato")) ) %>% #muovi nomi
  mutate(TERAPIA_ANTFUN =  revalue(TERAPIA_ANTFUN, c("0"="No Terapia", "1"="Terapia")) ) %>%
  ggplot( aes(x = TERAPIA_ANTFUN, fill = Colonizzazione)) +
    geom_bar(alpha = 0.8, position = "fill") +
    coord_flip() +
    labs(title="Titolo",
         subtitle=paste("Il p-value del test del Chiquadro è: " , round(chiquadro$p.value,7)),
         y="Frequenza (%)", 
         x="",
         caption = "Dataset Niguarda") +
  scale_fill_manual(values=c("#CCFFE5", "#E69F00")) +
    theme(
      plot.subtitle = element_text(size = 10),
      plot.caption = element_text( face = "italic")) #+
  #guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE))



dati %>%
  mutate(Colonizzazione = fct_relevel(Colonizzazato, '1', '0')) %>%  #ordine colore
  mutate(Colonizzazione =  revalue(Colonizzazione, c("0"="Non colonizzato", "1"="Colonizzato"))  ) %>% #muovi nomi colore
  mutate(TERAPIA_ANTFUN =  revalue(TERAPIA_ANTFUN, c("0"="No Terapia", "1"="Terapia")) ) %>% #asse y
  ggplot( aes(x = TERAPIA_ANTFUN, fill = Colonizzazione)) +
  geom_bar( position = "fill") +
  coord_flip() +
  labs(title="Relazione Terapia e colonizzazione",
       subtitle=paste("Il p-value del test del Chiquadro è: " , round(chiquadro$p.value,8)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Terapia antifungina: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10)) +
  guides(fill = guide_legend(reverse = TRUE))


dati %>%
  mutate(Colonizzazione = fct_relevel(Colonizzazato, '1', '0')) %>%  #ordine colore
  mutate(Colonizzazione =  revalue(Colonizzazione, c("0"="Non colonizzato", "1"="Colonizzato"))  ) %>% #muovi nomi colore
  mutate(TERAPIA_ANTFUN =  revalue(TERAPIA_ANTFUN, c("0"="No Terapia", "1"="Terapia")) ) %>% #asse y
  ggplot( aes(x = TERAPIA_ANTFUN, fill = Colonizzazione)) +
  geom_mosaic( aes(product(Colonizzazione, TERAPIA_ANTFUN), fill= Colonizzazione)  ) +
  coord_flip() +
  labs(title="Relazione Terapia e colonizzazione",
       subtitle=paste("Il p-value del test del Chiquadro è: " , round(chiquadro$p.value,8)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Terapia antifungina: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10)) +
  guides(fill = guide_legend(reverse = TRUE))


#colonizzato infezione fungina probabile
tbl <- table(dati$Colonizzazato, dati$INFEZIONE.FUNGINA.PROBABILE) 
tbl
chiquadro <- chisq.test(tbl)
chiquadro
ggplot(dati, aes(x = INFEZIONE.FUNGINA.PROBABILE, fill = Colonizzazato, color = Colonizzazato)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip()


dati %>%
  mutate(Colonizzazione = fct_relevel(Colonizzazato, '1', '0')) %>%  #ordine colore
  mutate(Colonizzazione =  revalue(Colonizzazione, c("0"="Non colonizzato", "1"="Colonizzato"))  ) %>% #muovi nomi colore
  mutate(INFEZIONE.FUNGINA.PROBABILE =  revalue(INFEZIONE.FUNGINA.PROBABILE, c("0"="No IFI", "1"="Infezione fungina probabile")) ) %>% #asse y
  ggplot( aes(x = INFEZIONE.FUNGINA.PROBABILE, fill = Colonizzazione)) +
  geom_bar( position = "fill") +
  coord_flip() +
  labs(title="Relazione Infezione fungina e Colonizzazione",
       subtitle=paste("Il p-value del test del Chiquadro è: " , round(chiquadro$p.value,5)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Terapia antifungina: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10)) +
  guides(fill = guide_legend(reverse = TRUE))


dati %>%
  #mutate(INFEZIONE.FUNGINA.PROBABILE = fct_relevel(INFEZIONE.FUNGINA.PROBABILE, '1', '0')) %>%  #ordine colore
  mutate(Colonizzazione =  revalue(Colonizzazato, c("0"="Non colonizzato", "1"="Colonizzato"))  ) %>% #muovi nomi colore
  mutate(INFEZIONE.FUNGINA.PROBABILE =  revalue(INFEZIONE.FUNGINA.PROBABILE, c("0"="No IFI", "1"="Infezione fungina probabile")) ) %>% #asse y
  ggplot( aes(x = Colonizzazione, fill = INFEZIONE.FUNGINA.PROBABILE)) +
  geom_bar( position = "fill") +
  labs(title="Relazione Infezione fungina e Colonizzazione",
       subtitle=paste("Il p-value del test del Chiquadro è: " , round(chiquadro$p.value,5)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Terapia antifungina: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10)) +
  guides(fill = guide_legend(reverse = TRUE))


  
#profilassi over 5 e terapia

Colter <- table(dati$TERAPIA_ANTFUN, dati$Profilassiover5, dnn = c("Terapia", "Colonizzato"))
Colter
chiquadro <- chisq.test(Colter) # sono dipendenti
chiquadro
fisher.test(Colter)

dati %>%
  mutate(Profilassiover5 = fct_relevel(Profilassiover5, '1', '0')) %>%  #ordine colore
  mutate(Profilassiover5 =  revalue(Profilassiover5, c("0"="Non in profilassi", "1"="In profilassi"))  ) %>% #muovi nomi colore
  mutate(TERAPIA_ANTFUN =  revalue(TERAPIA_ANTFUN, c("0"="No Terapia", "1"="Terapia")) ) %>% #asse y
  ggplot( aes(x = TERAPIA_ANTFUN, fill = Profilassiover5)) +
  geom_bar( position = "fill") +
  coord_flip() +
  labs(title="Relazione Terapia e Profilassi",
       subtitle=paste("Il p-value del test del Chiquadro è: " , round(chiquadro$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Profilassi:  ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10)) +
  guides(fill = guide_legend(reverse = TRUE))

#profilassi over 5 infezione fungina probabile

tbl <- table(dati$Profilassiover5, dati$INFEZIONE.FUNGINA.PROBABILE) 
tbl
chiquadro <- chisq.test(tbl)
chiquadro
fisher.test(tbl)


dati %>%
  mutate(Profilassiover5 = fct_relevel(Profilassiover5, '1', '0')) %>%  #ordine colore
  mutate(Profilassiover5 =  revalue(Profilassiover5, c("0"="Non in profilassi", "1"="Profilassi"))  ) %>% #muovi nomi colore
  mutate(INFEZIONE.FUNGINA.PROBABILE =  revalue(INFEZIONE.FUNGINA.PROBABILE, c("0"="No IFI", "1"="Infezione fungina probabile")) ) %>% #asse y
  ggplot( aes(x = INFEZIONE.FUNGINA.PROBABILE, fill = Profilassiover5)) +
  geom_bar( position = "fill") +
  coord_flip() +
  labs(title="Relazione Infezione fungina probabile e Profilassi",
       subtitle=paste("Il p-value del test del Chiquadro è: " , round(chiquadro$p.value,5)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Profilassi:  ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10)) +
  guides(fill = guide_legend(reverse = TRUE))





 

#alterazioni polmonari terapia
tbl <- table(dati$TERAPIA_ANTFUN, dati$Alterazioni_radiologiche_polmonari) 
tbl
chiquadro <- chisq.test(tbl)
chiquadro
ggplot(dati, aes(x = Alterazioni_radiologiche_polmonari, fill = TERAPIA_ANTFUN, color = TERAPIA_ANTFUN)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip()

dati %>%
  mutate(Terapia_antifungina = fct_relevel(TERAPIA_ANTFUN, '1', '0')) %>%  #ordine colore
  mutate(Terapia_antifungina =  revalue(Terapia_antifungina, c("0"="Non somministrata", "1"="Somministrata")) ) %>% #muovi nomi colore
  mutate(Alterazioni_radiologiche_polmonari =  revalue(Alterazioni_radiologiche_polmonari, c("0"="No alterazioni radiologiche", "1"="Alterazioni aspecifiche", '2' =  'Alterazioni indicative \n di patolofia fungina')) ) %>% #asse y
  ggplot( aes(x = Alterazioni_radiologiche_polmonari, fill = Terapia_antifungina)) +
  geom_bar( position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Alterazioni radiologiche polmonari e Terapia antifungina",
       subtitle=paste("Il p-value del test del Chiquadro è: " , round(chiquadro$p.value,25)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Terapia antifungina: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
     panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))+
  guides(fill = guide_legend(reverse = TRUE))
    #plot.background = element_rect(fill = '#EDFEEA')
    
    
    
    
    # panel.grid.major.y = element_blank(),
    # panel.grid.minor.y = element_blank(),
    # panel.grid.major.x = element_line('white', size = 0.5),
    # panel.grid.minor.x = element_line('white', size = 0.3)) 
 
  





#POTERE DIAGNOSTICO DI BDG E ASPERGILLO####




#antifun e bdg alter condizionato al fatto che bdg è stato fatto

terbdg <- table(datibdg$TERAPIA_ANTFUN, datibdg$BDG_ALTER, dnn = c("Terapia", "Alter"))
chisq.test(terbdg)
fisher <- fisher.test(terbdg)
fisher
ggplot(datibdg, aes(x = BDG_ALTER, fill = TERAPIA_ANTFUN, color = TERAPIA_ANTFUN)) +
  geom_bar(alpha = 0.8, position = 'fill') +
  coord_flip()


dati %>%
  mutate(Terapia_antifungina = fct_relevel(TERAPIA_ANTFUN, '1', '0')) %>%  #ordine colore
  mutate(Terapia_antifungina =  revalue(Terapia_antifungina, c("0"="Non somministrata", "1"="Somministrata")) ) %>% #muovi nomi colore
  mutate(BDG_ALTER =  revalue(BDG_ALTER, c("0"="Bdg non alterato", "1"="Bdg Alterato")) ) %>% #asse y
  drop_na(BDG_ALTER) %>%
  ggplot( aes(x = BDG_ALTER, fill = Terapia_antifungina)) +
  geom_bar( position = "fill") +
  coord_flip() +
  labs(title= paste('Relazione tra BDG e Terapia antifungina'),
       subtitle=paste("Il p-value del test esatto di Fisher è: " , round(fisher$p.value,7)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Terapia antifungina: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))+
  guides(fill = guide_legend(reverse = TRUE))


#antifun e aspergillo dato aspergillo fatto




terasag <- table(datiasp$TERAPIA_ANTFUN, datiasp$S_ASAG_OLTRE, dnn = c("Terapia", "Oltre"))
terasag
chiquadro <- chisq.test(terasag)
chiquadro
ggplot(datiasp, aes(x = S_ASAG_OLTRE, fill = TERAPIA_ANTFUN, color = TERAPIA_ANTFUN)) +
  geom_bar(alpha = 0.8, position = 'fill') +
  coord_flip()

dati %>%
  mutate(Terapia_antifungina = fct_relevel(TERAPIA_ANTFUN, '1', '0')) %>%  #ordine colore
  mutate(Terapia_antifungina =  revalue(Terapia_antifungina, c("0"="Non somministrata", "1"="Somministrata")) ) %>% #muovi nomi colore
  mutate(S_ASAG_OLTRE =  revalue(S_ASAG_OLTRE, c("0"="Aspergillo non oltre", "1"="Aspergillo oltre")) ) %>% #asse y
  drop_na(S_ASAG_OLTRE) %>% 
  ggplot( aes(x = S_ASAG_OLTRE, fill = Terapia_antifungina)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Apergillo e Terapia antifungina",
       subtitle=paste("Il p-value del test del chiquadro è: " , round(chiquadro$p.value,9)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Terapia antifungina: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))



#vedere se aspergillo e bdg danno lo stesso esito

datitest <- dati[dati$BDG_FATTO == 1 & dati$S_ASAG_FATTO == 1, ]
bdgasag <- table(datitest$BDG_ALTER, datitest$S_ASAG_OLTRE, dnn = c("Bdg", "Asag"))
bdgasag
chiquadro <- chisq.test(bdgasag)
chiquadro
fisher <- fisher.test(bdgasag) 
fisher

ggplot(datitest, aes(x = S_ASAG_OLTRE, fill = BDG_ALTER, color = BDG_ALTER)) +
  geom_bar(alpha = 0.8, position = 'fill') +
  coord_flip()

datitest %>%
  mutate(BDG_ALTER = fct_relevel(BDG_ALTER, '1', '0')) %>%  #ordine colore
  mutate(BDG_ALTER =  revalue(BDG_ALTER, c("0"="Bdg non alterato", "1"="Bdg Alterato")) ) %>% #muovi nomi colore
  mutate(S_ASAG_OLTRE =  revalue(S_ASAG_OLTRE, c("0"="Aspergillo non oltre", "1"="Aspergillo oltre")) ) %>% #asse y
  #drop_na(BDG_ALTER) %>%
  ggplot( aes(x = S_ASAG_OLTRE, fill = BDG_ALTER)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Beta di glucano e Aspergillo",
       subtitle=paste("Il p-value del test esatto di Fisher è: " , round(fisher$p.value,4)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))


ggplot(datitest, aes(x = S_ASAG_OLTRE, fill = BDG_ALTER, color = BDG_ALTER)) +
  geom_bar(alpha = 0.8) 

datitest %>%
  mutate(BDG_ALTER = fct_relevel(BDG_ALTER, '1', '0')) %>%  #ordine colore
  mutate(BDG_ALTER =  revalue(BDG_ALTER, c("0"="Bdg non alterato", "1"="Bdg Alterato")) ) %>% #muovi nomi colore
  mutate(S_ASAG_OLTRE =  revalue(S_ASAG_OLTRE, c("0"="Aspergillo non oltre", "1"="Aspergillo oltre")) ) %>% #asse y
  #drop_na(BDG_ALTER) %>%
  ggplot( aes(x = S_ASAG_OLTRE, fill = BDG_ALTER)) +
  geom_bar(alpha = 0.8) +
  labs(title="Relazione tra Beta di glucano e Aspergillo",
       subtitle=paste("Il p-value del test esatto di Fisher è: " , round(fisher$p.value,4)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))



#sicuri malati con bdg fatto e positivo

table(datibdgD$BDG_ALTER, datibdgD$sicurimalati,  dnn = c('Bdg alterato', 'Malati'))
mat <- confusionMatrix(datibdgD$BDG_ALTER, datibdgD$sicurimalati, positive="1")
#Confusion matrix
mat$table
mat$byClass
mat$overall
#sensitività TP / TP + FN 
#specificità TN / T + FP


#sicuri malati con aspergillo fatto e positivo

table(dati$S_ASAG_OLTRE, dati$sicurimalati,  dnn = c('Asag', 'Malati'))
mat <- confusionMatrix(dati$S_ASAG_OLTRE, dati$sicurimalati, positive="1")
#Confusion matrix
mat$table
mat$byClass
mat$overall


#infezione fungina certa con bdg fatto e positivo

table(datibdgD$BDG_ALTER, datibdgD$INFEZIONE.FUNGINA.CERTA,  dnn = c('Bdg alterato', 'Malati'))
mat <- confusionMatrix(datibdgD$BDG_ALTER, datibdgD$INFEZIONE.FUNGINA.CERTA, positive="1")
#Confusion matrix
mat$table
mat$byClass
mat$overall
#sensitività TP / TP + FN 
#specificità TN / T + FP



#infezione fungina certa con aspergillo fatto e positivo

table(dati$S_ASAG_OLTRE, dati$INFEZIONE.FUNGINA.CERTA,  dnn = c('Asag', 'Malati'))
mat <- confusionMatrix(dati$S_ASAG_OLTRE, dati$INFEZIONE.FUNGINA.CERTA, positive="1")
#Confusion matrix
mat$table
mat$byClass
mat$overall


#infezione fungina probabile con bdg fatto e positivo

table(datibdgD$BDG_ALTER, datibdgD$INFEZIONE.FUNGINA.PROBABILE,  dnn = c('Bdg alterato', 'Malati'))
mat <- confusionMatrix(datibdgD$BDG_ALTER, datibdgD$INFEZIONE.FUNGINA.PROBABILE, positive="1")
#Confusion matrix
mat$table
mat$byClass
mat$overall
#sensitività TP / TP + FN 
#specificità TN / T + FP


#infezione fungina probabile con bdg fatto e positivo

table(datibdgD$BDG_ALTER, datibdgD$INFEZIONE.FUNGINA.PROBABILE,  dnn = c('Bdg alterato', 'Malati'))
mat <- confusionMatrix(datibdgD$BDG_ALTER, datibdgD$INFEZIONE.FUNGINA.PROBABILE, positive="1")
#Confusion matrix
mat$table
mat$byClass
mat$overall
#sensitività TP / TP + FN 
#specificità TN / T + FP




#infezione fungina probabile con aspergillo fatto e positivo

table(dati$S_ASAG_OLTRE, dati$INFEZIONE.FUNGINA.PROBABILE,  dnn = c('Asag', 'Probabili Malati'))
mat <- confusionMatrix(dati$S_ASAG_OLTRE, dati$INFEZIONE.FUNGINA.PROBABILE, positive="1")
#Confusion matrix
mat$table
mat$byClass
mat$overall


#infezione fungina probabile con doppio bdg fatto e positivo

table(datidopbdg$DOPPIO_BDG_ALTER, datidopbdg$INFEZIONE.FUNGINA.PROBABILE,  dnn = c('doppiobdg', 'Probabili Malati'))
mat <- confusionMatrix(datidopbdg$DOPPIO_BDG_ALTER, datidopbdg$INFEZIONE.FUNGINA.PROBABILE, positive="1")
#Confusion matrix
mat$table
mat$byClass
mat$overall



#inezione fungina certa con aspergillo su BAL
table(dati$ALTR_ASAG_OLTRE, dati$INFEZIONE.FUNGINA.CERTA,  dnn = c('Asag BAL', 'Malati'))
mat <- confusionMatrix(dati$ALTR_ASAG_OLTRE, dati$INFEZIONE.FUNGINA.CERTA, positive="1")
#Confusion matrix
mat$table
mat$byClass
mat$overall

#inezione fungina Probabile con aspergillo su BAL
table(dati$ALTR_ASAG_OLTRE, dati$INFEZIONE.FUNGINA.PROBABILE,  dnn = c('Asag BAL', 'Probabili Malati'))
mat <- confusionMatrix(dati$ALTR_ASAG_OLTRE, dati$INFEZIONE.FUNGINA.PROBABILE, positive="1")
#Confusion matrix
mat$table
mat$byClass
mat$overall

#infezione fungina probabile con sia bdg che aspergillo
table(dati$BDGCONASAG, dati$INFEZIONE.FUNGINA.PROBABILE,  dnn = c('BDG e ASAG', 'Probabili Malati'))
mat <- confusionMatrix(dati$BDGCONASAG, dati$INFEZIONE.FUNGINA.PROBABILE, positive="1")
#Confusion matrix
mat$table
mat$byClass
mat$overall


#infezione fungina probabile con doppio bdg definitivo 
table(dati$DOPPIO.BDG.DEFINITIVO, dati$INFEZIONE.FUNGINA.PROBABILE,  dnn = c('BDG e ASAG', 'Probabili Malati'))
mat <- confusionMatrix(dati$DOPPIO.BDG.DEFINITIVO, dati$INFEZIONE.FUNGINA.PROBABILE, positive="1")
#Confusion matrix
mat$table
mat$byClass
mat$overall

#infezione fungina probabile con bdg alternato 
table(dati$BDG.ALTERNATO, dati$INFEZIONE.FUNGINA.PROBABILE,  dnn = c('BDG e ASAG', 'Probabili Malati'))
mat <- confusionMatrix(dati$BDG.ALTERNATO, dati$INFEZIONE.FUNGINA.PROBABILE, positive="1")
#Confusion matrix
mat$table
mat$byClass
mat$overall

#infezione fungina probabile senza bdg con doppio bdg definitivo 
table(dati$DOPPIO.BDG.DEFINITIVO, dati$INFEZIONE_PROBABILE_SENZA_BDG,  dnn = c('BDG e ASAG', 'Probabili Malati'))
mat <- confusionMatrix(dati$DOPPIO.BDG.DEFINITIVO, dati$INFEZIONE_PROBABILE_SENZA_BDG, positive="1")
#Confusion matrix
mat$table
mat$byClass
mat$overall

#infezione fungina probabile senza bdg con bdg alternato
table(dati$BDG.ALTERNATO, dati$INFEZIONE_PROBABILE_SENZA_BDG,  dnn = c('BDG e ASAG', 'Probabili Malati'))
mat <- confusionMatrix(dati$BDG.ALTERNATO, dati$INFEZIONE_PROBABILE_SENZA_BDG, positive="1")
#Confusion matrix
mat$table
mat$byClass
mat$overall

#infezione fungina probabile senza bdg con bdg alter
table(datibdgD$BDG_ALTER, datibdgD$INFEZIONE_PROBABILE_SENZA_BDG,  dnn = c('BDG e ASAG', 'Probabili Malati'))
mat <- confusionMatrix(datibdgD$BDG_ALTER, datibdgD$INFEZIONE_PROBABILE_SENZA_BDG, positive="1")
#Confusion matrix
mat$table
mat$byClass
mat$overall

#distribuzione bdgnax


kruskal.test( TOT_GIORN ~ IFI, data = dati)


ggplot(dati[dati$POPOLAZIONE_PAZIENTI == 1, ], aes(Age, fill = PERIODO_POSTBDG, col = PERIODO_POSTBDG)) +
  geom_density(alpha= .5)
boxplot(Age ~PERIODO_POSTBDG, data = dati[dati$POPOLAZIONE_PAZIENTI == 1, ] )

dati %>%
  mutate(IFI =  revalue(IFI, c("0"="No IFI", "1"="IFI possibile", '2' =  'IFI probabile', '3' = 'IFI provata')) ) %>% #muovi nomi colore
  ggplot( aes(x = BDG.max, fill = IFI)) +
  geom_density(alpha= 0.8) +
  labs(title="Titolo",
       subtitle=paste("Il p-value del t-test è: " ), #wilcoU$p.value
       y="Densita", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Terapia antifungina') +
  #scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))

dati %>%
  mutate(IFI =  revalue(IFI, c("0"="No IFI", "1"="IFI possibile", '2' =  'IFI probabile', '3' = 'IFI provata')) ) %>% #muovi nomi colore
  ggplot( aes(x = IFI, y = BDG.max, fill = IFI)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot() +
  geom_hline(yintercept=11, linetype="dashed", 
             color = "red", size=0.5) +
  labs(title="Titolo",
       subtitle=paste("Il p-value del t-test: " ),
       y="BDG", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Periodo:') +
  #scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))+
  coord_cartesian( ylim = c(0, 100))

#distribuzione bdgmax con ifi o no ifi
testt <-  t.test(BDG.max ~ INFEZIONE.FUNGINA.PROBABILE, data = dati,
                 alternative = 'two.sided',     
                 conf.level = .95)
testt
wilco <-  wilcox.test(BDG.max~INFEZIONE.FUNGINA.PROBABILE, data = dati)
wilco


dati %>%
  mutate(INFEZIONE.FUNGINA.PROBABILE =  revalue(INFEZIONE.FUNGINA.PROBABILE, c("0"="No IFI e IFI possibile", "1"="IFI probabile e certa")) ) %>% #muovi nomi colore
  ggplot( aes(x = INFEZIONE.FUNGINA.PROBABILE, y = BDG.max, fill = INFEZIONE.FUNGINA.PROBABILE)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot() +
  geom_hline(yintercept=11, linetype="dashed", 
             color = "red", size=0.5) +
  labs(title="Titolo",
       subtitle=paste("Il p-value del test di Wilcoxon <  0.001 " ),
       y= 'BDG(pg/mL)', 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Periodo:') +
  geom_text(x = 0.6, y = 14, label = "Valore limite", color = "red", size = 5) + 
  #scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10)) +
  coord_cartesian( ylim = c(0, 100))


#
dati %>%
  #mutate(IFI =  revalue(IFI, c("0"="No IFI", "1"="IFI possibile", '2' =  'IFI probabile', '3' = 'IFI certa')) ) %>% #muovi nomi colore
  ggplot( aes(x = INFEZIONE.FUNGINA.PROBABILE, y = BDG.max, fill = INFEZIONE.FUNGINA.PROBABILE)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot() +
  geom_hline(yintercept=11, linetype="dashed", 
             color = "red", size=0.5) +
  labs(title="Titolo",
       subtitle=paste("Il p-value del t-test: " ),
       y="BDG", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Periodo:') +
  #scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))


#Days of medicine
krusko <- kruskal.test( Giorni ~ Medicinale, data = graficobox)
krusko

graficobox %>%
  mutate(PeriodoBDG =  revalue(PeriodoBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = Giorni, fill = Medicinale)) +
  geom_density(alpha= 0.6) +
  facet_grid(~ PeriodoBDG) +
  labs(title="Titolo",
       y="Densita", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Medicinali') +
  #scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))


graficobox %>%
  mutate(PeriodoBDG =  revalue(PeriodoBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = Medicinale, y = Giorni, fill = Medicinale)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot() + 
  facet_grid(~ PeriodoBDG) +
  labs(title="Titolo",
       y="Giorni", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Farmaci:') +
  #scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))


#Days of medicine senza isavocunazolo


graficobox %>%
  mutate(PeriodoBDG =  revalue(PeriodoBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  filter(Medicinale != 'Isavuconazolo') %>% 
  ggplot( aes(x = Giorni, fill = Medicinale)) +
  geom_density(alpha= 0.6) +
  facet_grid(~ PeriodoBDG) +
  labs(title="Titolo",
       y="Densita", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Medicinali') +
  #scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))


graficobox %>%
  mutate(PeriodoBDG =  revalue(PeriodoBDG, c("0"="Periodo pre BDG", "1"="Periodo post BDG")) ) %>% #muovi nomi colore
  filter(Medicinale != 'Isavuconazolo') %>% 
  mutate(Medicinale =  revalue(Medicinale, c("Cancidas"="Caspofungina")) ) %>% 
  ggplot( aes(x = Medicinale, y = Giorni, fill = Medicinale)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot() + 
  facet_grid(~ PeriodoBDG) +
  labs(title="Giorni di terapia somministrati per i diversi farmaci antifungini",
       y="Giorni", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Farmaci:') +
  #scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))



#terapia complessiva

dati %>%
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  mutate(TERAPIA_COMPLESSIVA =  revalue(TERAPIA_COMPLESSIVA, c("0"="Nessuna", "1"="Empirica", '2' ='Pre-emptive', '3'= 'Mirata')) ) %>%
  ggplot( aes(x = TOT_GIORN, fill = TERAPIA_COMPLESSIVA)) +
  geom_density(alpha= 0.6) +
  facet_grid(~ PERIODO_POSTBDG) +
  labs(title="Titolo",
       y="Densita", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Terapia complessiva: ') +
  #scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))


dati %>%
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  mutate(TERAPIA_COMPLESSIVA =  revalue(TERAPIA_COMPLESSIVA, c("0"="Nessuna", "1"="Empirica", '2' ='Pre-emptive', '3'= 'Mirata')) ) %>% 
  ggplot( aes(x = TERAPIA_COMPLESSIVA, y = TOT_GIORN, fill = TERAPIA_COMPLESSIVA)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot() + 
  facet_grid(~ PERIODO_POSTBDG) +
  labs(title="Titolo",
       y="Giorni", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Terapia complessiva: ') +
  #scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))



dati %>%
  filter(TERAPIA_COMPLESSIVA != 0) %>% 
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  mutate(TERAPIA_COMPLESSIVA =  revalue(TERAPIA_COMPLESSIVA, c("1"="Empirica", '2' ='Pre-emptive', '3'= 'Mirata')) ) %>%
  ggplot( aes(x = TERAPIA_COMPLESSIVA, y = TOT_GIORN, fill = TERAPIA_COMPLESSIVA)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot() + 
  facet_grid(~ PERIODO_POSTBDG) +
  labs(title="Titolo",
       y="Giorni", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Terapia complessiva: ') +
  #scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))


#ANOVA####
testt <- t.test(TOT_GIORN ~ IFI, data = dati,
                alternative = 'two.sided',     
                conf.level = .95)
wilcoU <- wilcox.test(BDG.max ~ IFI, data = dati)
wilcoU

shapiro.test(dati$Age)
shapiro.test(dati$TOT_GIORN)

kruskal.test( TOT_GIORN ~ IFI, data = dati)

#OMOGENEITA' CAMPIONI####

#coso su serie storica
plot(a)
plot(tab)
tab %>% str()
seriestor <- data.frame(mese = as.Date(names(tab)), valore = as.vector(tab))

ggplot(seriestor, aes(x=mese, y=valore)) +
  geom_line( color="steelblue") + 
  geom_point()


areachart <- xts(x = seriestor$valore, order.by = seriestor$mese)

# Default = line plot --> See chart #316

# Add points
dygraph(areachart) %>%
  dyOptions( drawPoints = TRUE, pointSize = 4, fillGraph = T )




plot_ly(x = ~as.Date(names(tab)), y = ~b$Numero, mode = 'lines') # va messo a posto

plot_ly(x = ~as.Date(names(tab)), y = ~b$Numero) 

#test periodo gender 

tbl <- table(dati$PERIODO_POSTBDG, dati$Gender) 
tbl
chiquadro <- chisq.test(tbl)  #accetto l'ipotesi nulla di indipendenza, differenze dovute dal caso



ggplot(dati, aes(x = Gender, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip()+
  geom_hline(yintercept = 0.3753, color = 'darkgreen', linetype = 'dashed', size = 0.7)

ggplot(dati, aes(x = PERIODO_POSTBDG, fill = Gender)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip()

dati %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  mutate(Gender =  revalue(Gender, c("M"="Male", "F"="Female")) ) %>% #asse y
  #drop_na(S_ASAG_OLTRE) %>% 
  ggplot( aes(x = Gender, fill = PERIODO_POSTBDG)) +
  geom_bar( position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Beta di glucano e Sesso",
       subtitle=paste("Il p-value del test del chiquadro è: " , round(chiquadro$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10)) +
  guides(fill = guide_legend(reverse = TRUE))







#test periodo eta

tbl <- table(dati$PERIODO_POSTBDG, dati$Age) 
chiquadro <- chisq.test(tbl) 
chiquadro$p.value
aggregate(Age ~ PERIODO_POSTBDG, data = dati, summary)
fisher.test(tbl, simulate.p.value = TRUE)
testt <-  t.test(Age ~ PERIODO_POSTBDG, data = dati,
       alternative = 'two.sided',     
       conf.level = .95)
testt
wilco <-  wilcox.test(TOT_GIORN~PERIODO_POSTBDG, data = dati)
wilco
ggplot(dati, aes(Age, fill = PERIODO_POSTBDG, col = PERIODO_POSTBDG)) +
  geom_density(alpha= .5)
ggplot(dati,  aes(x=PERIODO_POSTBDG, y=Age, fill=PERIODO_POSTBDG)) + 
  geom_boxplot()

dati %>%
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = Age, fill = PERIODO_POSTBDG)) +
  geom_density(alpha= 0.8) +
  labs(title="Titolo",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,3)), #wilcoU$p.value
       y="Densita", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Terapia antifungina') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))


dati %>%
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = PERIODO_POSTBDG, y = Age, fill = PERIODO_POSTBDG)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot() +
  labs(title="Titolo",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,3)),
       y="Età", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Periodo:') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))







boxplot(Age ~PERIODO_POSTBDG, data = dati )
          
#test periodo età considerando pazienti e non episodi

testt <- t.test(Age ~ PERIODO_POSTBDG, data = dati[dati$POPOLAZIONE_PAZIENTI == 1, ],
       alternative = 'two.sided',     
       conf.level = .95)
testt
wilco <-  wilcox.test(Age~PERIODO_POSTBDG, data = dati)
wilco

ggplot(dati[dati$POPOLAZIONE_PAZIENTI == 1, ], aes(Age, fill = PERIODO_POSTBDG, col = PERIODO_POSTBDG)) +
  geom_density(alpha= .5)
boxplot(Age ~PERIODO_POSTBDG, data = dati[dati$POPOLAZIONE_PAZIENTI == 1, ] )

dati[dati$POPOLAZIONE_PAZIENTI == 1, ] %>%
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = Age, fill = PERIODO_POSTBDG)) +
  geom_density(alpha= 0.8) +
  labs(title="Titolo",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,3)), #wilcoU$p.value
       y="Densita", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Terapia antifungina') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))

dati[dati$POPOLAZIONE_PAZIENTI == 1, ] %>%
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = PERIODO_POSTBDG, y = Age, fill = PERIODO_POSTBDG)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot() +
  labs(title="Titolo",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,3)),
       y="Età", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Periodo:') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))



#grafico patologie

dati %>%
  mutate(Pat_Emat =  revalue(Pat_Emat, c('GAMMOPATIA MONOCLONALE'='GAMMOPATIA \n MONOCLONALE')) ) %>% 
  mutate(Pat_Emat = fct_reorder(Pat_Emat, Pat_Emat, .fun = 'length')) %>% 
  ggplot( aes(Pat_Emat)) +
  geom_bar(stat = "count", fill = heat.colors(9)[9:1]) +
  labs(title="Titolo",
       subtitle='Sottotitolo',
       y="Numero", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Periodo:') +
    theme_economist() +
   theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10),
    axis.text.x = element_text(angle = 0)) 

dati %>%
  mutate(Pat_Emat =  revalue(Pat_Emat, c('GAMMOPATIA MONOCLONALE'='GAMMOPATIA \n MONOCLONALE')) ) %>% 
  mutate(Pat_Emat = fct_reorder(Pat_Emat, Pat_Emat, .fun = 'length')) %>% 
  ggplot( aes(Pat_Emat)) +
  geom_bar(stat = "count", fill = rainbow(9)[9:1]) +
  labs(title="Titolo",
       subtitle='Sottotitolo',
       y="Numero", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Periodo:') +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10),
    axis.text.x = element_text(angle = 0)) 

dati %>%
  mutate(Pat_Emat =  revalue(Pat_Emat, c('GAMMOPATIA MONOCLONALE'='GAMMOPATIA \n MONOCLONALE')) ) %>% 
  mutate(Pat_Emat = fct_reorder(Pat_Emat, Pat_Emat, .fun = 'length')) %>% 
  ggplot( aes(Pat_Emat)) +
  geom_bar(stat = "count", fill = '#12BDEF') +
  labs(title="Titolo",
       subtitle='Sottotitolo',
       y="Numero", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Periodo:') +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10),
    axis.text.x = element_text(angle = 0))

dati %>%
  mutate(Pat_Emat =  revalue(Pat_Emat, c('GAMMOPATIA MONOCLONALE'='GAMMOPATIA \n MONOCLONALE')) ) %>% 
  mutate(Pat_Emat = fct_reorder(Pat_Emat, Pat_Emat, .fun = 'length')) %>% 
  mutate(INFEZIONE.FUNGINA.PROBABILE =  revalue(INFEZIONE.FUNGINA.PROBABILE, c("0"="No IFI", "1"="Infezione fungina probabile")) ) %>%
  ggplot( aes(Pat_Emat,  fill = INFEZIONE.FUNGINA.PROBABILE)) +
  geom_bar(stat = "count") +
  labs(title="Titolo",
       subtitle='Sottotitolo',
       y="Numero", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Periodo:') +
  scale_fill_manual(values=c('#12BDEF', "red")) + #FD9200 #12BDEF
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10),
    axis.text.x = element_text(angle = 0))



ggplot(dati, aes(Pat_Emat)) +
  geom_histogram(stat = "count", col = "purple", fill= "green") +
  theme_dark() 
  


# test periodo patologie 

tbl <- table(dati$PERIODO_POSTBDG, dati$Pat_Emat) 
tbl
chisq.test(tbl)
fisher <- fisher.test(tbl, simulate.p.value = T)
fisher

ggplot(dati, aes(x = Pat_Emat, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() 


dati %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  mutate(Pat_Emat =  revalue(Pat_Emat, c('GAMMOPATIA MONOCLONALE'='GAMMOPATIA \n MONOCLONALE')) ) %>%  #asse y
  #drop_na(S_ASAG_OLTRE) %>% 
  ggplot( aes(x = Pat_Emat, fill = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Beta di glucano e Sesso",
       subtitle=paste("Il p-value del test esatto di Fisher è: " , round(fisher$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))


# test periodo e terapia
tbl <- table(dati$PERIODO_POSTBDG, dati$TERAPIA_COMPLESSIVA) 
tbl
chiquadro <- chisq.test(tbl)


ggplot(dati, aes(x = TERAPIA_COMPLESSIVA, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip()

dati %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  #mutate(TERAPIA_COMPLESSIVA =  revalue(TERAPIA_COMPLESSIVA, c("M"="Male", "F"="Female")) ) %>% #asse y
  #drop_na(S_ASAG_OLTRE) %>% 
  ggplot( aes(x = TERAPIA_COMPLESSIVA, fill = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Terapia e Periodo",
       subtitle=paste("Il p-value del test del chiquadro è: " , round(chiquadro$p.value,3)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))

#test periodo e profilassi più di cinque giorni

tbl <- table(dati$PERIODO_POSTBDG, dati$Profilassiover5) 
tbl
chiquadro<- chisq.test(tbl)
chiquadro

ggplot(dati, aes(x = Profilassiover5, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip()

dati %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  mutate(Profilassiover5 =  revalue(Profilassiover5, c("0"="Tempo di profilassi \n Minore di 5 giorni", "1"="Tempo di profilassi \n Maggiore di 5 giorni")) ) %>% #asse y
  #drop_na(S_ASAG_OLTRE) %>% 
  ggplot( aes(x = Profilassiover5, fill = PERIODO_POSTBDG)) +
  geom_bar( position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Profilassi e Periodo",
       subtitle=paste("Il p-value del test del chiquadro è: " , round(chiquadro$p.value,3)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10)) +
  guides(fill = guide_legend(reverse = TRUE))



dati %>%
  #mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  mutate(Profilassiover5 =  revalue(Profilassiover5, c("0"="Tempo di profilassi \n Minore di 5 giorni", "1"="Tempo di profilassi \n Maggiore di 5 giorni")) ) %>% #asse y
  #drop_na(S_ASAG_OLTRE) %>% 
  ggplot( aes(x = PERIODO_POSTBDG, fill = Profilassiover5)) +
  geom_bar( position = "fill") +
  labs(title="Relazione tra Profilassi e Periodo",
       subtitle=paste("Il p-value del test del chiquadro è: " , round(chiquadro$p.value,3)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))+
  guides(fill = guide_legend(reverse = TRUE))


#test profilassi con patologie
tbl <- table(dati$Pat_Emat, dati$Profilassiover5) 
tbl
chisq.test(tbl)
fisher <- fisher.test(tbl, simulate.p.value = T)
fisher

ggplot(dati, aes(x = Pat_Emat, fill = Profilassiover5, color = Profilassiover5)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip()

ggplot(dati, aes(x = Pat_Emat, fill = Profilassiover5)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()

ggplot(dati, aes(x = Pat_Emat, fill = Profilassiover5)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# nomipat <- levels(dati$Pat_Emat)
# nomipat[2] <- "GAMMOPATIA \n MONOCLONALE"
# levels(dati$Pat_Emat) <- nomipat

dati %>%
  mutate(Profilassiover5 = fct_relevel(Profilassiover5, '1', '0')) %>%  #ordine colore
  mutate(Profilassiover5 =  revalue(Profilassiover5, c("0"="Profilassi inferiore 5", "1"="Profilassi superio di")) ) %>% #muovi nomi colore
  mutate(Pat_Emat = fct_reorder(Pat_Emat, Pat_Emat, .fun = 'length')) %>% 
  mutate(Pat_Emat =  revalue(Pat_Emat, c('GAMMOPATIA MONOCLONALE'='GAMMOPATIA \n MONOCLONALE')) ) %>%
  ggplot( aes(Pat_Emat, fill = Profilassiover5)) +
  geom_bar(stat = "count") +
  labs(title="Titolo",
       subtitle='Sottotitolo',
       y="Numero", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Profilassi:') +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10),
    axis.text.x = element_text(angle = 0))
    #plot.background = element_rect(fill = '#F2EFB8')) 



#grafico a tre con patologia ematologica in assoluto con pre e dopo e profilassi over 5.

loglm(formula = ~ Pat_Emat + Profilassiover5 + INFEZIONE.FUNGINA.PROBABILE, data = dati)

ggplot(dati, aes(x = Pat_Emat,  fill = Profilassiover5)) + 
  geom_bar(stat = 'count', position = 'stack') +
  facet_grid(~ neoperiodo) + ggtitle('Barplot patologie condizionato a profilassi prima e dopo') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#mosaic
library(ggmosaic)
ggplot(data = fly) +
  geom_mosaic(aes(x = product(DoYouRecline, RudeToRecline), fill=DoYouRecline), na.rm=TRUE) +  labs(x = "Is it rude recline? ", title='f(DoYouRecline, RudeToRecline| Gender)') + facet_grid(Gender~.)

dati %>%
  mutate(Profilassiover5 = fct_relevel(Profilassiover5, '1', '0')) %>%  #ordine colore
  mutate(Profilassiover5 =  revalue(Profilassiover5, c("0"="Profilassi inferiore 5", "1"="Profilassi superiore di 5")) ) %>% #muovi nomi colore
  mutate(Pat_Emat = fct_reorder(Pat_Emat, Pat_Emat, .fun = 'length')) %>% 
  ggplot( aes(Profilassiover5, fill = Pat_Emat)) +
  geom_mosaic(aes(x = product(Pat_Emat, Profilassiover5 ), fill = Pat_Emat)) +
  facet_grid(~ neoperiodo) +
  labs(title="Titolo",
       subtitle='Sottotitolo',
       y="Numero", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Profilassi:') + 
  #scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10),
    axis.text.x = element_text(angle = 90))+
  guides(fill = guide_legend(reverse = TRUE)) 




dati %>%
  mutate(Profilassiover5 = fct_relevel(Profilassiover5, '1', '0')) %>%  #ordine colore
  mutate(Profilassiover5 =  revalue(Profilassiover5, c("0"="Profilassi inferiore 5", "1"="Profilassi superiore di 5")) ) %>% #muovi nomi colore
  mutate(Pat_Emat = fct_reorder(Pat_Emat, Pat_Emat, .fun = 'length')) %>% 
  ggplot( aes(Pat_Emat, fill = Profilassiover5)) +
  facet_grid(~ neoperiodo) +
  geom_bar(stat = "count", position = 'stack') +
  labs(title="Titolo",
       subtitle='Sottotitolo',
       y="Numero", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Profilassi:') + 
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10),
    axis.text.x = element_text(angle = 90))+
  guides(fill = guide_legend(reverse = TRUE)) +
  guides(fill = guide_legend(reverse = TRUE))
#plot.background = element_rect(fill = '#F2EFB8')) 


tbl <- table(dati$Profilassiover5, dati$Pat_Emat, dati$INFEZIONE.FUNGINA.PROBABILE, dnn = c('Profilassi', 'Patologie', 'Infezione_fungina'))

#profilassi, patologia ed infezione fungina probabile
loglm(formula = ~ Profilassi + Patologie + Infezione_fungina, data = tbl)
loglm(~Profilassi+Patologie+Infezione_fungina+Profilassi*Patologie+Patologie*Infezione_fungina+Infezione_fungina*Profilassi, tbl)

mantelhaen.test(tbl)

dati %>%
  mutate(Profilassiover5 = fct_relevel(Profilassiover5, '1', '0')) %>%  #ordine colore
  mutate(Profilassiover5 =  revalue(Profilassiover5, c("0"="Profilassi inferiore 5", "1"="Profilassi superiore di 5")) ) %>% #muovi nomi colore
  mutate(Pat_Emat = fct_reorder(Pat_Emat, Pat_Emat, .fun = 'length')) %>% 
  ggplot( aes(Pat_Emat, fill = Profilassiover5)) +
  facet_grid(~ INFEZIONE.FUNGINA.PROBABILE) +
  geom_bar(stat = "count", position = 'fill') +
  coord_flip() +
  labs(title="Titolo",
       subtitle='Sottotitolo',
       y="Numero", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Profilassi:') +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10),
    axis.text.x = element_text(angle = 90),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank()
    
    )+
  guides(fill = guide_legend(reverse = TRUE)) 


#alterazione polmonari periodo
tbl <- table(dati$PERIODO_POSTBDG, dati$Alterazioni_radiologiche_polmonari) 
tbl
chiquadro <- chisq.test(tbl)
chiquadro
ggplot(dati, aes(x = Alterazioni_radiologiche_polmonari, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip()

dati %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  mutate(Alterazioni_radiologiche_polmonari =  revalue(Alterazioni_radiologiche_polmonari, c("0"="No alterazioni radiologiche", "1"="Alterazioni aspecifiche", '2' =  'Alterazioni indicative \n di patolofia fungina')) ) %>% #asse y
  ggplot( aes(x = Alterazioni_radiologiche_polmonari, fill = PERIODO_POSTBDG)) +
  geom_bar( position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Alterazioni radiologiche polmonari e Periodo",
       subtitle=paste("Il p-value del test del chiquadro è: " , round(chiquadro$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10)) +
  guides(fill = guide_legend(reverse = TRUE))


dati %>%
  #mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  mutate(Alterazioni_radiologiche_polmonari =  revalue(Alterazioni_radiologiche_polmonari, c("0"="No alterazioni radiologiche", "1"="Alterazioni aspecifiche", '2' =  'Alterazioni indicative \n di patolofia fungina')) ) %>% #asse y
  ggplot( aes(x = PERIODO_POSTBDG, fill = Alterazioni_radiologiche_polmonari)) +
  geom_bar( position = "fill") +
  labs(title="Relazione tra Alterazioni radiologiche polmonari e Periodo",
       subtitle=paste("Il p-value del test del chiquadro è: " , round(chiquadro$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  #scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10)) +
  guides(fill = guide_legend(reverse = TRUE))

# Ifi e periodo
tbl <- table(dati$PERIODO_POSTBDG, dati$IFI) 
tbl
chisq.test(tbl)
fisher <- fisher.test(tbl)
fisher

ggplot(dati, aes(x = IFI, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip()

dati %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  mutate(IFI =  revalue(IFI, c("0"="No IFI", "1"="IFI possibile", '2' =  'IFI probabile', '3' = 'IFI provata')) ) %>% #asse y
  ggplot( aes(x = IFI, fill = PERIODO_POSTBDG)) +
  geom_bar( position = "fill") +
  coord_flip() +
  labs(title="Relazione tra IFI e Periodo",
       subtitle=paste("Il p-value del test esatto di Fisher è: " , round(fisher$p.value,3)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))+
  guides(fill = guide_legend(reverse = TRUE))




ggplot(dati, aes(x = IFI, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8) 

dati %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  mutate(IFI =  revalue(IFI, c("0"="No IFI", "1"="IFI possibile", '2' =  'IFI probabile', '3' = 'IFI provata')) ) %>% #asse y
  ggplot( aes(x = IFI, fill = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8) +
  labs(title="Relazione tra IFI e Periodo",
       subtitle=paste("Il p-value del test esatto di Fisher è: " , round(fisher$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))



# Ifi senza BDG e periodo
tbl <- table(dati$PERIODO_POSTBDG, dati$IFI.SENZA.BDG) 
tbl
chisq.test(tbl)
fisher <- fisher.test(tbl)
fisher

ggplot(dati, aes(x = IFI.SENZA.BDG, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip()

dati %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  mutate(IFI.SENZA.BDG = as.factor(IFI.SENZA.BDG)) %>% 
  mutate(IFI.SENZA.BDG =  revalue(IFI.SENZA.BDG, c("0"="No IFI", "1"="IFI possibile", '2' =  'IFI probabile', '3' = 'IFI provata')) ) %>% #asse y
  ggplot( aes(x = IFI.SENZA.BDG, fill = PERIODO_POSTBDG)) +
  geom_bar( position = "fill") +
  coord_flip() +
  labs(title="Relazione tra IFI e Periodo",
       subtitle=paste("Il p-value del test del chiquadro è: " , round(fisher$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))+
  guides(fill = guide_legend(reverse = TRUE))




ggplot(dati, aes(x = IFI.SENZA.BDG, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8) 

dati %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  mutate(IFI.SENZA.BDG =  revalue(IFI.SENZA.BDG, c("0"="No IFI", "1"="IFI possibile", '2' =  'IFI probabile', '3' = 'IFI provata')) ) %>% #asse y
  ggplot( aes(x = IFI.SENZA.BDG, fill = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8) +
  labs(title="Relazione tra IFI e Periodo",
       subtitle=paste("Il p-value del test del chiquadro è: " , round(fisher$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))


# periodo e Fuo

tbl <- table(dati$PERIODO_POSTBDG, dati$NEOFUO..retrospettiva., dnn = c("Periodo", "FUO")) 
tbl
chiquadro <- chisq.test(tbl) #ipotesi nulla indipendenza
chiquadro


ggplot(dati, aes(x = NEOFUO..retrospettiva., fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() 

dati %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  mutate(NEOFUO..retrospettiva. =  revalue(NEOFUO..retrospettiva., c("0"="No Fuo", "1"="Fuo")) ) %>% #asse y
  ggplot( aes(x = NEOFUO..retrospettiva., fill = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Fuo e Periodo",
       subtitle=paste("Il p-value del test del chiquadro è: " , round(chiquadro$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))

#Test periodo classificazione FUO IFI ASPECIFICO


tbl <- table(dati$PERIODO_POSTBDG, dati$Classificazione.FUO.IFI.ASPECIFICO, dnn = c("Periodo", "Classificazione")) 
tbl
chisq.test(tbl) #non definitiva
fisher <- fisher.test(tbl)
fisher

ggplot(dati, aes(x = Classificazione.FUO.IFI.ASPECIFICO, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() 

dati %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = Classificazione.FUO.IFI.ASPECIFICO, fill = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Alterazioni e Periodo",
       subtitle=paste("Il p-value del test esatto di Fisher è: " , round(fisher$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))

# colonizzato e periodo
Colper <- table(dati$PERIODO_POSTBDG, dati$Colonizzazato, dnn = c("Periodo", "Colonizzato"))
Colper
chiquadro <- chisq.test(Colper) # sono indipendenti
chiquadro

ggplot(dati, aes(x = PERIODO_POSTBDG, fill = Colonizzazato, color = Colonizzazato)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip()

dati %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  mutate(Colonizzazione =  revalue(Colonizzazato, c("0"="Non colonizzato", "1"="Colonizzato")) ) %>% #asse y
  ggplot( aes(x = Colonizzazione, fill = PERIODO_POSTBDG)) +
  geom_bar( position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Colonizzazione e Periodo",
       subtitle=paste("Il p-value del test del chiquadro è: " , round(chiquadro$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10)) +
  guides(fill = guide_legend(reverse = TRUE))



dati %>%
  #mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  mutate(Colonizzazione =  revalue(Colonizzazato, c("0"="Non colonizzato", "1"="Colonizzato")) ) %>% #asse y
  ggplot( aes(x = PERIODO_POSTBDG, fill = Colonizzazione)) +
  geom_bar( position = "fill") +
  labs(title="Relazione tra Colonizzazione e Periodo",
       subtitle=paste("Il p-value del test del chiquadro è: " , round(chiquadro$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10)) +
  guides(fill = guide_legend(reverse = TRUE))




#CAMBIAMENTO DELL'APPROCCIO DEL CLINICO####

#alterazione specifiche per ifi e terapia

tbl <- table(dati$ALTERAZIONI.SPECIFICHE.PER.IFI..GENERALE., dati$TERAPIA_ANTFUN, dnn = c("Periodo", "Terapiatrico"))
tbl
chiquadro <- chisq.test(tbl)
chiquadro


dati %>%
  mutate(ALTERAZIONI.SPECIFICHE.PER.IFI..GENERALE. = fct_relevel(ALTERAZIONI.SPECIFICHE.PER.IFI..GENERALE., '1', '0')) %>%  #ordine colore
  mutate(ALTERAZIONI.SPECIFICHE.PER.IFI..GENERALE. =  revalue(ALTERAZIONI.SPECIFICHE.PER.IFI..GENERALE., c("0"="Non indicative di IFI", "1"="Indicative")) ) %>% #muovi nomi colore
  mutate(TERAPIA_ANTFUN =  revalue(TERAPIA_ANTFUN, c("0"="No Terapia", "1"="Terapia")) ) %>% #asse y
  ggplot( aes(x = TERAPIA_ANTFUN, fill = ALTERAZIONI.SPECIFICHE.PER.IFI..GENERALE.)) +
  geom_bar( position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Terapia e Alterazioni",
       subtitle=paste("Il p-value del test del chiquadro è: " , round(chiquadro$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Alterazioni: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10)) +
  guides(fill = guide_legend(reverse = TRUE))


#terapia e periodo
tbl <- table(dati$PERIODO_POSTBDG, dati$TERAPIA_ANTFUN, dnn = c("Periodo", "Terapiatrico"))
tbl
chiquadro <- chisq.test(tbl)
chiquadro


dati %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  mutate(TERAPIA_ANTFUN =  revalue(TERAPIA_ANTFUN, c("0"="No Terapia", "1"="Terapia")) ) %>% #asse y
  ggplot( aes(x = TERAPIA_ANTFUN, fill = PERIODO_POSTBDG)) +
  geom_bar( position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Terapia e Periodo",
       subtitle=paste("Il p-value del test del chiquadro è: " , round(chiquadro$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10)) +
  guides(fill = guide_legend(reverse = TRUE))




#Ricorso terapie tricomplessiva e  Periodo

tbl <- table(dati$PERIODO_POSTBDG, dati$TERAPIA_TRICOMPLESSIVA, dnn = c("Periodo", "Terapiatrico"))
tbl
chiquadro <- chisq.test(tbl)
chiquadro


ggplot(dati, aes(x = TERAPIA_TRICOMPLESSIVA, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip()

dati %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  #mutate(TERAPIA_TRICOMPLESSIVA =  revalue(TERAPIA_TRICOMPLESSIVA, c("0"="Non colonizzato", "1"="Colonizzato")) ) %>% #asse y
  ggplot( aes(x = TERAPIA_TRICOMPLESSIVA, fill = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Terapia tricomplessiva e Periodo",
       subtitle=paste("Il p-value del test del chiquadro è: " , round(chiquadro$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))



ggplot(dati, aes(x = TERAPIA_TRICOMPLESSIVA, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8) 

dati %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  #mutate(TERAPIA_TRICOMPLESSIVA =  revalue(TERAPIA_TRICOMPLESSIVA, c("0"="Non colonizzato", "1"="Colonizzato")) ) %>% #asse y
  ggplot( aes(x = TERAPIA_TRICOMPLESSIVA, fill = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8) +
  labs(title="Relazione tra Terapia tricomplessiva e Periodo",
       subtitle=paste("Il p-value del test del chiquadro è: " , round(chiquadro$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))




ggplot(dati, aes(x = PERIODO_POSTBDG, fill = TERAPIA_TRICOMPLESSIVA, color = TERAPIA_TRICOMPLESSIVA)) +
  geom_bar(alpha = 0.8, position = "fill") +   guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE))

dati %>%
  mutate(TERAPIA_TRICOMPLESSIVA = fct_relevel(TERAPIA_TRICOMPLESSIVA, '2', '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  #mutate(TERAPIA_TRICOMPLESSIVA =  revalue(TERAPIA_TRICOMPLESSIVA, c("0"="Non colonizzato", "1"="Colonizzato")) ) %>% #asse y
  ggplot( aes(x = PERIODO_POSTBDG, fill = TERAPIA_TRICOMPLESSIVA)) +
  geom_bar(alpha = 0.8) +
  labs(title="Relazione tra Terapia tricomplessiva e Periodo",
       subtitle=paste("Il p-value del test del chiquadro è: " , round(chiquadro$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF", 'darkgreen')) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))




#Ricorso terapia complessiva e periodo gia fatto sopra 

tbl <- table(dati$PERIODO_POSTBDG, dati$TERAPIA_COMPLESSIVA, dnn = c("Periodo", "Terapiatrico"))
tbl
chiquadro <- chisq.test(tbl)
chiquadro

ggplot(dati, aes(x = TERAPIA_COMPLESSIVA, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip()


dati %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  mutate(TERAPIA_COMPLESSIVA =  revalue(TERAPIA_COMPLESSIVA, c('0' = "No Terapia", '1' ='Terapia empirica ','2'= 'Terapia pre-emptive', '3'= 'Terapia mirata')) ) %>% #asse y
  ggplot( aes(x = TERAPIA_COMPLESSIVA, fill = PERIODO_POSTBDG)) +
  geom_bar( position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Terapia tricomplessiva e Periodo",
       subtitle=paste("Il p-value del test del chiquadro è: " , round(chiquadro$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10)) +
  guides(fill = guide_legend(reverse = TRUE))


dati %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  mutate(TERAPIA_COMPLESSIVA =  revalue(TERAPIA_COMPLESSIVA, c('0' = "No Terapia", '1' ='Terapia empirica ','2'= 'Terapia pre-emptive', '3'= 'Terapia mirata')) ) %>% #asse y
  ggplot( aes(x = TERAPIA_COMPLESSIVA, fill = PERIODO_POSTBDG)) +
  geom_bar( position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Terapia tricomplessiva e Periodo",
       subtitle=paste("Il p-value del test del chiquadro è: " , round(chiquadro$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10)) +
  guides(fill = guide_legend(reverse = TRUE))

dati %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  mutate(TERAPIA_COMPLESSIVA =  revalue(TERAPIA_COMPLESSIVA, c('0' = "No Terapia", '1' ='Terapia empirica ','2'= 'Terapia pre-emptive', '3'= 'Terapia mirata')) ) %>% #asse y
  ggplot( aes(x = TERAPIA_COMPLESSIVA, fill = PERIODO_POSTBDG)) +
  geom_bar( ) +
  labs(title="Relazione tra Terapia tricomplessiva e Periodo",
       subtitle=paste("Il p-value del test del chiquadro è: " , round(chiquadro$p.value,2)),
       y="Count", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10)) +
  guides(fill = guide_legend(reverse = TRUE))

dati %>%
  #mutate(TERAPIA_COMPLESSIVA = fct_relevel(TERAPIA_COMPLESSIVA, '3', '2', '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  mutate(TERAPIA_COMPLESSIVA =  revalue(TERAPIA_COMPLESSIVA, c('0' = "No Terapia", '1' ='Terapia empirica ','2'= 'Terapia pre-emptive', '3'= 'Terapia mirata')) ) %>% #asse y
  ggplot( aes(x = PERIODO_POSTBDG, fill = TERAPIA_COMPLESSIVA)) +
  geom_bar( position = "fill") +
  #coord_flip() +
  labs(title="Relazione tra Terapia tricomplessiva e Periodo",
       subtitle=paste("Il p-value del test del chiquadro è: " , round(chiquadro$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values= c("#F88A00", "#F9F600", '#26A6B5', '#400851')) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10)) +
  guides(fill = guide_legend(reverse = TRUE))



# Relazione periodo e terapia tricomplessiva condizionato a neofuo = 1


tbl <- table(datineofuo1$PERIODO_POSTBDG, datineofuo1$TERAPIA_TRICOMPLESSIVA)[, -3] 
chisq.test(tbl)
fisher <- fisher.test(tbl)
fisher

ggplot(datineofuo1, aes(x = TERAPIA_TRICOMPLESSIVA, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip()

datineofuo1 %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  #mutate(TERAPIA_TRICOMPLESSIVA =  revalue(TERAPIA_TRICOMPLESSIVA, c("0"="Non colonizzato", "1"="Colonizzato")) ) %>% #asse y
  ggplot( aes(x = TERAPIA_TRICOMPLESSIVA, fill = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Terapia tricomplessiva e Periodo condizionato a neofuo = 1",
       subtitle=paste("Il p-value del test esatto di Fisher è: " , round(fisher$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))




#terapia tricomplessiva e periodo condizionato a IFI

#IFI diverso da 0
tbl <- table(datisiifi$PERIODO_POSTBDG, datisiifi$TERAPIA_TRICOMPLESSIVA, dnn = c("Periodo", "Terapia tricomplessiva"))
tbl
chisq.test(tbl)
fisher.test(tbl)
fisher <- fisher.test(tbl)

ggplot(datisiifi, aes(x = TERAPIA_TRICOMPLESSIVA, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip()

datisiifi %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  #mutate(TERAPIA_TRICOMPLESSIVA =  revalue(TERAPIA_TRICOMPLESSIVA, c("0"="Non colonizzato", "1"="Colonizzato")) ) %>% #asse y
  ggplot( aes(x = TERAPIA_TRICOMPLESSIVA, fill = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Terapia tricomplessiva e Periodo condizionato a IFI almeno possibile",
       subtitle=paste("Il p-value del test esatto di Fisher è: " , round(fisher$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))


#IFI=0

tbl <- table(datiifi0$PERIODO_POSTBDG, datiifi0$TERAPIA_TRICOMPLESSIVA, dnn = c("Periodo", "Terapia tricomplessiva"))[, -3] 
tbl
chiquadro <- chisq.test(tbl) 
chisq.test(tbl)
fisher.test(tbl)

ggplot(datiifi0, aes(x = TERAPIA_TRICOMPLESSIVA, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() 

datiifi0 %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  #mutate(TERAPIA_TRICOMPLESSIVA =  revalue(TERAPIA_TRICOMPLESSIVA, c("0"="Non colonizzato", "1"="Colonizzato")) ) %>% #asse y
  ggplot( aes(x = TERAPIA_TRICOMPLESSIVA, fill = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Terapia tricomplessiva e Periodo condizionato a IFI non presente",
       subtitle=paste("Il p-value del test del chiquadro è: " , round(chiquadro$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))


#IFI=1

tbl <- table(datiifi1$PERIODO_POSTBDG, datiifi1$TERAPIA_TRICOMPLESSIVA, dnn = c("Periodo", "Terapia tricomplessiva"))[, -3]
tbl
chisq.test(tbl)
fisher<- fisher.test(tbl)
fisher.test(tbl)


ggplot(datiifi1, aes(x = TERAPIA_TRICOMPLESSIVA, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() 

datiifi1 %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  #mutate(TERAPIA_TRICOMPLESSIVA =  revalue(TERAPIA_TRICOMPLESSIVA, c("0"="Non colonizzato", "1"="Colonizzato")) ) %>% #asse y
  ggplot( aes(x = TERAPIA_TRICOMPLESSIVA, fill = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Terapia tricomplessiva e Periodo condizionato a IFI possibile",
       subtitle=paste("Il p-value del test esatto di Fisher è: " , round(fisher$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))


#IFI=2

tbl <- table(datiifi2$PERIODO_POSTBDG, datiifi2$TERAPIA_TRICOMPLESSIVA, dnn = c("Periodo", "Terapia tricomplessiva")) 
tbl
chisq.test(tbl)
fisher.test(tbl)
fisher <- fisher.test(tbl)

ggplot(datiifi2, aes(x = TERAPIA_TRICOMPLESSIVA, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() 

datiifi2 %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  #mutate(TERAPIA_TRICOMPLESSIVA =  revalue(TERAPIA_TRICOMPLESSIVA, c("0"="Non colonizzato", "1"="Colonizzato")) ) %>% #asse y
  ggplot( aes(x = TERAPIA_TRICOMPLESSIVA, fill = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Terapia tricomplessiva e Periodo condizionato a IFI probabile",
       subtitle=paste("Il p-value del test esatto di Fisher è: " , round(fisher$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))

#IFI=3

tbl <- table(datiifi3$PERIODO_POSTBDG, datiifi3$TERAPIA_TRICOMPLESSIVA, dnn = c("Periodo", "Terapia tricomplessiva"))
tbl
chisq.test(tbl) 

ggplot(datiifi3, aes(x = TERAPIA_TRICOMPLESSIVA, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() 




# Relazione periodo e terapia (COMPLESSIVA DA ORA IN OI FINO AL TERMINE) condizionato a neofuo = 1

dati$TERAPIA_COMPLESSIVA
tbl <- table(datineofuo1$PERIODO_POSTBDG, datineofuo1$TERAPIA_COMPLESSIVA)[, -4] 
chisq.test(tbl)
fisher <- fisher.test(tbl)
fisher

ggplot(datineofuo1, aes(x = TERAPIA_COMPLESSIVA, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip()

datineofuo1 %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  #mutate(TERAPIA_COMPLESSIVA =  revalue(TERAPIA_COMPLESSIVA, c("0"="Non colonizzato", "1"="Colonizzato")) ) %>% #asse y
  ggplot( aes(x = TERAPIA_COMPLESSIVA, fill = PERIODO_POSTBDG)) +
  geom_bar( position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Terapia tricomplessiva e Periodo condizionato a neofuo = 1",
       subtitle=paste("Il p-value del test esatto di Fisher è: " , round(fisher$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))+
  guides(fill = guide_legend(reverse = TRUE))




#terapia tricomplessiva e periodo condizionato a IFI

#IFI diverso da 0
tbl <- table(datisiifi$PERIODO_POSTBDG, datisiifi$TERAPIA_COMPLESSIVA, dnn = c("Periodo", "Terapia tricomplessiva"))[, -2]
tbl
chisq.test(tbl)
fisher.test(tbl)
fisher <- fisher.test(tbl)

ggplot(datisiifi, aes(x = TERAPIA_COMPLESSIVA, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip()

datisiifi %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  #mutate(TERAPIA_COMPLESSIVA =  revalue(TERAPIA_COMPLESSIVA, c("0"="Non colonizzato", "1"="Colonizzato")) ) %>% #asse y
  ggplot( aes(x = TERAPIA_COMPLESSIVA, fill = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Terapia tricomplessiva e Periodo condizionato a IFI almeno possibile",
       subtitle=paste("Il p-value del test esatto di Fisher è: " , round(fisher$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))


#IFI=0

tbl <- table(datiifi0$PERIODO_POSTBDG, datiifi0$TERAPIA_COMPLESSIVA, dnn = c("Periodo", "Terapia tricomplessiva"))[, -4] 
tbl
chiquadro <- chisq.test(tbl) 
chisq.test(tbl)
fisher.test(tbl)

ggplot(datiifi0, aes(x = TERAPIA_COMPLESSIVA, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() 

datiifi0 %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  #mutate(TERAPIA_COMPLESSIVA =  revalue(TERAPIA_COMPLESSIVA, c("0"="Non colonizzato", "1"="Colonizzato")) ) %>% #asse y
  ggplot( aes(x = TERAPIA_COMPLESSIVA, fill = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Terapia tricomplessiva e Periodo condizionato a IFI non presente",
       subtitle=paste("Il p-value del test del chiquadro è: " , round(chiquadro$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))


#IFI=1

tbl <- table(datiifi1$PERIODO_POSTBDG, datiifi1$TERAPIA_COMPLESSIVA, dnn = c("Periodo", "Terapia tricomplessiva")) [,c(-2, -4)]
tbl
chisq.test(tbl)
fisher<- fisher.test(tbl)
fisher.test(tbl)


ggplot(datiifi1, aes(x = TERAPIA_COMPLESSIVA, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() 

datiifi1 %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  #mutate(TERAPIA_COMPLESSIVA =  revalue(TERAPIA_COMPLESSIVA, c("0"="Non colonizzato", "1"="Colonizzato")) ) %>% #asse y
  ggplot( aes(x = TERAPIA_COMPLESSIVA, fill = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Terapia tricomplessiva e Periodo condizionato a IFI possibile",
       subtitle=paste("Il p-value del test esatto di Fisher è: " , round(fisher$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))


#IFI=2

tbl <- table(datiifi2$PERIODO_POSTBDG, datiifi2$TERAPIA_COMPLESSIVA, dnn = c("Periodo", "Terapia tricomplessiva")) [,c(-2, -3)]
tbl
chisq.test(tbl)
fisher.test(tbl)
fisher <- fisher.test(tbl)

ggplot(datiifi2, aes(x = TERAPIA_COMPLESSIVA, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() 

datiifi2 %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  #mutate(TERAPIA_COMPLESSIVA =  revalue(TERAPIA_COMPLESSIVA, c("0"="Non colonizzato", "1"="Colonizzato")) ) %>% #asse y
  ggplot( aes(x = TERAPIA_COMPLESSIVA, fill = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Terapia tricomplessiva e Periodo condizionato a IFI probabile",
       subtitle=paste("Il p-value del test esatto di Fisher è: " , round(fisher$p.value,2)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))

#IFI=3

tbl <- table(datiifi3$PERIODO_POSTBDG, datiifi3$TERAPIA_COMPLESSIVA, dnn = c("Periodo", "Terapia tricomplessiva"))
tbl
chisq.test(tbl) 

ggplot(datiifi3, aes(x = TERAPIA_COMPLESSIVA, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() 




#VARI TEST SULLA DURATA DELLA TERAPIA NELLE VARIE CONDIZIONI####

#grafico

dati %>%
  mutate(IFI =  revalue(IFI, c("0"="No IFI", "1"="IFI possibile", '2' =  'IFI probabile', '3' = 'IFI provata')) ) %>% 
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = TOT_GIORN, fill = IFI)) +
  geom_density(alpha= 0.6) +
  facet_grid(~ PERIODO_POSTBDG) +
  labs(title="Titolo",
       y="Densita", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Medicinali') +
  #scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))


dati %>%
  mutate(IFI =  revalue(IFI, c("0"="No IFI", "1"="IFI possibile", '2' =  'IFI probabile', '3' = 'IFI provata')) ) %>% 
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = IFI, y = TOT_GIORN, fill = IFI)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot() + 
  facet_grid(~ PERIODO_POSTBDG) +
  labs(title="Titolo",
       y="Giorni", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'IFI:') +
  #scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))


#grafico solo con i primi due

#grafico

dati %>%
  filter(IFI == '0' | IFI == '1') %>% 
  mutate(IFI =  revalue(IFI, c("0"="No IFI", "1"="IFI possibile")) ) %>% 
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = TOT_GIORN, fill = IFI)) +
  geom_density(alpha= 0.6) +
  facet_grid(~ PERIODO_POSTBDG) +
  labs(title="Titolo",
       y="Densita", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Medicinali') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))


dati %>%
  filter(IFI == '0' | IFI == '1') %>% 
  mutate(IFI =  revalue(IFI, c("0"="No IFI", "1"="IFI possibile")) ) %>% 
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = IFI, y = TOT_GIORN, fill = IFI)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot() + 
  facet_grid(~ PERIODO_POSTBDG) +
  labs(title="Titolo",
       y="Giorni", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'IFI:') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))



#Giorni 


testt <- t.test(TOT_GIORN~PERIODO_POSTBDG, data = dati, alternative = 'two.sided',   conf.level = .95)
testt
t.test(TOT_GIORN~PERIODO_POSTBDG, data = datimod, alternative = 'two.sided',   conf.level = .95)
wilco <-  wilcox.test(TOT_GIORN~PERIODO_POSTBDG, data = dati)
wilco
wilcox.test(TOT_GIORN~PERIODO_POSTBDG, data = datimod)

ggplot(dati, aes(TOT_GIORN, fill = PERIODO_POSTBDG, col = PERIODO_POSTBDG)) +
  geom_density(alpha= .5)

dati %>%
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = TOT_GIORN, fill = PERIODO_POSTBDG)) +
  geom_density(alpha= 0.8) +
  labs(title="Distribuzione totale giorni condizionata a periodo beta di glucano",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,4)), #wilcoU$p.value
       y="Densita", 
       x="Totale giorni",
       caption = "Dataset Niguarda",
       fill = 'Introduzione beta di glucano') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))


boxplot(TOT_GIORN ~PERIODO_POSTBDG, data = dati )

dati %>%
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = PERIODO_POSTBDG, y = TOT_GIORN, fill = PERIODO_POSTBDG)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot() +
  labs(title="Titolo",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,4)),
       y="Totale giorni", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Periodo:') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))


 

fitto <- glm(TOT_GIORN~PERIODO_POSTBDG + IFI, data = dati)
summary(fitto)

#relazione bdg e inizio terapia

tbl <- table(dati$BDG_ALTER, dati$TERAPIA_ANTFUN)
tbl
chiquadro <- chisq.test(tbl) # sono dipendenti
chiquadro
fisher <- fisher.test(tbl)
fisher




dati %>%
  mutate(BDG_ALTER = fct_relevel(BDG_ALTER, '1', '0')) %>%  #ordine colore
  mutate(BDG_ALTER =  revalue(BDG_ALTER, c("0"="Negativo", "1"="Positivo"))  ) %>% #muovi nomi colore
  mutate(TERAPIA_ANTFUN =  revalue(TERAPIA_ANTFUN, c("0"="No Terapia", "1"="Terapia")) ) %>% #asse y
  drop_na(BDG_ALTER) %>% 
  ggplot( aes(x = TERAPIA_ANTFUN, fill = BDG_ALTER)) +
  geom_bar( position = "fill") +
  coord_flip() +
  labs(title="Relazione Terapia e colonizzazione",
       subtitle=paste("Il p-value del test esatto di Fisher è: " , round(fisher$p.value,7)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Terapia antifungina: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10)) +
  guides(fill = guide_legend(reverse = TRUE))


#tot giorno e bdg alter con NA 
testt <- t.test(TOT_GIORN~BDG_ALTER, data = dati, alternative = 'two.sided',   conf.level = .95)
testt
wilco <-  wilcox.test(TOT_GIORN~BDG_ALTER, data = dati)
wilco



dati$BDG_ALTER
dati %>%
  mutate(BDG_ALTER =  revalue(BDG_ALTER, c("0"="Normale", "1"="Alterato")) ) %>% #muovi nomi colore
  drop_na(BDG_ALTER) %>% 
  ggplot( aes(x = TOT_GIORN, fill = BDG_ALTER)) +
  geom_density(alpha= 0.8) +
  labs(title="Distribuzione totale giorni condizionata a periodo beta di glucano",
       subtitle=paste("Il p-value del wilcoxon test è: " , round(wilco$p.value,4)), #wilcoU$p.value
       y="Densita", 
       x="Totale giorni",
       caption = "Dataset Niguarda",
       fill = 'BDG: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))





dati %>%
  mutate(BDG_ALTER =  revalue(BDG_ALTER, c("0"="Normale", "1"="Alterato")) ) %>% #muovi nomi colore
  drop_na(BDG_ALTER) %>% 
  ggplot( aes(x = BDG_ALTER, y = TOT_GIORN, fill = BDG_ALTER)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot() +
  labs(title="Titolo",
       subtitle=paste("Il p-value del Wilcoxon test: " , round(wilco$p.value,4)),
       y="Totale giorni", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Periodo:') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))




#tot giorno e bdg alter con 0
testt <- t.test(TOT_GIORN~BDG_ALTER, data = datimod, alternative = 'two.sided',   conf.level = .95)
testt
wilco <-  wilcox.test(TOT_GIORN~BDG_ALTER, data = datimod)
wilco




datimod %>%
  mutate(BDG_ALTER =  revalue(BDG_ALTER, c("0"="Normale", "1"="Alterato")) ) %>% #muovi nomi colore
  drop_na(BDG_ALTER) %>% 
  ggplot( aes(x = TOT_GIORN, fill = BDG_ALTER)) +
  geom_density(alpha= 0.8) +
  labs(title="Distribuzione totale giorni condizionata a periodo beta di glucano",
       subtitle=paste("Il p-value del wilcoxon test è: " , round(wilco$p.value,8)), #wilcoU$p.value
       y="Densita", 
       x="Totale giorni",
       caption = "Dataset Niguarda",
       fill = 'BDG: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))





datimod %>%
  mutate(BDG_ALTER =  revalue(BDG_ALTER, c("0"="Normale", "1"="Alterato")) ) %>% #muovi nomi colore
  drop_na(BDG_ALTER) %>% 
  ggplot( aes(x = BDG_ALTER, y = TOT_GIORN, fill = BDG_ALTER)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot() +
  labs(title="Titolo",
       subtitle=paste("Il p-value del Wilcoxon test: " , round(wilco$p.value,8)),
       y="Totale giorni", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Periodo:') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))




#Ambisome e Albacet

tbl <- table(dati$PERIODO_POSTBDG, dati$Abelcet) 
chiquadro <- chisq.test(tbl)
chiquadro

ggplot(dati, aes(x = Abelcet, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() 

dati %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  mutate(Abelcet = as.factor(Abelcet)) %>% 
  mutate(Abelcet =  revalue(Abelcet, c("0"="Abelcet non somministrato", "1"="Abelcet somministrato")) ) %>% #asse y
  drop_na(Abelcet) %>% 
  ggplot( aes(x = Abelcet, fill = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Abelcet e Periodo",
       subtitle=paste("Il p-value del test del chiquadro è: " , round(chiquadro$p.value,4)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))


# Ambisome 
tbl <- table(dati$PERIODO_POSTBDG, dati$Ambisome) 
chiquadro <- chisq.test(tbl)
chiquadro
ggplot(dati, aes(x = Ambisome, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip()

dati %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  mutate(Ambisome = as.factor(Ambisome)) %>% 
  mutate(Ambisome =  revalue(Ambisome, c("0"="Ambisome non somministrato", "1"="Ambisome somministrato")) ) %>% #asse y
  drop_na(Ambisome) %>% 
  ggplot( aes(x = Ambisome, fill = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Ambisome e Periodo",
       subtitle=paste("Il p-value del test del chiquadro è: " , round(chiquadro$p.value,1)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))




#Ambisome nei due periodi


ttest <- t.test(Days_Ambisome~PERIODO_POSTBDG, data = datambi, alternative = 'two.sided',   conf.level = .95)
ttest
wilco <-  wilcox.test(Days_Ambisome~PERIODO_POSTBDG, data = datambi)
wilco

ggplot(datambi, aes(Days_Ambisome, fill = PERIODO_POSTBDG, col = PERIODO_POSTBDG)) +
  geom_density(alpha= .5)
dati %>%
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = Days_Ambisome, fill = PERIODO_POSTBDG)) +
  geom_density(alpha= 0.8) +
  labs(title="Distribuzione giorni di Ambisome condizionata a periodo beta di glucano",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,3)), #wilcoU$p.value
       y="Densita", 
       x="Totale giorni",
       caption = "Dataset Niguarda",
       fill = 'Introduzione beta di glucano') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))



boxplot(Days_Ambisome ~PERIODO_POSTBDG, data = dati )

dati %>%
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = PERIODO_POSTBDG, y = Days_Ambisome, fill = PERIODO_POSTBDG)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot() +
  labs(title="Boxplot giorni di Ambisome condizionati a periodo beta di glucano",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,3)),
       y="Giorni di Ambisome", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Periodo:') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))





#Amfotericina

#con NA

tbl <- table(dati$PERIODO_POSTBDG, dati$Amfotericina, dnn = c("Periodo", "Amfotericina"), useNA = "always")[-3, ] 
tbl
chiquadro <- chisq.test(tbl)
chiquadro
ggplot(dati, aes(x = Amfotericina, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() 

dati %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  mutate(Amfotericina = as.factor(Amfotericina)) %>% 
  mutate(Amfotericina =  revalue(Amfotericina, c("0"="Amfotericina non somministrato", "1"="Amfotericina somministrato")) ) %>% #asse y
  #drop_na(Ambisome) %>% 
  ggplot( aes(x = Amfotericina, fill = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Amfotericina e Periodo",
       subtitle=paste("Il p-value del test del chiquadro è: " , round(chiquadro$p.value,1)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))

#Senza NA

tbl <- table(dati$PERIODO_POSTBDG, dati$Amfotericina, dnn = c("Periodo", "Amfotericina")) 
tbl
chiquadro <- chisq.test(tbl)
chiquadro
ggplot(subset(dati, !is.na(Amfotericina)), aes(x = Amfotericina, fill = PERIODO_POSTBDG, color = PERIODO_POSTBDG)) +
  geom_bar(alpha = 0.8, position = "fill") +
  coord_flip() 

dati %>%
  mutate(PERIODO_POSTBDG = fct_relevel(PERIODO_POSTBDG, '1', '0')) %>%  #ordine colore
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  mutate(Amfotericina = as.factor(Amfotericina)) %>% 
  mutate(Amfotericina =  revalue(Amfotericina, c("0"="Amfotericina non somministrato", "1"="Amfotericina somministrato")) ) %>% #asse y
  drop_na(Amfotericina) %>% 
  ggplot( aes(x = Amfotericina, fill = PERIODO_POSTBDG)) +
  geom_bar( position = "fill") +
  coord_flip() +
  labs(title="Relazione tra Amfotericina e Periodo",
       subtitle=paste("Il p-value del test del chiquadro è: " , round(chiquadro$p.value,1)),
       y="Frequenza (%)", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Introduzione Beta di glucano: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor= element_blank(),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10)) +
  guides(fill = guide_legend(reverse = TRUE))
  



#giorni sotto Amfotericina e periodo
shapiro.test(dati$Days_Amfotericina)
testt <- t.test(Days_Amfotericina~PERIODO_POSTBDG, data = dati, alternative = 'two.sided',   conf.level = .95)
testt
wilco <-  wilcox.test(Days_Amfotericina~PERIODO_POSTBDG, data = dati)
wilco

ggplot(dati, aes(Days_Amfotericina, fill = PERIODO_POSTBDG, col = PERIODO_POSTBDG)) +
  geom_density(alpha= .5)
boxplot(Days_Amfotericina ~PERIODO_POSTBDG, data = dati )

dati %>%
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = Days_Amfotericina, fill = PERIODO_POSTBDG)) +
  geom_density(alpha= 0.8) +
  labs(title="Distribuzione giorni di Amfotericina condizionata a periodo beta di glucano",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,3)), #wilcoU$p.value
       y="Densita", 
       x="Totale giorni",
       caption = "Dataset Niguarda",
       fill = 'Introduzione beta di glucano') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))




dati %>%
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = PERIODO_POSTBDG, y = Days_Amfotericina, fill = PERIODO_POSTBDG)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot() +
  labs(title="Boxplot giorni di Amfotericina condizionati a periodo beta di glucano",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,3)),
       y="Giorni di Amfotericina", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Periodo:') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))





#Giorni e periodo condizionato a diversi IFI

#ttest IFI = 0
t.test(TOT_GIORN~PERIODO_POSTBDG, data = dati[dati$IFI == 0, ], alternative = 'two.sided',   conf.level = .90)
wilcox.test(TOT_GIORN~PERIODO_POSTBDG, data = dati[dati$IFI == 0, ])

#ttest IFI diverso da 0
t.test(TOT_GIORN~PERIODO_POSTBDG, data = dati[dati$IFI != 0, ], alternative = 'two.sided',   conf.level = .95)
wilcox.test(TOT_GIORN~PERIODO_POSTBDG, data = dati[dati$IFI != 0, ])

#ttest IFI = 1
t.test(TOT_GIORN~PERIODO_POSTBDG, data = dati[dati$IFI == 1, ], alternative = 'two.sided',   conf.level = .90)
wilcox.test(TOT_GIORN~PERIODO_POSTBDG, data = dati[dati$IFI == 1, ])

#ttest IFI = 1 e IFI =0
t.test(TOT_GIORN~PERIODO_POSTBDG, data = dati[dati$IFI %in% c(0, 1) , ], alternative = 'two.sided',   conf.level = .90)
wilcox.test(TOT_GIORN~PERIODO_POSTBDG, data = dati[dati$IFI %in% c(0, 1) , ])


#ttest IFI = 2
t.test(TOT_GIORN~PERIODO_POSTBDG, data = dati[dati$IFI == 2, ], alternative = 'two.sided',   conf.level = .95)
wilcox.test(TOT_GIORN~PERIODO_POSTBDG, data = dati[dati$IFI == 2, ])

#ttest IFI = 3
t.test(TOT_GIORN~PERIODO_POSTBDG, data = dati[dati$IFI == 3, ], alternative = 'two.sided',   conf.level = .95)
wilcox.test(TOT_GIORN~PERIODO_POSTBDG, data = dati[dati$IFI == 3, ])

#ttest IFI = 2 e IFI =3
t.test(TOT_GIORN~PERIODO_POSTBDG, data = dati[dati$IFI %in% c(2, 3) , ], alternative = 'two.sided',   conf.level = .90)
wilcox.test(TOT_GIORN~PERIODO_POSTBDG, data = dati[dati$IFI %in% c(2, 3) , ])

#Giorni e periodo condizionato a diversi IFI con giorni = 0 non NA


#ttest IFI = 0
t.test(TOT_GIORN~PERIODO_POSTBDG, data = datimod[datimod$IFI == 0, ], alternative = 'two.sided',   conf.level = .95)
wilcox.test(TOT_GIORN~PERIODO_POSTBDG, data = datimod[datimod$IFI == 0, ])

#ttest IFI diverso da 0
t.test(TOT_GIORN~PERIODO_POSTBDG, data = datimod[datimod$IFI != 0, ], alternative = 'two.sided',   conf.level = .95)
wilcox.test(TOT_GIORN~PERIODO_POSTBDG, data = datimod[datimod$IFI != 0, ])

#ttest IFI = 1
t.test(TOT_GIORN~PERIODO_POSTBDG, data = datimod[datimod$IFI == 1, ], alternative = 'two.sided',   conf.level = .95)
wilcox.test(TOT_GIORN~PERIODO_POSTBDG, data = datimod[datimod$IFI == 1, ])

#ttest IFI = 1 e IFI =0
t.test(TOT_GIORN~PERIODO_POSTBDG, data = datimod[dati$IFI %in% c(0, 1) , ], alternative = 'two.sided',   conf.level = .90)
wilcox.test(TOT_GIORN~PERIODO_POSTBDG, data = datimod[dati$IFI %in% c(0, 1) , ])

#ttest IFI = 2
t.test(TOT_GIORN~PERIODO_POSTBDG, data = datimod[datimod$IFI == 2, ], alternative = 'two.sided',   conf.level = .95)
wilcox.test(TOT_GIORN~PERIODO_POSTBDG, data = datimod[datimod$IFI == 2, ])

#ttest IFI = 3
t.test(TOT_GIORN~PERIODO_POSTBDG, data = datimod[datimod$IFI == 3, ], alternative = 'two.sided',   conf.level = .95)
wilcox.test(TOT_GIORN~PERIODO_POSTBDG, data = datimod[datimod$IFI == 3, ])

#ttest IFI = 2 e IFI = 3
t.test(TOT_GIORN~PERIODO_POSTBDG, data = datimod[dati$IFI %in% c(2, 3) , ], alternative = 'two.sided',   conf.level = .90)
wilcox.test(TOT_GIORN~PERIODO_POSTBDG, data = datimod[dati$IFI %in% c(2, 3) , ])

#con giorni = 0 le prime tre differenze nelle medie diventano significative anche al 95%, confermando le non significaficatività per
#IFi =2 e IFI = 3



# Giorni terapia e Fuo DA RIVEDERE  

testt <- t.test(TOT_GIORN~NEOFUO..retrospettiva., data = dati, alternative = 'two.sided',   conf.level = .90)
testt
wilco <-  wilcox.test(TOT_GIORN~NEOFUO..retrospettiva., data = dati)
wilco

ggplot(dati, aes(TOT_GIORN, fill = NEOFUO..retrospettiva., col = NEOFUO..retrospettiva.)) +
  geom_density(alpha= .5)
boxplot(TOT_GIORN ~NEOFUO..retrospettiva., data = dati )


dati %>%
  mutate(NEOFUO..retrospettiva. =  revalue(NEOFUO..retrospettiva., c("0"="No Fuo", "1"="Fuo")) ) %>% #muovi nomi colore
  ggplot( aes(x = TOT_GIORN, fill = NEOFUO..retrospettiva.)) +
  geom_density(alpha= 0.8) +
  labs(title="Distribuzione giorni di terapia condizionata a Fuo",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,3)), #wilcoU$p.value
       y="Densita", 
       x="Totale giorni",
       caption = "Dataset Niguarda",
       fill = 'Introduzione beta di glucano') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))




dati %>%
  mutate(NEOFUO..retrospettiva. =  revalue(NEOFUO..retrospettiva., c("0"="No Fuo", "1"="Fuo")) ) %>% #muovi nomi colore
  ggplot( aes(x = NEOFUO..retrospettiva., y = TOT_GIORN, fill = NEOFUO..retrospettiva.)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot() +
  labs(title="Boxplot giorni di terapia condizionata a Fuo",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,3)),
       y="Totale giorni", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Periodo:') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))



# Giorni terapia e Fuo con NA = 0  DA RIVEDERE
testt <- t.test(TOT_GIORN~NEOFUO..retrospettiva., data = datimod, alternative = 'two.sided',   conf.level = .95)
testt
wilco <-  wilcox.test(TOT_GIORN~NEOFUO..retrospettiva., data = datimod)
wilco

ggplot(datimod, aes(TOT_GIORN, fill = NEOFUO..retrospettiva., col = NEOFUO..retrospettiva.)) +
  geom_density(alpha= .5)
boxplot(TOT_GIORN ~NEOFUO..retrospettiva., data = datimod )

datimod %>%
  mutate(NEOFUO..retrospettiva. =  revalue(NEOFUO..retrospettiva., c("0"="No Fuo", "1"="Fuo")) ) %>% #muovi nomi colore
  ggplot( aes(x = TOT_GIORN, fill = NEOFUO..retrospettiva.)) +
  geom_density(alpha= 0.8) +
  labs(title="Distribuzione giorni di terapia condizionata a Fuo",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,7)), #wilcoU$p.value
       y="Densita", 
       x="Totale giorni",
       caption = "Dataset Niguarda",
       fill = 'Introduzione beta di glucano') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))




datimod %>%
  mutate(NEOFUO..retrospettiva. =  revalue(NEOFUO..retrospettiva., c("0"="No Fuo", "1"="Fuo")) ) %>% #muovi nomi colore
  ggplot( aes(x = NEOFUO..retrospettiva., y = TOT_GIORN, fill = NEOFUO..retrospettiva.)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot() +
  labs(title="Boxplot giorni di terapia condizionata a Fuo",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,7)),
       y="Totale giorni", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Periodo:') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))


# Periodo e giorni condizionato a fuo

testt<- t.test(TOT_GIORN~PERIODO_POSTBDG, data = datineofuo1, alternative = 'two.sided',   conf.level = .90)
testt
wilco <-  wilcox.test(TOT_GIORN~PERIODO_POSTBDG, data = datineofuo1)
wilco

ggplot(datineofuo1, aes(TOT_GIORN, fill = PERIODO_POSTBDG, col = PERIODO_POSTBDG)) +
  geom_density(alpha= .5) +
  ggtitle("Con fuo = 1 senza NA")
boxplot(TOT_GIORN ~PERIODO_POSTBDG, data = datineofuo1, main = "Con fuo = 1 senza NA" )

datineofuo1 %>%
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = TOT_GIORN, fill = PERIODO_POSTBDG)) +
  geom_density(alpha= 0.8) +
  labs(title="Distribuzione totale giorni condizionata a periodo beta di glucano dato Fuo",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,3)), #wilcoU$p.value
       y="Densita", 
       x="Totale giorni",
       caption = "Dataset Niguarda",
       fill = 'Introduzione beta di glucano') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))



datineofuo1 %>%
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = PERIODO_POSTBDG, y = TOT_GIORN, fill = PERIODO_POSTBDG)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot() +
  labs(title="Boxplot totale giorni condizionati a periodo beta di glucano dato Fuo",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,3)),
       y="Totale giorni", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Periodo:') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))



# Periodo e giorni condizionato a ifi = 0

testt<- t.test(TOT_GIORN~PERIODO_POSTBDG, data = datiifi0, alternative = 'two.sided',   conf.level = .90)
testt
wilco <-  wilcox.test(TOT_GIORN~PERIODO_POSTBDG, data = datiifi0)
wilco


datiifi0 %>%
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = TOT_GIORN, fill = PERIODO_POSTBDG)) +
  geom_density(alpha= 0.8) +
  labs(title="Distribuzione totale giorni condizionata a periodo beta di glucano dato Fuo",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,3)), #wilcoU$p.value
       y="Densita", 
       x="Totale giorni",
       caption = "Dataset Niguarda",
       fill = 'Introduzione beta di glucano') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))



datiifi0 %>%
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = PERIODO_POSTBDG, y = TOT_GIORN, fill = PERIODO_POSTBDG)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot() +
  labs(title="Boxplot totale giorni condizionati a periodo beta di glucano dato Fuo",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,3)),
       y="Totale giorni", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Periodo:') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))


# Periodo e giorni condizionato a ifi = 1

testt<- t.test(TOT_GIORN~PERIODO_POSTBDG, data = datiifi1, alternative = 'two.sided',   conf.level = .90)
testt
wilco <-  wilcox.test(TOT_GIORN~PERIODO_POSTBDG, data = datiifi1)
wilco


datiifi1 %>%
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Pre-BDG", "1"="Post-BDG")) ) %>% #muovi nomi colore
  ggplot( aes(x = TOT_GIORN, fill = PERIODO_POSTBDG)) +
  geom_density(alpha= 0.8) +
  labs(title="Distribuzione totale giorni condizionata a periodo beta di glucano dato Fuo",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,3)), #wilcoU$p.value
       y="Densita", 
       x="Totale giorni",
       caption = "Dataset Niguarda",
       fill = 'Periodo: ') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))



datiifi1 %>%
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Pre-BDG", "1"="Post-BDG")) ) %>% #muovi nomi colore
  ggplot( aes(x = PERIODO_POSTBDG, y = TOT_GIORN, fill = PERIODO_POSTBDG)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot() +
  labs(title="Boxplot totale giorni condizionati a periodo beta di glucano dato Fuo",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,3)),
       y="Totale giorni", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Periodo:') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))



# Periodo e giorni condizionato a fuo con NA = 0
testt <- t.test(TOT_GIORN~PERIODO_POSTBDG, data = datineofuo1mod, alternative = 'two.sided',   conf.level = .95)
testt
wilco <-  wilcox.test(TOT_GIORN~PERIODO_POSTBDG, data = datineofuo1mod)
wilco

ggplot(datineofuo1mod, aes(TOT_GIORN, fill = PERIODO_POSTBDG, col = PERIODO_POSTBDG)) +
  geom_density(alpha= .5) +
  ggtitle("Con fuo = 1 e NA = 0")
boxplot(TOT_GIORN ~PERIODO_POSTBDG, data = datineofuo1mod )

datineofuo1mod %>%
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = TOT_GIORN, fill = PERIODO_POSTBDG)) +
  geom_density(alpha= 0.8) +
  labs(title="Distribuzione totale giorni condizionata a periodo beta di glucano dato Fuo",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,3)), #wilcoU$p.value
       y="Densita", 
       x="Totale giorni",
       caption = "Dataset Niguarda",
       fill = 'Introduzione beta di glucano') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))



datineofuo1mod %>%
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = PERIODO_POSTBDG, y = TOT_GIORN, fill = PERIODO_POSTBDG)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot() +
  labs(title="Boxplot totale giorni condizionati a periodo beta di glucano dato Fuo",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,3)),
       y="Totale giorni", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Periodo:') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))


#condizionato a terapia tricomplessiva empirica =1 test periodo e giorni di terapia


testt <- t.test(TOT_GIORN~PERIODO_POSTBDG, data = datiemp, alternative = 'two.sided',   conf.level = .95)
testt
wilco <-  wilcox.test(TOT_GIORN~PERIODO_POSTBDG, data = datiemp)
wilco

ggplot(datiemp, aes(TOT_GIORN, fill = PERIODO_POSTBDG, col = PERIODO_POSTBDG)) +
  geom_density(alpha= .5)+
  ggtitle('Distribuzione giorni condizionata a periodo data terapia empirica ')
boxplot(TOT_GIORN ~PERIODO_POSTBDG, data = datiemp )

datiemp %>%
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = TOT_GIORN, fill = PERIODO_POSTBDG)) +
  geom_density(alpha= 0.8) +
  labs(title="Distribuzione totale giorni condizionata a periodo beta di glucano dato terapia tricomplessiva empirica = 1",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,3)), #wilcoU$p.value
       y="Densita", 
       x="Totale giorni",
       caption = "Dataset Niguarda",
       fill = 'Introduzione beta di glucano') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))



datiemp %>%
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = PERIODO_POSTBDG, y = TOT_GIORN, fill = PERIODO_POSTBDG)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot() +
  labs(title="Boxplot totale giorni condizionati a periodo beta di glucano  dato terapia tricomplessiva empirica = 1",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,3)),
       y="Totale giorni", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Periodo:') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))


#condizionato a terapia complessiva empirica =1 test periodo e giorni di terapia


testt <- t.test(TOT_GIORN~PERIODO_POSTBDG, data = datiter1, alternative = 'two.sided',   conf.level = .95)
testt
wilco <-  wilcox.test(TOT_GIORN~PERIODO_POSTBDG, data = datiter1)
wilco

ggplot(datiter1, aes(TOT_GIORN, fill = PERIODO_POSTBDG, col = PERIODO_POSTBDG)) +
  geom_density(alpha= .5)+
  ggtitle('Distribuzione giorni condizionata a periodo data terapia empirica ')
boxplot(TOT_GIORN ~PERIODO_POSTBDG, data = datiter1 )


datiter1 %>%
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = TOT_GIORN, fill = PERIODO_POSTBDG)) +
  geom_density(alpha= 0.8) +
  labs(title="Distribuzione totale giorni condizionata a periodo beta di glucano dato terapia complessiva empirica = 1",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,3)), #wilcoU$p.value
       y="Densita", 
       x="Totale giorni",
       caption = "Dataset Niguarda",
       fill = 'Introduzione beta di glucano') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))



datiter1 %>%
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = PERIODO_POSTBDG, y = TOT_GIORN, fill = PERIODO_POSTBDG)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot() +
  labs(title="Boxplot totale giorni condizionati a periodo beta di glucano  dato terapia complessiva empirica = 1",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,3)),
       y="Totale giorni", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Periodo:') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))


#condizionato a terapia complessiva empirica =2 test periodo e giorni di terapia

testt <- t.test(TOT_GIORN~PERIODO_POSTBDG, data = datiter2, alternative = 'two.sided',   conf.level = .95)
testt
wilco <-  wilcox.test(TOT_GIORN~PERIODO_POSTBDG, data = datiter2)
wilco

ggplot(datiter2, aes(TOT_GIORN, fill = PERIODO_POSTBDG, col = PERIODO_POSTBDG)) +
  geom_density(alpha= .5)+
  ggtitle('Distribuzione giorni condizionata a periodo data terapia empirica ')
boxplot(TOT_GIORN ~PERIODO_POSTBDG, data = datiter2 )


datiter2 %>%
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = TOT_GIORN, fill = PERIODO_POSTBDG)) +
  geom_density(alpha= 0.8) +
  labs(title="Distribuzione totale giorni condizionata a periodo beta di glucano dato terapia complessiva empirica = 2",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,3)), #wilcoU$p.value
       y="Densita", 
       x="Totale giorni",
       caption = "Dataset Niguarda",
       fill = 'Introduzione beta di glucano') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))



datiter2 %>%
  mutate(PERIODO_POSTBDG =  revalue(PERIODO_POSTBDG, c("0"="Ante", "1"="Post")) ) %>% #muovi nomi colore
  ggplot( aes(x = PERIODO_POSTBDG, y = TOT_GIORN, fill = PERIODO_POSTBDG)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot() +
  labs(title="Boxplot totale giorni condizionati a periodo beta di glucano  dato terapia complessiva empirica = 2",
       subtitle=paste("Il p-value del Wilcoxon test è: " , round(wilco$p.value,3)),
       y="Totale giorni", 
       x="",
       caption = "Dataset Niguarda",
       fill = 'Periodo:') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))



#Durata della terapia in base a BDG


ttest <- t.test(TOT_GIORN ~BDG_ALTER, data = dati, alternative = 'two.sided',   conf.level = .95)
ttest
wilco <-  wilcox.test(TOT_GIORN ~BDG_ALTER, data = dati)
wilco


dati %>%
  mutate(BDG_ALTER =  revalue(BDG_ALTER, c("0"="Negativo", "1"="Positivo")) ) %>% #muovi nomi colore
  drop_na(BDG_ALTER) %>% 
  ggplot( aes(x = TOT_GIORN, fill = BDG_ALTER)) +
  geom_density(alpha= 0.8) +
  labs(title="Distribuzione giorni di terapia condizionata all' esito BDG", #wilcoU$p.value
       y="Densita", 
       x="Totale giorni",
       caption = "Dataset Niguarda",
       fill = 'Esito test BDG:') +
  scale_fill_manual(values=c("#FD9200", "#12BDEF")) +
  theme_economist() +
  theme(
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text( face = "italic"),
    legend.title = element_text( size = 13, face = 'bold'),
    legend.text = element_text( size= 10))





#ANALISI SOPRAVVIVENZA####


#sopravvivenza per morti per infezioni fungine nei pazienti work up

survdiff(Surv(Follow.up_mesi, neooutcome) ~ PERIODO_POSTBDG, data = dati) #Accetto l'ipotesi nulla che nei due gruppi muoiono 
#in maniera simile

fit <- survfit(Surv(Follow.up_mesi, neooutcome) ~ PERIODO_POSTBDG, data = dati, conf.type = "log-log")
fit
summary(fit)
plot(fit,
      main = "Pazienti avviati work up", col = c('red', 'blue'),
      ylab = "Probability", xlab = "Survival Time in Month",
       ylim = c(0.95, 1), xlim = c(0, 6), lwd = 1.5
      )
legend('bottomright', legend=c("Pre", "Post"),
       col=c("red", "blue"), lty = 1, cex=1, lwd = 1.5)


#sopravvivenza per morti per qualsiasi causa nei pazienti work up



survdiff(Surv(Follow.up_mesi, Deceduto.entro.tot.mesi) ~ PERIODO_POSTBDG, data = datimorti) #Accetto l'ipotesi nulla che nei due gruppi muoiono 
#in maniera simile
dati$Deceduto.entro.tot.mesi
fit <- survfit(Surv(Follow.up_mesi, Deceduto.entro.tot.mesi) ~ PERIODO_POSTBDG, data = dati, conf.type = "log-log")
fit
summary(fit)
plot(fit,
     main = "Pazienti avviati work up", col = c('red', 'blue'),
     ylab = "Probability", xlab = "Survival Time in Month",
     ylim = c(0.7, 1), xlim = c(0, 6), lwd = 1.5
)
legend('bottomright', legend=c("Pre", "Post"),
       col=c("red", "blue"), lty = 1, cex=1, lwd = 1.5)


#sopravvivenza in relazione a IFI

survdiff(Surv(Follow.up_mesi, neooutcome) ~ IFI, data = dati)
fit <- survfit(Surv(Follow.up_mesi, neooutcome) ~ IFI, data = dati, conf.type = "log-log")
fit
plot(fit,
     main = "Pazienti avviati work up", col = c('red', 'blue', 'darkgreen', 'orange'),
     ylab = "Probability", xlab = "Survival Time in Month",
     ylim = c(0.55, 1), xlim = c(0, 6), lwd = 1.5
)
legend('bottomright', legend=c("No IFI", "IFI possibile", 'IFI probabile', 'IFI provata'),
       col = c('red', 'blue', 'darkgreen', 'orange'), lty = 1, cex=1, lwd = 1.5)



#sopravvivenza in generale relazione a IFI

survdiff(Surv(Follow.up_mesi, Deceduto.entro.tot.mesi) ~ IFI, data = datimorti) #Accetto l'ipotesi nulla che nei due gruppi muoiono 
#in maniera simile

fit <- survfit(Surv(Follow.up_mesi, Deceduto.entro.tot.mesi) ~ IFI, data = dati, conf.type = "log-log")
fit
summary(fit)
plot(fit,
     main = "Pazienti avviati work up", col = c('red', 'blue', 'darkgreen', 'orange'),
     ylab = "Probability", xlab = "Survival Time in Month",
     ylim = c(0.55, 1), xlim = c(0, 6), lwd = 1.5
)
legend('bottomright', legend=c("No IFI", "IFI possibile", 'IFI probabile', 'IFI provata'),
       col = c('red', 'blue', 'darkgreen', 'orange'), lty = 1, cex=1, lwd = 1.5)


#sopravivvenza condizionata ad IFI


survdiff(Surv(Follow.up_mesi, neooutcome) ~ PERIODO_POSTBDG, data = datisiifi)
fit <- survfit(Surv(Follow.up_mesi, neooutcome) ~ PERIODO_POSTBDG, data = datisiifi, conf.type = "log-log")
fit
plot(fit,
     main = "Pazienti IFI", col = c('red', 'blue'),
     ylab = "Probability", xlab = "Survival Time in Month",
     ylim = c(0.8, 1), xlim = c(0, 6), lwd = 1.5
)
legend('bottomright', legend=c("Pre", "Post"),
       col=c("red", "blue"), lty = 1, cex=1, lwd = 1.5)

par(mfrow = c(2, 2))
#sopravvivenza per morti per infezioni fungine nei pazienti work up

survdiff(Surv(Follow.up_mesi, neooutcome) ~ PERIODO_POSTBDG, data = dati) #Accetto l'ipotesi nulla che nei due gruppi muoiono 
#in maniera simile

fit <- survfit(Surv(Follow.up_mesi, neooutcome) ~ PERIODO_POSTBDG, data = dati, conf.type = "log-log")
fit
summary(fit)
plot(fit,
     main = "Pazienti avviati al work-up", col = c('red', 'blue'),
     ylab = "Probability", xlab = "Survival Time in Month",
     ylim = c(0.95, 1), xlim = c(0, 6), lwd = 1.5
)
legend('bottomright', legend=c("Pre", "Post"),
       col=c("red", "blue"), lty = 1, cex=1, lwd = 1.5)


#sopravivvenza condizionata ad IFI=1

survdiff(Surv(Follow.up_mesi, neooutcome) ~ PERIODO_POSTBDG, data = datiifi1)
fit <- survfit(Surv(Follow.up_mesi, neooutcome) ~ PERIODO_POSTBDG, data = datiifi1, conf.type = "log-log")
fit
plot(fit,
     main = "Pazienti IFI possibile", col = c('red', 'blue'),
     ylab = "Probability", xlab = "Survival Time in Month",
     ylim = c(0.8, 1), xlim = c(0, 6), lwd = 1.5
)
legend('bottomright', legend=c("Pre", "Post"),
       col=c("red", "blue"), lty = 1, cex=1, lwd = 1.5)


#sopravivvenza condizionata ad IFI=2

survdiff(Surv(Follow.up_mesi, neooutcome) ~ PERIODO_POSTBDG, data = datiifi2)
fit <- survfit(Surv(Follow.up_mesi, neooutcome) ~ PERIODO_POSTBDG, data = datiifi2, conf.type = "log-log")
fit
plot(fit,
     main = "Pazienti IFI probabile", col = c('red', 'blue'),
     ylab = "Probability", xlab = "Survival Time in Month",
     ylim = c(0.5, 1), xlim = c(0, 6), lwd = 1.5
)
legend('bottomright', legend=c("Pre", "Post"),
       col=c("red", "blue"), lty = 1, cex=1, lwd = 1.5)

#sopravivvenza condizionata ad IFI=3


survdiff(Surv(Follow.up_mesi, neooutcome) ~ PERIODO_POSTBDG, data = datiifi3)
fit <- survfit(Surv(Follow.up_mesi, neooutcome) ~ PERIODO_POSTBDG, data = datiifi3, conf.type = "log-log")
fit
plot(fit,
     main = "Pazienti IFI provata", col = c('red', 'blue'),
     ylab = "Probability", xlab = "Survival Time in Month",
     ylim = c(0.5, 1), xlim = c(0, 6), lwd = 1.5
)
legend('bottomright', legend=c("Pre", "Post"),
       col=c("red", "blue"), lty = 1, cex=1, lwd = 1.5)
par(mfrow = c(1, 1))




par(mfrow = c(2, 2))


#sopravivvenza condizionata ad IFI=0

survdiff(Surv(Follow.up_mesi, neooutcome) ~ PERIODO_POSTBDG, data = datiifi0)
fit <- survfit(Surv(Follow.up_mesi, neooutcome) ~ PERIODO_POSTBDG, data = datiifi0, conf.type = "log-log")
fit
plot(fit,
     main = "Pazienti No IFI", col = c('red', 'blue'),
     ylab = "Probability", xlab = "Survival Time in Month",
     ylim = c(0.8, 1), xlim = c(0, 6), lwd = 1.5
)
legend('bottomright', legend=c("Pre", "Post"),
       col=c("red", "blue"), lty = 1, cex=1, lwd = 1.5)



#sopravivvenza condizionata ad IFI=1

survdiff(Surv(Follow.up_mesi, neooutcome) ~ PERIODO_POSTBDG, data = datiifi1)
fit <- survfit(Surv(Follow.up_mesi, neooutcome) ~ PERIODO_POSTBDG, data = datiifi1, conf.type = "log-log")
fit
plot(fit,
     main = "Pazienti IFI possibile", col = c('red', 'blue'),
     ylab = "Probability", xlab = "Survival Time in Month",
     ylim = c(0.8, 1), xlim = c(0, 6), lwd = 1.5
)
legend('bottomright', legend=c("Pre", "Post"),
       col=c("red", "blue"), lty = 1, cex=1, lwd = 1.5)


#sopravivvenza condizionata ad IFI=2

survdiff(Surv(Follow.up_mesi, neooutcome) ~ PERIODO_POSTBDG, data = datiifi2)
fit <- survfit(Surv(Follow.up_mesi, neooutcome) ~ PERIODO_POSTBDG, data = datiifi2, conf.type = "log-log")
fit
plot(fit,
     main = "Pazienti IFI probabile", col = c('red', 'blue'),
     ylab = "Probability", xlab = "Survival Time in Month",
     ylim = c(0.5, 1), xlim = c(0, 6), lwd = 1.5
)
legend('bottomright', legend=c("Pre", "Post"),
       col=c("red", "blue"), lty = 1, cex=1, lwd = 1.5)

#sopravivvenza condizionata ad IFI=3


survdiff(Surv(Follow.up_mesi, neooutcome) ~ PERIODO_POSTBDG, data = datiifi3)
fit <- survfit(Surv(Follow.up_mesi, neooutcome) ~ PERIODO_POSTBDG, data = datiifi3, conf.type = "log-log")
fit
plot(fit,
     main = "Pazienti IFI provata", col = c('red', 'blue'),
     ylab = "Probability", xlab = "Survival Time in Month",
     ylim = c(0.5, 1), xlim = c(0, 6), lwd = 1.5
)
legend('bottomright', legend=c("Pre", "Post"),
       col=c("red", "blue"), lty = 1, cex=1, lwd = 1.5)
par(mfrow = c(1, 1))


#sopravvivenze per approcci terapeutici
dati$TERAPIA_COMPLESSIVA
survdiff(Surv(Follow.up_mesi, neooutcome) ~ TERAPIA_COMPLESSIVA, data = dati)
fit <- survfit(Surv(Follow.up_mesi, neooutcome) ~ TERAPIA_COMPLESSIVA, data = dati, conf.type = "log-log")
fit
plot(fit,
     main = "Pazienti avviati work up", col = c('red', 'blue', 'darkgreen', 'orange'),
     ylab = "Probability", xlab = "Survival Time in Month",
     ylim = c(0.55, 1), xlim = c(0, 6), lwd = 1.5, lty = c(1,2,1,1)
)
legend('bottomright', legend=c("No Terapia", "Terapia empirica", 'terapia pre-emptive', 'Terapia mirata'),
       col = c('red', 'blue', 'darkgreen', 'orange'), lty = c(1,2,1,1), cex=1, lwd = 1.5)


#sopravivvenza condizionata ad approcci terapeutici

#par(mfrow = c(1, 2))

#Terapia pre emptive



survdiff(Surv(Follow.up_mesi, neooutcome) ~ PERIODO_POSTBDG, data = datiter2)
fit <- survfit(Surv(Follow.up_mesi, neooutcome) ~ PERIODO_POSTBDG, data = datiter2, conf.type = "log-log")
fit
plot(fit,
     main = "Terapia pre emptive", col = c('red', 'blue'),
     ylab = "Probability", xlab = "Survival Time in Month",
     ylim = c(0.8, 1), xlim = c(0, 6), lwd = 1.5
)
legend('bottomright', legend=c("Pre", "Post"),
       col=c("red", "blue"), lty = 1, cex=1, lwd = 1.5)


#Terapia mirata

survdiff(Surv(Follow.up_mesi, neooutcome) ~ PERIODO_POSTBDG, data = datiter3)
fit <- survfit(Surv(Follow.up_mesi, neooutcome) ~ PERIODO_POSTBDG, data = datiter3, conf.type = "log-log")
fit
plot(fit,
     main = "Terapia mirata", col = c('red', 'blue'),
     ylab = "Probability", xlab = "Survival Time in Month",
     ylim = c(0.6, 1), xlim = c(0, 6), lwd = 1.5
)
legend('bottomright', legend=c("Pre", "Post"),
       col=c("red", "blue"), lty = 1, cex=1, lwd = 1.5)



#sopravvivenza per bdg alter

survdiff(Surv(Follow.up_mesi, neooutcome) ~ BDG_ALTER, data = dati) #Accetto l'ipotesi nulla che nei due gruppi muoiono 
#in maniera simile

fit <- survfit(Surv(Follow.up_mesi, neooutcome) ~ BDG_ALTER, data = dati, conf.type = "log-log")
fit
summary(fit)
plot(fit,
     main = "Pazienti avviati work up", col = c('red', 'blue'),
     ylab = "Probability", xlab = "Survival Time in Month",
     ylim = c(0.73, 1), xlim = c(0, 6), lwd = 1.5
)
legend('bottomright', legend=c("Non alterato", "Alterato"),
       col=c("red", "blue"), lty = 1, cex=1, lwd = 1.5)





#REGRESSIONE###
model <- glm(PERIODO_POSTBDG ~IFI,family=binomial(link='probit'),data=dati)
summary(model)


# TERAPIA STORICA####
#cose di Puz
terminata <- as.POSIXct(dati$STOP_TERTOT, format = '%d/%m/%Y')
dataesame <- as.POSIXct(dati$Data_BDG_1, format = '%d/%m/%Y')
tempodopoesame1 <- difftime(terminata, dataesame, units = 'days')
dataesame <- as.POSIXct(dati$Data_BDG_2, format = '%d/%m/%Y')
tempodopoesame2 <- difftime(terminata, dataesame, units = 'days')
dataesame <- as.POSIXct(dati$Data_BDG_3, format = '%d/%m/%Y')
tempodopoesame3 <- difftime(terminata, dataesame, units = 'days')
dataesame <- as.POSIXct(dati$Data_BDG_4, format = '%d/%m/%Y')
tempodopoesame4 <- difftime(terminata, dataesame, units = 'days')
dataesame <- as.POSIXct(dati$Data_BDG_5, format = '%d/%m/%Y')
tempodopoesame5 <- difftime(terminata, dataesame, units = 'days')


tempopuz <- vector('numeric', length = 469)
indicepuz <- (!is.na(tempodopoesame1)) %>%  which()
bdgval<- vector('numeric', length = 469)
for (i in indicepuz) {
  posi <- confronto <- c(tempodopoesame1[i], tempodopoesame2[i], tempodopoesame3[i], tempodopoesame4[i], tempodopoesame5[i])
  confronto <- confronto[!is.na(confronto)]
  confronto <- confronto[confronto >= 0]
  tempopuz[i] <- min(confronto)
  bdgind  <- which(posi==tempopuz[i])
  bdgval[i] <- c(dati$Valore_BDG_1[i], dati$Valore_BDG_2[i], dati$Valore_BDG_3[i], dati$Valore_BDG_4[i], dati$Valore_BDG_5[i] )[bdgind]
 
}

tempopuz[is.na(tempodopoesame1)] <- NA
tempopuznona <- tempopuz[!is.na(tempopuz)]
bdgvalnona <- bdgval[bdgval != 0]
 

finefollowup <- as.POSIXct(dati$Fine_follow_up, format = '%d/%m/%Y')
morti <- difftime(finefollowup[indicepuz], terminata[indicepuz], units = 'days')
bdgvalnona <-  bdgvalnona[morti >1]
tempopuznona <-tempopuznona[morti > 1]
table(tempopuznona <= 5)
table(tempopuznona[bdgvalnona < 11] <= 5) # Esito negativo
table(tempopuznona[bdgvalnona >= 11] <= 5) #Esito positivo

#Analisi

esito <- ifelse(bdgvalnona < 11, 'Negativo', 'Positivo')
esamevicino <- data.frame(esito, tempodopoesame = as.numeric(tempopuznona), bdgvalnona)
t.test(tempodopoesame~esito, data = esamevicino, alternative = 'two.sided',   conf.level = .95)
ggplot(esamevicino, aes(tempodopoesame, fill = esito, col = esito)) +
  geom_density(alpha = .5)
ggplot(esamevicino, aes(tempodopoesame, fill = esito, col = esito)) +
  geom_histogram(aes(y=..density..), alpha = .5) +
  geom_density(alpha = .2)
boxplot(tempodopoesame ~esito, data = esamevicino )

#Aspergillo

terminata <- as.POSIXct(dati$STOP_TERTOT, format = '%d/%m/%Y')
dataesame <- as.POSIXct(dati$Data_ASAG_1, format = '%d/%m/%Y')
tempodopoesame1 <- difftime(terminata, dataesame, units = 'days')
dataesame <- as.POSIXct(dati$Data_ASAG_2, format = '%d/%m/%Y')
tempodopoesame2 <- difftime(terminata, dataesame, units = 'days')
dataesame <- as.POSIXct(dati$Data_ASAG_3, format = '%d/%m/%Y')
tempodopoesame3 <- difftime(terminata, dataesame, units = 'days')
dataesame <- as.POSIXct(dati$Data_ASAG_4, format = '%d/%m/%Y')
tempodopoesame4 <- difftime(terminata, dataesame, units = 'days')
dataesame <- as.POSIXct(dati$Data_ASAG_5, format = '%d/%m/%Y')
tempodopoesame5 <- difftime(terminata, dataesame, units = 'days')
dataesame <- as.POSIXct(dati$Data_ASAG_6, format = '%d/%m/%Y')
tempodopoesame6 <- difftime(terminata, dataesame, units = 'days')
dataesame <- as.POSIXct(dati$Data_ASAG_7, format = '%d/%m/%Y')
tempodopoesame7 <- difftime(terminata, dataesame, units = 'days')
dataesame <- as.POSIXct(dati$Data_ASAG_8, format = '%d/%m/%Y')
tempodopoesame8 <- difftime(terminata, dataesame, units = 'days')
dataesame <- as.POSIXct(dati$Data_ASAG_9, format = '%d/%m/%Y')
tempodopoesame9 <- difftime(terminata, dataesame, units = 'days')
dataesame <- as.POSIXct(dati$Data_ASAG_10, format = '%d/%m/%Y')
tempodopoesame10 <- difftime(terminata, dataesame, units = 'days')
dataesame <- as.POSIXct(dati$Data_ASAG_11, format = '%d/%m/%Y')
tempodopoesame11 <- difftime(terminata, dataesame, units = 'days')
dataesame <- as.POSIXct(dati$Data_ASAG_12, format = '%d/%m/%Y')
tempodopoesame12 <- difftime(terminata, dataesame, units = 'days')
dataesame <- as.POSIXct(dati$Data_ASAG_13, format = '%d/%m/%Y')
tempodopoesame13 <- difftime(terminata, dataesame, units = 'days')
dataesame <- as.POSIXct(dati$Data_ASAG_14, format = '%d/%m/%Y')
tempodopoesame14 <- difftime(terminata, dataesame, units = 'days')
dataesame <- as.POSIXct(dati$Data_ASAG_15, format = '%d/%m/%Y')
tempodopoesame15 <- difftime(terminata, dataesame, units = 'days')





tempopuz <- vector('numeric', length = 469)
indicepuz <- (!is.na(tempodopoesame1)) %>%  which()
asagval<- vector('numeric', length = 469)
for (i in indicepuz) {
  posi <- confronto <- c(tempodopoesame1[i], tempodopoesame2[i], tempodopoesame3[i], tempodopoesame4[i], tempodopoesame5[i], 
                         tempodopoesame6[i], tempodopoesame7[i], tempodopoesame8[i], tempodopoesame9[i], tempodopoesame10[i],
                         tempodopoesame11[i], tempodopoesame12[i], tempodopoesame13[i], tempodopoesame14[i], tempodopoesame15[i])
  confronto <- confronto[!is.na(confronto)]
  confronto <- confronto[confronto >= 0]
  tempopuz[i] <- min(confronto)
  asagind  <- which(posi==tempopuz[i])
  asagval[i] <- c(dati$VAL_ASAG_1[i], dati$VAL_ASAG_2[i], dati$VAL_ASAG_3[i], dati$VAL_ASAG_3[i], dati$VAL_ASAG_4[i], dati$VAL_ASAG_5[i],
                  dati$VAL_ASAG_5[i], dati$VAL_ASAG_6[i], dati$VAL_ASAG_7[i], dati$VAL_ASAG_8[i], dati$VAL_ASAG_9[i], dati$VAL_ASAG_10[i],
                  dati$VAL_ASAG_11[i], dati$VAL_ASAG_12[i], dati$VAL_ASAG_13[i], dati$VAL_ASAG_14[i], dati$VAL_ASAG_15[i])[asagind]
  
}

tempopuz[is.na(tempodopoesame1)] <- NA
tempopuznona <- tempopuz[!is.na(tempopuz)]
asagvalnona <- asagval[asagval != 0]


finefollowup <- as.POSIXct(dati$Fine_follow_up, format = '%d/%m/%Y')
morti <- difftime(finefollowup[indicepuz], terminata[indicepuz], units = 'days')
asagvalnona <-  asagvalnona[morti >1]
tempopuznona <-tempopuznona[morti > 1]
table(tempopuznona <= 5)
table(tempopuznona[asagvalnona < 0.5] <= 5) # Esito negativo
table(tempopuznona[asagvalnona >= 0.5] <= 5) #Esito positivo
#Analisi
esito <- ifelse(asagvalnona < 0.5, 'Negativo', 'Positivo')
esamevicinoas <- data.frame(esito, tempodopoesame = as.numeric(tempopuznona), asagvalnona)
t.test(tempodopoesame~esito, data = esamevicinoas, alternative = 'two.sided',   conf.level = .95)
ggplot(esamevicinoas, aes(tempodopoesame, fill = esito, col = esito)) +
  geom_density(alpha = .5)
ggplot(esamevicinoas, aes(tempodopoesame, fill = esito, col = esito)) +
  geom_histogram(aes(y=..density..), alpha = .5) +
  geom_density(alpha = .2)
boxplot(tempodopoesame ~esito, data = esamevicinoas )


#Bdg1

terminata <- as.POSIXct(dati$STOP_TERTOT, format = '%d/%m/%Y')
dataesame <- as.POSIXct(dati$Data_BDG_1, format = '%d/%m/%Y')
tempodopoesame <- difftime(terminata, dataesame, units = 'days')
esito <- ifelse(dati$Valore_BDG_1<11, 'Negativo', 'Positivo')
esamebdg1 <- data.frame(esito, tempodopoesame = as.numeric(tempodopoesame))
esamebdg1$tempodopoesame %>%  str()
t.test(tempodopoesame~esito, data = esamebdg1, alternative = 'two.sided',   conf.level = .95)
ggplot(esamebdg1, aes(tempodopoesame, fill = esito, col = esito)) +
  geom_density(alpha = .5)
ggplot(esamebdg1, aes(tempodopoesame, fill = esito, col = esito)) +
  geom_histogram(aes(y=..density..), alpha = .5) +
  geom_density(alpha = .2)
boxplot(tempodopoesame ~esito, data = esamebdg1 )

#Bdg2
dataesame <- as.POSIXct(dati$Data_BDG_2, format = '%d/%m/%Y')
tempodopoesame <- difftime(terminata, dataesame, units = 'days')
esito <- ifelse(dati$Valore_BDG_2 < 11, 'Negativo', 'Positivo')
esamebdg2 <- data.frame(esito, tempodopoesame = as.numeric(tempodopoesame))
t.test(tempodopoesame~esito, data = esamebdg2, alternative = 'two.sided',   conf.level = .95)
ggplot(esamebdg2, aes(tempodopoesame, fill = esito, col = esito)) +
  geom_histogram(aes(y=..density..), alpha = .5) +
  geom_density(alpha = .2)
ggplot(esamebdg2, aes(tempodopoesame, fill = esito, col = esito)) +
  geom_density(alpha = .5)
boxplot(tempodopoesame ~esito, data = esamebdg2 )
which(tempodopoesame<0)
dati[9, c('Data_BDG_3', 'STOP_TERTOT')]

#bdg3

dataesame <- as.POSIXct(dati$Data_BDG_3, format = '%d/%m/%Y')
tempodopoesame <- difftime(terminata, dataesame, units = 'days')
esito <- ifelse(dati$Valore_BDG_3 < 11, 'Negativo', 'Positivo')
esamebdg3 <- data.frame(esito, tempodopoesame = as.numeric(tempodopoesame))
esamebdg3$tempodopoesame %>%  str()
t.test(tempodopoesame~esito, data = esamebdg3, alternative = 'two.sided',   conf.level = .95)
ggplot(esamebdg3, aes(tempodopoesame, fill = esito, col = esito)) +
  geom_histogram(aes(y=..density..), alpha = .5) +
  geom_density(alpha = .2)
ggplot(esamebdg3, aes(tempodopoesame, fill = esito, col = esito)) +
  geom_density(alpha = .5)
boxplot(tempodopoesame ~esito, data = esamebdg3 )



#bdg4
dataesame <- as.POSIXct(dati$Data_BDG_4, format = '%d/%m/%Y')
tempodopoesame <- difftime(terminata, dataesame, units = 'days')
esito <- ifelse(dati$Valore_BDG_4 < 11, 'Negativo', 'Positivo')
esamebdg4 <- data.frame(esito, tempodopoesame = as.numeric(tempodopoesame))
t.test(tempodopoesame~esito, data = esamebdg4, alternative = 'two.sided',   conf.level = .95)
ggplot(esamebdg4, aes(tempodopoesame, fill = esito, col = esito)) +
  geom_histogram(aes(y=..density..), alpha = .5) +
  geom_density(alpha = .2)
ggplot(esamebdg4, aes(tempodopoesame, fill = esito, col = esito)) +
  geom_density(alpha = .5)
boxplot(tempodopoesame ~esito, data = esamebdg4 )

#bdg5
dataesame <- as.POSIXct(dati$Data_BDG_5, format = '%d/%m/%Y')
tempodopoesame <- difftime(terminata, dataesame, units = 'days')
esito <- ifelse(dati$Valore_BDG_5 < 11, 'Negativo', 'Positivo')
esamebdg5 <- data.frame(esito, tempodopoesame = as.numeric(tempodopoesame))
t.test(tempodopoesame~esito, data = esamebdg5, alternative = 'two.sided',   conf.level = .95)
ggplot(esamebdg5, aes(tempodopoesame, fill = esito, col = esito)) +
  geom_histogram(aes(y=..density..), alpha = .5) +
  geom_density(alpha = .2)
ggplot(esamebdg5, aes(tempodopoesame, fill = esito, col = esito)) +
  geom_density(alpha = .5)
boxplot(tempodopoesame ~esito, data = esamebdg5 )


#Previsioni#########


nuove <- names(dati) %in% c('ASAG.INIZIO_TERAPIA', 'ASAG.FINE_TERAPIA', 'serie' )
neodati <- dati[,!nuove]
neodati$INFEZIONE.FUNGINA.PROBABILE
elimino <- names(neodati) [ is.na(neodati) %>% colSums() > 0]
dati$Days_Fluconazolo
as.factor(neodati$DOPPIO_BDG_FATTO)
elimino
pulito <- neodati[! names(neodati) %in% elimino]

#niente: BDG_per_diagnosi, BDG_ALTER, DOPPIO_BDG_ALTER, ALTR_ASAG_OLTRE, S_ASAG_OLTRE, Isolamento_contaminato_1, Ife_aspergillari,
#Terapia_Mirata, Abelcet, Ambisome, Cancidas, "Isavuconazolo", "Voriconazolo", "Fluconazolo", Amfotericina


# 0: Valore_altri_ASAG, FUNG_ISOL_VIVO, Isolamento_sito_sterile, ISOLAMENTO_BAL, Inizio_profilassi_prima_dell.evento,
# Days_ABELCET, Days_Ambisome, Days_Cancidas, Days_Isavuconazolo, Days_Voriconazolo, Days_Fluconazolo, TOT_GIORN, ASPER_INIZIO.TER,
#BDG_INIZIO.TER, BDG_INIZIO.TER, ISOL_INIZIO.TER, FINE.TERAPIA_FOLLOW.UP, BDG.INIZIO_TERAPIA, BDG.FINE_TERAPIA, FUNG_ISOL 
#"Valore_BDG_1", "Valore_BDG_2", "Valore_BDG_3", "Valore_BDG_4", "Valore_BDG_5", "BDG_ALTER", "DOPPIO_BDG_ALTER", "Pneumocistis"
#"VAL_ASAG_1", "VAL_ASAG_2", "VAL_ASAG_3", "VAL_ASAG_4", "VAL_ASAG_5", "VAL_ASAG_6", "VAL_ASAG_7", "VAL_ASAG_8", "VAL_ASAG_9"
#"VAL_ASAG_10", "VAL_ASAG_11", "VAL_ASAG_12", "VAL_ASAG_13", "VAL_ASAG_14", "VAL_ASAG_15"  


niente <- c('BDG_per_diagnosi', 'BDG_ALTER', 'DOPPIO_BDG_ALTER', 'ALTR_ASAG_OLTRE', 'S_ASAG_OLTRE', 'Isolamento_contaminato_1', 
            'Ife_aspergillari', 'Terapia_Mirata', 'Abelcet', 'Ambisome', 'Cancidas', "Isavuconazolo", "Voriconazolo", "Fluconazolo", 'Amfotericina')
datnient <- neodati[, niente]
datnient <- data.frame(lapply(datnient, as.character), stringsAsFactors=FALSE)
datnient[is.na(datnient)] <- 'Niente'
datnient <- data.frame(lapply(datnient, as.factor), stringsAsFactors=TRUE)

zero <- c('Valore_altri_ASAG', 'FUNG_ISOL_VIVO', 'Isolamento_sito_sterile', 'ISOLAMENTO_BAL', 'Inizio_profilassi_prima_dell.evento',
           'Days_ABELCET', 'Days_Ambisome', 'Days_Cancidas', 'Days_Isavuconazolo', 'Days_Voriconazolo', 'Days_Fluconazolo', 'TOT_GIORN', 'ASPER_INIZIO.TER',
          'BDG_INIZIO.TER', 'BDG_INIZIO.TER', 'ISOL_INIZIO.TER', 'FINE.TERAPIA_FOLLOW.UP', 'BDG.INIZIO_TERAPIA', 'BDG.FINE_TERAPIA', 'FUNG_ISOL', 
          "Valore_BDG_1", "Valore_BDG_2", "Valore_BDG_3", "Valore_BDG_4", "Valore_BDG_5", "BDG_ALTER", "DOPPIO_BDG_ALTER", "Pneumocistis",
          "VAL_ASAG_1", "VAL_ASAG_2", "VAL_ASAG_3", "VAL_ASAG_4", "VAL_ASAG_5", "VAL_ASAG_6", "VAL_ASAG_7", "VAL_ASAG_8", "VAL_ASAG_9",
          "VAL_ASAG_10", "VAL_ASAG_11", "VAL_ASAG_12", "VAL_ASAG_13", "VAL_ASAG_14", "VAL_ASAG_15"  )
datizero <- neodati[, zero]
datizero[is.na(datizero)] <- 0 
finito <- data.frame(pulito, datnient, datizero)
# Data_BDG_1, Data_BDG_2, Data_BDG_3, Data_BDG_4, Data_BDG_5, Data_ASAG_1, Data_ASAG_2, Data_ASAG_3, Data_ASAG_4, Data_ASAG_5, Data_ASAG_6,
# Data_ASAG_7, Data_ASAG_8, Data_ASAG_9, Data_ASAG_10, Data_ASAG_11, Data_ASAG_12, Data_ASAG_13, Data_ASAG_14, Data_ASAG_15, Motivo_BDG_1,
#Inizio_profilassi, Fine_profilassi, Nistatina_inizio, Nistatina_fine, SDO, Altro_2, Name, Date_of_birth, Patologia.ematologica..esteso., 
#Motivo.per.terapia.antifungina,  Motivo.per.esecuzione.Beta.D.glucano, INI_ABELCET, STOP_ABELCET, INI_Ambisome, STOP_Ambisome, INI_Cancidas,
#STOP_Cancidas, INI_Isavuconazolo, STOP_Isavuconazolo, INI_Voriconazolo, STOP_Voriconazolo, INI_Fluconazolo, STOP_Fluconazolo, INI_TERTOT, 
#STOP_TERTOT, Prima.data.AG.ASPERGILLARE, Prima.data.BDG, Prima.data.ISOLAMENTO,Fine_follow_up, SURNAME1, SURNAME2, SURNAME3, NUMERO_NON_MATRICE,
#NUMERO_MATRICE
finito <- finito[, !names(finito) %in% 
    c( 'Data_BDG_1', 'Data_BDG_2', 'Data_BDG_3', 'Data_BDG_4', 'Data_BDG_5', 'Data_ASAG_1', 'Data_ASAG_2', 'Data_ASAG_3', 'Data_ASAG_4', 'Data_ASAG_5', 'Data_ASAG_6',
   'Data_ASAG_7', 'Data_ASAG_8', 'Data_ASAG_9', 'Data_ASAG_10', 'Data_ASAG_11', 'Data_ASAG_12', 'Data_ASAG_13', 'Data_ASAG_14', 'Data_ASAG_15', 'Motivo_BDG_1',
  'Inizio_profilassi', 'Fine_profilassi', 'Nistatina_inizio', 'Nistatina_fine', 'SDO', 'Altro_2', 'Name', 'Date_of_birth', 'Patologia.ematologica..esteso.', 
  'Motivo.per.terapia.antifungina',  'Motivo.per.esecuzione.Beta.D.glucano', 'INI_ABELCET', 'STOP_ABELCET', 'INI_Ambisome', 'STOP_Ambisome', 'INI_Cancidas',
  'STOP_Cancidas', 'INI_Isavuconazolo', 'STOP_Isavuconazolo', 'INI_Voriconazolo', 'STOP_Voriconazolo', 'INI_Fluconazolo', 'STOP_Fluconazolo', 'INI_TERTOT', 
  'STOP_TERTOT', 'Prima.data.AG.ASPERGILLARE', 'Prima.data.BDG', 'Prima.data.ISOLAMENTO','Fine_follow_up', 'SURNAME1', 'SURNAME2', 'SURNAME3', 'NUMERO_NON_MATRICE',
  'NUMERO_MATRICE', 'DATA_ISOL', 'Motivo_terapia_1', 'SURNAME', 'Data_inizio', 'ID_EPISODIO', 'ID_PAZIENTE', 'Fungo_autopsia', 'OUTCOME', 
  'OUTCOME_BINARIO', 'INFEZIONE.FUNGINA.CERTA', 'sicurimalati')]
finito$INFEZIONE.FUNGINA.PROBABILE <- as.factor(ifelse(finito$INFEZIONE.FUNGINA.PROBABILE == 1, 'Probabili malati', 'Sani'))

# randomForest(INFEZIONE.FUNGINA.PROBABILE ~., data = finito, ntree = 50 )
# train_control <- trainControl(method="LOOCV", savePredictions="final")
# fitto <- train(INFEZIONE.FUNGINA.PROBABILE ~., data = finito,
#                method = 'ranger', trControl = train_control)
# # is.infinite(finito) %>% table()
# # dai <- lapply(finito, is.infinite)
# # lapply(dai, table)
# gridcaret <-  expand.grid( n.trees = 150 ,
#                            interaction.depth = 3,
#                            shrinkage = 0.1,
#                            n.minobsinnode= 10
# )
# 
# modello <- train(INFEZIONE.FUNGINA.PROBABILE ~., data = finito,
#                  method = 'gbm',
#                  trControl = train_control,
#                  verbose = FALSE,
#                  tuneGrid = gridcaret
# )
# 
# finito$INFEZIONE.FUNGINA.PROBABILE <- as.factor(ifelse(finito$INFEZIONE.FUNGINA.PROBABILE == 1, 'Probabili malati', 'Sani'))
# 
# prev <- modello$pred
# prev <- prev[order(prev$rowIndex),]
# confusionMatrix(prev$pred, finito$INFEZIONE.FUNGINA.PROBABILE)
# ifi2e3 <- ifelse(finito$IFI==2 | finito$IFI ==3, 1, 0)
# confusionMatrix(as.factor(ifi2e3), finito$INFEZIONE.FUNGINA.PROBABILE)

