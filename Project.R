
#Installation
install.packages("pdftools")
install.packages("tm")
install.packages("SnowballC")
install.packages("reshape")

#Importation
library(pdftools)
library("tm")
library("SnowballC")
library(dplyr)
library(reshape2)

specialchars <- content_transformer(function(x) gsub("[^[:alnum:]///']", " ", x))

#########################################################################
RM <- pdf_text("Regression multiple.pdf")
ACP <- pdf_text("Analyse en Composantes Principales.pdf")
AFC1 <- pdf_text("AFC1.pdf")
AFC2 <- pdf_text("AFC2.pdf")
ACM <- pdf_text("Analyse des correspondances multiples.pdf")
AD <- pdf_text("analyse discriminante.pdf")
C <- pdf_text("Classification.pdf")

#########################################################################
RM <- Corpus(VectorSource(RM))
ACP <- Corpus(VectorSource(ACP))
AFC1 <- Corpus(VectorSource(AFC1))
AFC2 <- Corpus(VectorSource(AFC2))
ACM <- Corpus(VectorSource(ACM))
AD <- Corpus(VectorSource(AD))
C <- Corpus(VectorSource(C))

par( mfrow = c( 1,1 ),  oma=c(0,.5,.5,.5))
dotchart(as.numeric(tail(Resultat[,-8],1)),labels=colnames(Resultat[,-8]),cex=.7,
         main="Nombre de mots dans chaque cours apres traitement")
abline(v=median(as.numeric(tail(Resultat[,-8],1))),lty=1)
abline(v=mean(as.numeric(tail(Resultat[,-8],1))),lty=2)
legend("bottomright", inset=.05, title="",c("M�diane","Moyenne"), lty=c(1,2), lwd=2)
#########################################################################
RM <- tm_map(RM, specialchars)
RM <- tm_map(RM, removeNumbers)
RM <- tm_map(RM, removeWords, stopwords("french"))
RM <- tm_map(RM, stripWhitespace)
RM <- tm_map(RM, stemDocument)

dtm1 <- TermDocumentMatrix(RM)
m1 <- as.matrix(dtm1)
v1 <- sort(rowSums(m1),decreasing=TRUE)
dRM <- data.frame(freq=v1)
dRM <- setNames(cbind(rownames(dRM), dRM, row.names = NULL),c("Nom", "Frequence"))
dRM <- dRM[!(dRM$Frequence<=17),]


###########################################################################
ACP <- tm_map(ACP, specialchars)
ACP <- tm_map(ACP, removeNumbers)
ACP <- tm_map(ACP, removeWords, stopwords("french"))
ACP <- tm_map(ACP, stripWhitespace)
ACP <- tm_map(ACP, stemDocument)

dtm2 <- TermDocumentMatrix(ACP)
m2 <- as.matrix(dtm2)
v2 <- sort(rowSums(m2),decreasing=TRUE)
dACP <- data.frame(freq=v2)
dACP <- setNames(cbind(rownames(dACP), dACP, row.names = NULL),c("Nom", "Frequence"))
dACP <- dACP[!(dACP$Frequence<=17),]

############################################################################
AFC1 <- tm_map(AFC1, specialchars)
AFC1 <- tm_map(AFC1, removeNumbers)
AFC1 <- tm_map(AFC1, removeWords, stopwords("french"))
AFC1 <- tm_map(AFC1, stripWhitespace)
AFC1 <- tm_map(AFC1, stemDocument)

dtm3 <- TermDocumentMatrix(AFC1)
m3 <- as.matrix(dtm3)
v3 <- sort(rowSums(m3),decreasing=TRUE)
dAFC1 <- data.frame(freq=v3)
dAFC1 <- setNames(cbind(rownames(dAFC1), dAFC1, row.names = NULL),c("Nom", "Frequence"))
dAFC1 <- dAFC1[!(dAFC1$Frequence<=17),]

############################################################################
AFC2 <- tm_map(AFC2, specialchars)
AFC2 <- tm_map(AFC2, removeNumbers)
AFC2 <- tm_map(AFC2, removeWords, stopwords("french"))
AFC2 <- tm_map(AFC2, stripWhitespace)
AFC2 <- tm_map(AFC2, stemDocument)

dtm4 <- TermDocumentMatrix(AFC2)
m4 <- as.matrix(dtm4)
v4 <- sort(rowSums(m4),decreasing=TRUE)
dAFC2 <- data.frame(freq=v4)
dAFC2 <- setNames(cbind(rownames(dAFC2), dAFC2, row.names = NULL),c("Nom", "Frequence"))
dAFC2 <- dAFC2[!(dAFC2$Frequence<=17),]

############################################################################
ACM <- tm_map(ACM, specialchars)
ACM <- tm_map(ACM, removeNumbers)
ACM <- tm_map(ACM, removeWords, stopwords("french"))
ACM <- tm_map(ACM, stripWhitespace)
ACM <- tm_map(ACM, stemDocument)

dtm5 <- TermDocumentMatrix(ACM)
m5 <- as.matrix(dtm5)
v5 <- sort(rowSums(m5),decreasing=TRUE)
dACM <- data.frame(freq=v5)
dACM <- setNames(cbind(rownames(dACM), dACM, row.names = NULL),c("Nom", "Frequence"))
dACM <- dACM[!(dACM$Frequence<=17),]

#############################################################################
AD <- tm_map(AD, specialchars)
AD <- tm_map(AD, removeNumbers)
AD <- tm_map(AD, removeWords, stopwords("french"))
AD <- tm_map(AD, stripWhitespace)
AD <- tm_map(AD, stemDocument)

dtm6 <- TermDocumentMatrix(AD)
m6 <- as.matrix(dtm6)
v6 <- sort(rowSums(m6),decreasing=TRUE)
dAD <- data.frame(freq=v6)
dAD <- setNames(cbind(rownames(dAD), dAD, row.names = NULL),c("Nom", "Frequence"))
dAD <- dAD[!(dAD$Frequence<=17),]

#############################################################################
C <- tm_map(C, specialchars)
C <- tm_map(C, removeNumbers)
C <- tm_map(C, removeWords, stopwords("french"))
C <- tm_map(C, stripWhitespace)
C <- tm_map(C, stemDocument)

dtm7 <- TermDocumentMatrix(C)
m7 <- as.matrix(dtm7)
v7 <- sort(rowSums(m7),decreasing=TRUE)
dC <- data.frame(freq=v7)
dC <- setNames(cbind(rownames(dC), dC, row.names = NULL),c("Nom", "Frequence"))
dC <- dC[!(dC$Frequence<=17),]

##############################################################################
# Merging

Df <- bind_rows(list(RM=dRM, ACP=dACP, AFC1=dAFC1, AFC2=dAFC2, ACM=dACM, AD=dAD, C=dC), .id="ID")

#Creating table

Resultat = dcast(Df, Nom~ID, value=Frequence) #Reshape avec tri alphabetique

Resultat[is.na(Resultat)] <- 0 #Remplacer les valeurs NULL par 0

Resultat <- cbind(Resultat, Total = rowSums(Resultat[-1])) #Creation de la colonne Total

Resultat <- Resultat[!(Resultat$Total>=150),] #La condition sur la colonne Total

Resultat <- rbind(Resultat, data.frame(Nom = "Total", t(colSums(Resultat[,-1])))) #Creation de la ligne Total

row.names(Resultat) = Resultat$Nom # Changement du nom des lignes 1
Resultat[1] = NULL # Changement du nom des lignes 2
