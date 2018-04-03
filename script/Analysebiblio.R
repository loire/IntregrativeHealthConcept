require(tidyverse)
require(bibliometrix)
# CMAEE=readFiles("CMAEE.bib")
# ASTRE=readFiles("ASTRE.bib")
# AGIR =readFiles("AGIR.bib")
# 
# CMAEE = convert2df(CMAEE,dbsource="isi",format="bibtex")
# AGIR = convert2df(AGIR,dbsource="isi",format="bibtex")
# ASTRE = convert2df(ASTRE,dbsource="isi",format="bibtex")
# 
# LAB = rbind(CMAEE,AGIR,ASTRE)
# length(LAB$NU)
# LAB_DB = biblioAnalysis(LAB,sep=";")
# 
# sum(table(LAB$PY))
# 
# 
# 
# CMAEE_year  = as.data.frame(table(CMAEE$PY),stringsAsFactors=F)
# colnames(CMAEE_year) = c("year","count")
# CMAEE_year$lab = "CMAEE"
# #CMAEE_year$year=as.integer(CMAEE_year$year)
# 
# ASTRE_year  = as.data.frame(table(ASTRE$PY,stringsAsFactors=F))
# colnames(ASTRE_year) = c("year","count")
# ASTRE_year$lab = "ASTRE"
# #ASTRE_year$year=as.integer(ASTRE_year$year)
# 
# AGIR_year  = as.data.frame(table(AGIR$PY),stringsAsFactors=F)
# colnames(AGIR_year) = c("year","count")
# AGIR_year$lab = "AGIR"
# #AGIR_year$year=as.integer(AGIR_year$year)
# 
# datayear = rbind(CMAEE_year,AGIR_year,ASTRE_year)


####################################
#
# Years data from all database
#
###################################


lab  = read.table("data/Lab_year.txt",sep="\t",header=T)


colnames(lab) = c('year',"count","frac")
lab  %>% ggplot() + 
  geom_bar(aes(x=year,y=count),stat="identity") +
  theme_minimal() + scale_x_continuous(labels = 2007:2018,breaks =2007:2018)
sum(lab$count)


###########################
#
# Read bibtex files (core collection)
# Convert to dataframe
#
###########################

Lab=readFiles("data/Lab.bib")
Lab= convert2df(Lab,dbsource="isi",format="bibtex")

############################
#
# Add a column with ASTRE authors only
#
############################


authors = readFiles("listauthors.txt")
ASTREauthors = trimws(toupper(authors),which="both")
filtre_author = function(listau){
  outautall=""
  for(aut in strsplit(strsplit(listau,split=";")[[1]]," ")){
    if(aut[1] %in% ASTREauthors | paste(aut[1],aut[2],collapse=" ") %in% ASTREauthors){
      outaut = paste(aut,collapse=" ")
      outautall = paste(outautall,outaut,collapse=";",sep=";")
      }

  }
  return(trimws(substr(outautall,2,nchar(outautall)))) 
} 
Lab$AU

PLab = Lab %>% rowwise %>% mutate(AA = filtre_author(AU))  %>% filter(AA!="") %>% ungroup %>% as.data.frame

str(Lab)
str(PLab)





##### Historical network of autocitations
histResults <- histNetwork(Lab, n = 660, sep = ".  ")
histResults$histData %>% filter(GCS>50)

pdf("historicalnetwork.pdf",height = 20,width=50)
histPlot(histResults, size = TRUE,label=T,labelsize=0.5, arrowsize = 0.5,remove.isolates = T)
dev.off()


###### Most cited MS
CR <- citations(Lab, field = "article", sep = ".  ")
CR$Cited[1:10]

##### Productivity
L = lotka(Labdb)
L$AuthorProd
Observed=L$AuthorProd[,3]
Theoretical=10^(log10(L$C)-2*log10(L$AuthorProd[,1]))
plot(L$AuthorProd[,1],Theoretical,type="l",col="red",ylim=c(0, 1), xlab="Articles",ylab="Freq. of Authors",main="Scientific Productivity")
lines(L$AuthorProd[,1],Observed,col="blue")
legend(x="topright",c("Theoretical (B=2)","Observed"),col=c("red","blue"),lty = c(1,1,1),cex=0.6,bty="n")



########### Relation entre pays
Labdb= biblioAnalysis(Lab,sep=";")
summary(Labdb)

Labdb
LabAstre$AU = PLab$AA


LabAstre <- metaTagExtraction(LabAstre, Field = "AU_CO", sep = ";")

NetMatrix <- biblioNetwork(LabAstre, analysis = "collaboration", network = "authors",sep=";")


net=networkPlot(NetMatrix,Title = "Country Collaboration", type = "fruchterman", size=F, remove.multiple=T,labelsize=0.8)



#### PCA on authors


AutMat <- cocMatrix(PLab, Field = "AA", sep = ";") %>% as.matrix %>% as.data.frame

PLab$AA

require(MASS)

table(rowSums(AutMat)>=14)

AutMat %>% dim

AutMat %>% prcomp %>% ggbiplot(scale=0)

table(grepl("ONE HEALTH",PLab$AB))
PLab$INTEGRA = grepl("ONE HEALTH|INTEGR|SYSTEM|SOCIO|ECONO|PARTICIP",PLab$AB)



?ggbiplot

library(Factoshiny)


require(ggbiplot)


data(wine)
str(wine)
wine.pca <- prcomp(wine[,3:ncol(wine)])
ggbiplot(wine.pca, obs.scale = 1, var.scale = 1, groups = wine$Soil, ellipse = TRUE, circle = TRUE)


length(tt[1])
tt = ";GUIS H"
substr(tt,1,length(tt))

Lab$TI[155]

grepl("TRAN",ASTREauthors)
ASTREauthors


length(Lab$PY)

Lab_db = biblioAnalysis(Lab,sep=";")


str(Lab_db)



DF = dominance(Lab_db,k=20)
DF

CR <- citations(Lab, field = "article", sep = ".  ")
CR$Cited[1:10]




NetMatrix <- biblioNetwork(Lab, analysis = "co-occurrences", network = "keywords", sep = ";")
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 60, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 10,labelsize=0.7)



CS <- conceptualStructure(Lab,field="AB", minDegree=6, k.max=5, stemming=FALSE, labelsize=8)

?conceptualStructure


A <- cocMatrix(Lab, Field = "ID", sep = ";")
sort(Matrix::colSums(A), decreasing = TRUE)[1:5]

A %>% glimpse
prcomp(A)
require(ggfortify)
autoplot(prcomp(A),loadings=T)

NetMatrix <- biblioNetwork(Lab, analysis = "coupling", network = "authors", sep = ";")

net=networkPlot(NetMatrix, normalize = "salton", weighted=T, n = 20, Title = "Authors' Coupling", type = "fruchterman", size=FALSE,remove.multiple=TRUE)


LAB$C1


LAB %>% select(PY,CI) %>% head



LAB$CR
CR <- citations(LAB, field = "article", sep = ".  ")

CR$Year


DF = dominance(LAB_DB,k=20)
DF


summary(LAB_DB,k=20)

LAB_DB$Articles

rownames((LAB)) %>% unique %>% length


M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")

require(devtools)
install_github("vqv/ggbiplot")
require(ggbiplot)

A <- cocMatrix(Lab, Field = "SC", sep = ";")
field.pca = prcomp(A %>% as.matrix,scale. = TRUE)

pap = field.pca$x[,1:2] %>% rownames

dfpca = field.pca$x[,1:2] %>% as.data.frame
dfpca %>% mutate(paper = pap) %>% filter(PC1 < -20)






ggbiplot(field.pca, obs.scale = 1, var.scale = 1,
          ellipse = TRUE, circle = TRUE, var.axes=F) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')

field.pca$rotation %>% head

?ggbiplot


NetMatrix <- biblioNetwork(Lab_db, analysis = "coupling", network = "keywords", sep = ";")


net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 50, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)
PLab %>% filter(PY > 2015) %>% as.data.frame


tmp = PLab %>% filter(PY >= 2013) %>% as.data.frame
nrow(tmp)


CS <- conceptualStructure(PLab,field="AB", minDegree=50, k.max=5, stemming=T, labelsize=7)

NetMatrix <- biblioNetwork(Lab, analysis = "co-occurrences", network = "keywords", sep = ";")

pdf("Keywords.pdf",height=20,width=20)
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 100, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=1.4)
dev.off()

CS$res.mc$ind$contrib %>% head


rm(OH)


net <- histPlot(histResults, size = FALSE,label=TRUE, arrowsize = 0.5,remove.isolates = T)



?histPlot



str(net)

