library(TraMineR)
library(RColorBrewer)
library(cluster)


rawData = read.csv("Pedro_Review_22062015.csv",sep=";")  

companyName = rawData[,1]
fullName = rawData[,2]
pId = rawData[,3]


companies = rawData[rawData[,5]=="Companies",]
titles = rawData[rawData[,5]=="Titles",]
membership = rawData[rawData[,5]=="Board Memberships",]


# Functions
getSequence <- function(raw){
  return(raw[,-seq(1:5)])
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

makeSequencePlots <- function(seq){
  seqIplot(seq,sortv="from.start",withlegend=TRUE,title="Companies - All sequences - Sort by start")
  
  seqIplot(seq,sortv="from.end",withlegend=TRUE,title="Companies - All sequences - Sort by end")
  
  seqfplot(seq, withlegend = TRUE, border = NA, title="10 Most Frequent Sequences")
  
  seqdplot(seq, withlegend = TRUE, border = NA, title="States Distribution Over Time")
  
  seqmtplot(seq, withlegend = TRUE, title="Mean Time spent in each State")
  return(TRUE)
}



# I - COMPANIES ANALYSIS
companiesSequence <- getSequence(companies)
# Clean Data
companiesSequence[companiesSequence == "b"] <- "B"
companiesSequence[companiesSequence == "c"] <- "C"
companiesSequence[companiesSequence == "Partner"] <- "Z"
companiesSequence[companiesSequence == "Consultant"] <- "Z"
companiesSequence[companiesSequence == "President"] <- "Z"
companiesSequence[companiesSequence == "President, COO,CEO"] <- "Z"
companiesSequence[companiesSequence == "Senior Advisor"] <- "Z"
companiesSequence[companiesSequence == "sr. v.p., corp. Strategy"] <- "Z"
companiesSequence[companiesSequence == "President, COO"] <- "Z"
companiesSequence[companiesSequence == "CHECK"] <- "Z"

for (row in seq(1:dim(companiesSequence)[1])){
  for (col in seq(1:dim(companiesSequence)[2])){
    companiesSequence[row,col] <- trim(companiesSequence[row,col])
  }
}

# Build Sequence
companiesSequence.scodes <- unique(unlist(companiesSequence))
companiesSequence.alphabet <- unique(unlist(companiesSequence))
companiesSequence.labels <- unique(unlist(companiesSequence))
companiesSequence.seq <- seqdef(companiesSequence,NULL,states=companiesSequence.scodes,labels=companiesSequence.labels, alphabet=companiesSequence.alphabet,missing="Z")



# Vanilla sequence
companiesSequence.seq <- seqdef(companiesSequence,NULL,missing="Z")

cpal(companiesSequence.seq) <- brewer.pal(3,"Spectral")

makeSequencePlots(companiesSequence.seq)


# Synchronized Sequence -  Deleting values to the left and to the right 
companiesSequence.scodes <- c('A','B','C')
companiesSequence.alphabet <- c('A','B','C')
companiesSequence.labels <- c('A','B','C')
companiesSequence.seq <- seqdef(companiesSequence,NULL,states=companiesSequence.scodes,labels=companiesSequence.labels, alphabet=companiesSequence.alphabet,missing="Z",,right="DEL",left="DEL",void="Z")

companiesSequence.seq <- companiesSequence.seq[-c(24,44,178,229,330),]

companiesSequence.seq <- companiesSequence.seq[-226,]

# dim(companiesSequence.seq)
# 
# sum(rowSums(companiesSequence.seq == "Z") == 71)
# x<-which(rowSums(companiesSequence.seq == "*") == 71)
# 
# companiesSequence.seq[226,]




# II. Distance - Single dimension

mcdist <- seqdist(seqdata=companiesSequence.seq,method="OM",sm="CONSTANT",with.missing=TRUE)

clusterward<- agnes(mcdist,diss=TRUE,method="ward")
plot(clusterward,which.plots=2)
clusters <- cutree(clusterward,k=8)
table(clusters)


seqfplot(companiesSequence.seq,group=clusters,pbarw=TRUE)
seqmtplot(companiesSequence.seq, group = clusters)
seqIplot(companiesSequence.seq,sortv="from.start",group=clusters,withlegend=TRUE,title="Companies - All sequences - Sort by start")




# III. Event
companiesSequence.seqe <- seqecreate(companiesSequence.seq,tevent="state", use.labels=FALSE)
subsSequences <- seqefsub(companiesSequence.seqe,strsubseq=c("(A)-(C)-(A)","(A)-(B)-(A)"))
print(subsSequences)
subsSequences.pres <- seqeapplysub(subsSequences[,], method="presence")
index <- which(subsSequences.pres[,1]==1)

# Parei no DistanceUnitaryCmns - LOOKING FOR FREQUENTE SUBSEQUENCES

