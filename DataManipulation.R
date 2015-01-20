dev.off()
rm(list=ls()) 
#right numbers - 50466 & 18680

#setwd("/Users/leifurbjarkierlendsson/Dropbox/Applied_Statistics_Shared/Leifur/Applied_statistics/Assignment2")
#dir("/Users/leifurbjarkierlendsson/Dropbox/Applied_Statistics_Shared/Leifur/Applied_statistics/Assignment2")

#read in the data 
data.1 <- read.table("campy_pre2002.txt",header=T,sep="\t", fill = TRUE)
data.2 <- read.table("campy_2002-2005.csv",header=T,sep=",")
data.3 <- read.table("campy_2005-.csv",header=T,sep=",")

######## Steps in merging and cleaning the files############
#1) pre2002: Remove those with SEKTION=="res"
#2) pre2002: Only keep those with AKTVNR==5133 
#3) All files: Valid CHR numbers are 10000 and above
data.1 <- subset(data.1, (!SEKTION=="res" & AKTVNR==5133 & CHR_NR>=10000))
data.2  <- subset(data.2, Chrnr>=10000)
data.3  <- subset(data.3, Chrnr>=10000)

#4) Convert dates to common format.
data.1$PRV_DATO  <- as.Date(data.1$PRV_DATO,format="%d %b %Y")
data.2$Prvdato  <- as.Date(data.2$Prvdato,format="%m/%d/%y")
data.3$Provedato  <- as.Date(data.3$Provedato,format="%m/%d/%y")

#5) Get same order of columns to keep and then rename.
data.1 <- data.1[,c("CHR_NR" ,"EPINR", "JNR", "MATR","BAKTFUND", "PRV_DATO", "region")]
data.2  <- data.2[,c("Chrnr", "Epi.nr", "Jnr", "Materialeart", "Resultat", "Prvdato", "region")]
data.3  <- data.3[,c("Chrnr", "Epinr", "Jnr", "Materialeart", "Tolkning", "Provedato", "region")]

column.name  <-  c("chrnr", "epinr", "jnr", "matr", "resultat", "prvdato", "region")
colnames(data.1) <- column.name
colnames(data.2) <- column.name
colnames(data.3) <- column.name


#6) Merge the data using "rbind"
data <- rbind(data.1, data.2,data.3)

#7) Remove records with NA as epinr.
data <- subset(data,(!is.na(epinr)))
                         
#8) Reduce the levels of resultat to only "POS" or "NEG"
levels(data$resultat)  <- c("NEG","POS","POS","POS","POS","POS","POS","POS","POS","POS","POS","NEG","POS")

#9) Only keep records with "matr" in c("Kloaksvaber","Svaberprøve","766","772")
data  <- subset(data, matr =="Kloaksvaber"| matr =="Svaberprøve" | matr =="766" | matr =="772")


#10) Some tests are recorded in two files with different JNR!?!
data  <-  data[!duplicated(data[c("chrnr","epinr","resultat","prvdato")]),]

#11) Add week number since week one 1998 for each record
week1 = as.Date("1998-01-01")
data$weeknr  <- ceiling(as.numeric(data$prvdato-week1)/7)

#12) Only keep those with positive week number
data  <- subset(data,weeknr>=0)

#13) Remove records with duplicated jnr (Keep first record)
data <- subset(data,duplicated(jnr)!=TRUE)

#14) skipped

#15) Only include data from farms that have delivered more than 10 flocks, as those with less may have a bias.
library("plyr")
bias.farms <- ddply(data,.(chrnr),nrow)
bias.farms <- subset(bias.farms, V1<10)
data$bias <- data$chrnr %in% bias.farms$chrnr 
data <- subset(data, data$bias==FALSE)

#16) Summarize number of flocks slaughtered and number of positive flocks per week. 
#The output should be a table with one row per week and two columns (Total and positive counts).

data.all <- ddply(data,.(weeknr),nrow)
data.pos <- subset(data,resultat =="POS")
pos.week  <- ddply(data.pos,.(weeknr),nrow)
final.data <- data.frame(data.all,pos.week$V1)
write.table(data, "/Users/thrastarson/Desktop/dater.csv")
colnames(final.data) <- c("weeknr","total_meas","pos_meas")

#17) Save your data file!
save(final.data,file="campy.txt")
write.table(final.data, file = "campy.txt")
