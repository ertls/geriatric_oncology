#speichern:
sex <- mydata_bak[,c(3,27)]
write.csv(sex, "sex.csv")

#öffnen, bearbeiten, z.B. in Excel, abspeichern, erneut einlesen
sex <- read.table("sex_bearbeitet.csv", header=F, sep=";")
mydata_bak$sex <- sex[,3]
