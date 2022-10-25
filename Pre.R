df<-read.csv("toronto.csv", na.strings=c("","No Information","Pending","UNKNOWN","NOT LISTED, PLEASE SPECIFY","OTHER"))
head(df)
summary(df)
sum(is.na(df))

df$Episode.Date <- as.Date(df$Episode.Date, format = "%m/%d/%Y")
df$Reported.Date <- as.Date(df$Reported.Date, format = "%m/%d/%Y")
class(df$Episode.Date)

col <- c("Outbreak.Associated","Age.Group","Neighbourhood.Name","FSA","Source.of.Infection","Classification","Client.Gender", "Outcome","Currently.Hospitalized",
         "Currently.in.ICU","Currently.Intubated","Ever.Hospitalized","Ever.in.ICU","Ever.Intubated")
df[col] <-lapply(df[col],factor)

summary(df)
head(df)

df1<-df[complete.cases(df), ]
df1 <- df1[ -c(1:2) ]
summary(df1)

nrow(df)-nrow(df1)
nrow(df)
nrow(df1)

df1$Onset.Report<-(df1$Reported.Date-df1$Episode.Date)
head(df1)

write.csv(df1,"C:/KZ/S/CIND820\\df1.csv", row.names = FALSE)

head(df1)

#took out currently not much value, too little obs
df2<-df1[ -c(11:13) ]
head(df2)
summary(df2)
write.csv(df2,"C:/KZ/S/CIND820\\df2.csv", row.names = FALSE)
str(df2)
as.integer(df2$Onset.Report)

df3 <- df2
df3$Onset.Report[df3$Onset.Report<0] <- 0



df4<-df3
summary(df4)
df4<-df4[!grepl('ACTIVE', df4$Outcome),]
df4<-df4[!grepl('PROBABLE', df4$Classification),]
str(df4)
summary(df4)

activecases<-df3[grep("ACTIVE", df3$Outcome), ]
head(activecases)


mean(df4$Onset.Report)
min(df4$Onset.Report)
median(df4$Onset.Report)
sd(df4$Onset.Report)
quantile(df4$Onset.Report)
max(df4$Onset.Report)

col2keep <- c("Outbreak.Associated","Age.Group","Episode.Date","Source.of.Infection","Client.Gender","Neighbourhood.Name","Ever.Hospitalized","Ever.in.ICU","Ever.Intubated","Onset.Report","Outcome")

df5<-df4[col2keep]
head(df5)
write.csv(df5,"C:/KZ/S/CIND820\\df5.csv", row.names = FALSE)


join<-read.csv("join.csv")
head(join)
str(df5)
df6<-merge(df5,join, by = "Neighbourhood.Name")
df6$Income<-as.numeric(gsub(",", "", df6$Income))
df6$Onset.Report<-as.numeric(df6$Onset.Report)

write.csv(df6,"C:/KZ/S/CIND820\\df6.csv", row.names = FALSE)

summary(df6)
sd(df6$Income)
str(df6)

df6$Month <- format(df6$Episode.Date, "%m")
head(df6)

fatal<-df6[grep("FATAL", df6$Outcome), ]
head(fatal)
summary(fatal$Age.Group)
nrow(fatal)
sd(fatal$Income)

resolved<-df6[grep("RESOLVED", df6$Outcome), ]
summary(resolved)
summary(resolved$Age.Group)
sd(resolved$Onset.Report)
