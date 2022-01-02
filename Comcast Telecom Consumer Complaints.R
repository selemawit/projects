
#-----------------------------------------------------------------------------------------------------------

setwd("C:/Users/Selam/Desktop")
getwd()
df<- read.csv("C:/Users/Selam/Desktop/smiplilearn/R/1567503160_comcasttelecomcomplaintsdata/Comcast Telecom Complaints data.csv")
str(df)

#checking for missing values
sapply(df, function(x) sum(is.na(x)))

# Provide the trend chart for the number of complaints at monthly and daily granularity levels.
#df$Date<-as.Date(df$Date,format= "%d/%m/%Y")
f<- table(df$Date)
fd<- data.frame(f)

library(ggplot2)
chart_mon<-ggplot(fd)+ geom_bar(aes(x=Var1, y= Freq),stat= 'identity')

chart_mon + theme(axis.text.x = element_text(angle = 90))

#--------------------------------------------------------------------------------------------------------

#Provide a table with the frequency of complaint types.

df$Customer.Complaint<-replace(df$Customer.Complaint,df$Customer.Complaint=="Comcast" 
|df$Customer.Complaint=="COMCAST","comcast")


complaint <- sort(table(df$Customer.Complaint),decreasing=T)
write.csv(complaint,"mytable.csv",quote=F)
comp_t<-read.csv("mytable.csv")
comp_t

#Which complaint types are maximum i.e., around internet, network issues, or across any other domains.

#----------------------------------------------------------------------------------------------------------

#Create a new categorical variable with value as Open and Closed. Open & Pending is to be categorized as Open and Closed & Solved is to be categorized as Closed.

str(df)
df$Status<-tolower(df$Status)
df$Status<-replace(df$Status,df$Status=="pending", "open")
df$Status<-replace(df$Status,df$Status=="solved", "closed")
df$Status
#-----------------------------------------------------------------------------------------------------------------------------

#Provide state wise status of complaints in a stacked bar chart.
#Which state has the maximum complaints

state_status<- table(df$State,df$Status)
state_status
table1 = state_status[order(state_status[,"open"], decreasing = TRUE),]
head(df$State)
table1

df1<-data.frame(table1)
df1

library(ggplot2)
state_s<- ggplot(df1, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="stack", stat="identity" ) 

state_s + theme(axis.text.x = element_text(angle = 90))


#Which state has the highest percentage of unresolved complaints
per_state<- ggplot(df1, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="fill", stat="identity")

per_state + theme(axis.text.x = element_text(angle = 90))

#Provide the percentage of complaints resolved till date, which were received through the Internet and customer care calls.

df

received_status<- table(df$Received.Via, df$Status)
received_status

tab <- with(df, table(Received.Via, Status))
t<-prop.table(tab, margin = 2)
t


