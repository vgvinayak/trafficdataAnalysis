#Traffic Project
install.packages("ggplot2")
library("ggplot2")
# type of accident
Type_of_accidentanalysis <-function(a){
  colnames(a)<-tolower(colnames(a))
  Type_of_accident<-as.data.frame(table(tolower(a$type.of.accident)))
  names(Type_of_accident)<-c("TypeOfAccident","No.OfIncident")
  return(Type_of_accident)
}
#Barplot
Barplt_typeoaccident<-function(a){
  return(barplot(table(a$Type.of.accident),las=2,cex.names = .6,ylim=range(0:50)))}
#Chainage No.of incident
chainage_No.ofincident <-function(a){
  colnames(a)<-tolower(colnames(a))
  filter<-sub("\\+(.*)","",a$chainage)
  filter<-as.data.frame(table(filter))
  names(filter)<-c("ChainageInterval","No.ofincident")
  filter<-filter[order(filter$No.ofincident,decreasing = T),]
  return(filter)
}
#Chainage dataframe
chainage_dataframe<-function(a){
  colnames(a)<-tolower(colnames(a))
  Chainageframes<-data.frame(Chainageframe=sub("\\+(.*)","",a$chainage))
  a<-cbind(a,Chainageframes)
  Chainageframes$Chainageframe<-as.character(Chainageframes$Chainageframe)
  filter<-sub("\\+(.*)","",a$chainage)
  filter<-as.data.frame(table(filter))
  names(filter)<-c("ChainageInterval","No.ofincident")
  filter<-filter[order(filter$No.ofincident,decreasing = T),]
  filter<-filter[filter$No.ofincident>=max(filter$No.ofincident)/2,]
  compare<-as.character(filter$ChainageInterval)
  b<-a[a$Chainageframe==compare[1],]
  n=2
  repeat{
    c<-a[a$Chainageframe==compare[n],]
    b<-rbind(b,c)
    n<-n+1
    if(n>length(compare)){break}
  }
  return(b)
}
#Month No.of incident
Month_vs_No.ofincident<-function(a){
  colnames(a)<-tolower(colnames(a))
  a$date<-as.Date(a$date,format="%d.%m.%y")
  a$month<-as.numeric(format(a$date,"%m"))
  data_frame<-data.frame(month=c("Jan","Feb","Mar","Apr","May","Jun","July","Aug","Sep","Oct","Nov","Dec"),no.incident=c(nrow(a[a$month=="1",]),nrow(a[a$month=="2",]),nrow(a[a$month=="3",]),nrow(a[a$month=="4",]),nrow(a[a$month=="5",]),nrow(a[a$month=="6",]),nrow(a[a$month=="7",]),nrow(a[a$month=="8",]),nrow(a[a$month=="9",]),nrow(a[a$month=="10",]),nrow(a[a$month=="11",]),nrow(a[a$month=="12",])))
  return(data_frame)}
#Month wise distribution
Month_wise_distribution<-function(a){
  colnames(a)<-tolower(colnames(a))
  a$date<-as.Date(a$date,format="%d.%m.%y")
  a$month<-as.numeric(format(a$date,"%m"))
  b<-1
  Month_dataframe<-data.frame(Month=NA,TotalFatal=NA,TotalMajor=NA,TotalMinor=NA,TotalNon.injured=NA)
  repeat{
    frame<-data.frame(Month=b,TotalFatal=sum(a[a$month==b,]$fatal),TotalMajor=sum(a[a$month==b,]$major),TotalMinor=sum(a[a$month==b,]$minor),TotalNon.injured=sum(a[a$month==b,]$non.injured))
    Month_dataframe<-rbind(Month_dataframe,frame)
    b<-b+1
    if(b>12){break}}
  Month_dataframe<-na.omit(Month_dataframe)
  Month_dataframe$Month<-c("Jan","Feb","Mar","Apr","May","Jun","July","Aug","Sep","Oct","Nov","Dec")
  return(Month_dataframe)}
#plot of Chainage No.of incident
plot_chainage_No.ofincident<-function(a){
  colnames(a)<-tolower(colnames(a))
  filter<-sub("\\+(.*)","",a$chainage)
  filter<-as.data.frame(table(filter))
  names(filter)<-c("ChainageInterval","No.ofincident")
  filter<-filter[order(filter$No.ofincident,decreasing = T),]
  filter$ChainageInterval<-factor(filter$ChainageInterval,levels = filter$ChainageInterval)
  return(ggplot(data=filter,aes(x=(ChainageInterval),y=No.ofincident))+geom_histogram(stat="identity")+theme(axis.text.x=element_text(angle=90,hjust=1))+geom_hline(yintercept = max(filter$No.ofincident)/2))
}
#plot of Month No.ofincident
plot_Month_vs_No.ofincident<-function(a){
  colnames(a)<-tolower(colnames(a))
  a$date<-as.Date(a$date,format="%d.%m.%y")
  a$month<-as.numeric(format(a$date,"%m"))
  data_frame<-data.frame(Month=c("Jan","Feb","Mar","Apr","May","Jun","July","Aug","Sep","Oct","Nov","Dec"),No.incident=c(nrow(a[a$month=="1",]),nrow(a[a$month=="2",]),nrow(a[a$month=="3",]),nrow(a[a$month=="4",]),nrow(a[a$month=="5",]),nrow(a[a$month=="6",]),nrow(a[a$month=="7",]),nrow(a[a$month=="8",]),nrow(a[a$month=="9",]),nrow(a[a$month=="10",]),nrow(a[a$month=="11",]),nrow(a[a$month=="12",])))
  data_frame$Month<-factor(data_frame$Month,levels = data_frame$Month)
  Month<-as.character(data_frame$month)
  return(ggplot(data=data_frame,aes(x=Month,y=No.incident,group=1,label=data_frame$No.incident,fill=Month))+geom_bar(stat="identity")+geom_text(position=position_stack(vjust=0.5),color="black")+geom_point()+geom_line(size=1.5))
}
#plot of Month wise distribution
Plot_Month_wise_distribution<-function(a){
  colnames(a)<-tolower(colnames(a))
  a$date<-as.Date(a$date,format="%d.%m.%y")
  a$month<-as.numeric(format(a$date,"%m"))
  b<-1
  Month_dataframe<-data.frame(Month=NA,TotalFatal=NA,TotalMajor=NA,TotalMinor=NA,TotalNon.injured=NA)
  repeat{
    frame<-data.frame(Month=b,TotalFatal=sum(a[a$month==b,]$fatal),TotalMajor=sum(a[a$month==b,]$major),TotalMinor=sum(a[a$month==b,]$minor),TotalNon.injured=sum(a[a$month==b,]$non.injured))
    Month_dataframe<-rbind(Month_dataframe,frame)
    b<-b+1
    if(b>12){break}}
  Month_dataframe<-na.omit(Month_dataframe)
  Month_dataframe$Month<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  Month_wise1<-data.frame(Month=Month_dataframe$Month,Type=rep("TotalFatal",12),No.=as.numeric(Month_dataframe$TotalFatal))
  Month_wise2<-data.frame(Month=Month_dataframe$Month,Type=rep("TotalMajor",12),No.=as.numeric(Month_dataframe$TotalMajor))
  Month_wise3<-data.frame(Month=Month_dataframe$Month,Type=rep("TotalMinor",12),No.=as.numeric(Month_dataframe$TotalMinor))
  Month_wise4<-data.frame(Month=Month_dataframe$Month,Type=rep("TotalNon.injured",12),No.=as.numeric(Month_dataframe$TotalNon.injured))
  Month_wise<-rbind(Month_wise1,Month_wise2,Month_wise3,Month_wise4)
  Month_wise$Month<-factor(Month_wise$Month,levels =c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  return(ggplot(data=Month_wise,aes(x=Month,y=No.,group=1,fill=Month))+geom_bar(stat="identity",aes(fill=Month_wise$Type)))
}

#Transport analysis
Transport_analysis<-function(a){
  colnames(a)<-tolower(colnames(a))
  vehiclel1<-data.frame(Vehicle=tolower(a$vehicle))
  Vehiclel2<-data.frame(Vehicle=tolower(a$vehicle2))
  VehiclelTotal<-rbind(vehiclel1,Vehiclel2)
  vehicletotalDataframe<-as.data.frame(table(VehiclelTotal))
  names(vehicletotalDataframe)<-c("Type","No.")
  vehicletotalDataframe1<-vehicletotalDataframe[!(vehicletotalDataframe$Type=="")&!(vehicletotalDataframe$Type=="NA"),]
  return(vehicletotalDataframe1)}
#Time wise no.of accident
Time_wiseno.ofincident<-function(a){
  colnames(a)<-tolower(colnames(a))
  ourframe<-data.frame(Ourtime=strptime(tolower(a$time),("%I:%M %p")))
  timeframee<-cbind(a,ourframe)
  timeframee<-na.omit(timeframee)
  a<-strptime("06:00 AM","%I:%M %p")
  b<-strptime("12:00 PM","%I:%M %p")
  c<-strptime("06:00 PM","%I:%M %p")
  d<-strptime("11:59:59 PM","%I:%M:%S %p")
  e<-strptime("12:00:01 AM","%I:%M:%S %p")
  dataframe<-data.frame(TimeDistribution=c("6AMto12PM","12PMto6PM","6PMto12AM","12AMto6AM"),No.ofIncident=c(nrow(timeframee[timeframee$Ourtime>=a & timeframee$Ourtime<b,]),nrow(timeframee[timeframee$Ourtime>=b & timeframee$Ourtime<c,]),nrow(timeframee[timeframee$Ourtime>=c & timeframee$Ourtime<=d,]),nrow(timeframee[timeframee$Ourtime>=e & timeframee$Ourtime<a,])))
  return(dataframe)
}
#plot transport analysis
plot_transport_analysis<-function(a){
  colnames(a)<-tolower(colnames(a))
  vehiclel1<-data.frame(Vehicle=tolower(a$vehicle))
  Vehiclel2<-data.frame(Vehicle=tolower(a$vehicle2))
  VehiclelTotal<-rbind(vehiclel1,Vehiclel2)
  vehicletotalDataframe<-as.data.frame(table(VehiclelTotal))
  names(vehicletotalDataframe)<-c("Type","No.")
  vehicletotalDataframe1<-vehicletotalDataframe[!(vehicletotalDataframe$Type=="")&!(vehicletotalDataframe$Type=="NA"),]
  Type<-as.character(vehicletotalDataframe1$Type)
  vehicletotalDataframe1<-vehicletotalDataframe1[order(vehicletotalDataframe1$No.,decreasing = T),]
  vehicletotalDataframe1$Type<-factor(vehicletotalDataframe1$Type,levels = vehicletotalDataframe1$Type)
  return(ggplot(data=vehicletotalDataframe1,aes(x=Type,y=No.,group=1,label=vehicletotalDataframe1$No.,fill=Type))+theme(axis.text.x=element_text(angle=45,hjust=1))+geom_bar(stat="identity")+geom_text(position=position_stack(vjust=0.5),color="red")+geom_point()+geom_line(size=1))}
#plot time analysis
plot_time_analysis<-function(q){
  colnames(q)<-tolower(colnames(q))
  timeframe<-data.frame(Ourtime=strptime(tolower(q$time),"%I:%M %p"))
  Timeframe<-cbind(q,timeframe)
  timeframee<-na.omit(Timeframe)
  a<-strptime("06:00 AM","%I:%M %p")
  b<-strptime("12:00 PM","%I:%M %p")
  c<-strptime("06:00 PM","%I:%M %p")
  d<-strptime("11:59:59 PM","%I:%M:%S %p")
  e<-strptime("12:00:01 AM","%I:%M:%S %p")
  dataframe<-data.frame(TimeDistribution=c("6AMto12PM","12PMto6PM","6PMto12AM","12AMto6AM"),No.ofIncident=c(nrow(timeframee[timeframee$Ourtime>=a & timeframee$Ourtime<b,]),nrow(timeframee[timeframee$Ourtime>=b & timeframee$Ourtime<c,]),nrow(timeframee[timeframee$Ourtime>=c & timeframee$Ourtime<=d,]),nrow(timeframee[timeframee$Ourtime>=e & timeframee$Ourtime<a,])))
  dataframe<-dataframe[order(dataframe$No.ofIncident,decreasing = T),]
  dataframe$TimeDistribution<-factor(dataframe$TimeDistribution,levels = dataframe$TimeDistribution)
  return(return(ggplot(data=dataframe,aes(x=TimeDistribution,y=No.ofIncident,group=1,label=dataframe$No.ofIncident,fill=TimeDistribution))+geom_bar(stat="identity")+geom_text(position=position_stack(vjust=0.5),color="black")+geom_point()+geom_line(size=1.5)))
}
#Twelve AM to six am
TwelveAMtosixAM_analysis<-function(q){
  colnames(q)<-tolower(colnames(q))
  SixTwelve<-data.frame(Our_time=strptime(tolower(q$time),"%I:%M %p"))
  SixTwelve<-cbind(q,SixTwelve)
  SixTwelve<-na.omit(SixTwelve)
  vector1<-strptime("12:01 AM","%I:%M %p")
  vector2<-strptime("6:00 PM","%I:%M %p")
  required<-data.frame(Type.of.accident=SixTwelve[SixTwelve$Our_time>=vector1&SixTwelve$Our_time<=vector2,"type.of.accident"])
  required$Type.of.accident<-tolower(required$Type.of.accident)
  requiredtable<-table(required)
  requiredframe<-as.data.frame(requiredtable)
  names(requiredframe)<-c("Type","No.of.incident")
  return(requiredframe)
}
#Twelve pmto six pm
TwelvetoSix_analysis<-function(q){
  colnames(q)<-tolower(colnames(q))
  SixTwelve<-data.frame(Our_time=strptime(tolower(q$time),"%I:%M %p"))
  SixTwelve<-cbind(q,SixTwelve)
  SixTwelve<-na.omit(SixTwelve)
  vector1<-strptime("12:00 PM","%I:%M %p")
  vector2<-strptime("6:00 PM","%I:%M %p")
  required<-data.frame(Type.of.accident=SixTwelve[SixTwelve$Our_time>vector1&SixTwelve$Our_time<=vector2,"type.of.accident"])
  required$Type.of.accident<-tolower(required$Type.of.accident)
  requiredtable<-table(required)
  requiredframe<-as.data.frame(requiredtable)
  names(requiredframe)<-c("Type","No.of.incident")
  return(requiredframe)
}
#six am to twelve 
SixtoTwelve_analysis<-function(q){
  colnames(q)<-tolower(colnames(q))
  SixTwelve<-data.frame(Our_time=strptime(tolower(q$time),"%I:%M %p"))
  SixTwelve<-cbind(q,SixTwelve)
  SixTwelve<-na.omit(SixTwelve)
  vector1<-strptime("6:00 AM","%I:%M %p")
  vector2<-strptime("12:00 PM","%I:%M %p")
  required<-data.frame(Type.of.accident=SixTwelve[SixTwelve$Our_time>vector1&SixTwelve$Our_time<=vector2,"type.of.accident"])
  required$Type.of.accident<-tolower(required$Type.of.accident)
  requiredtable<-table(required)
  requiredframe<-as.data.frame(requiredtable)
  names(requiredframe)<-c("Type","No.of.incident")
  return(requiredframe)
}
#Six pmto twelve
SixPMtoTwelveAManalysis<-function(q){
  colnames(q)<-tolower(colnames(q))
  SixTwelve<-data.frame(Our_time=strptime(tolower(q$time),"%I:%M %p"))
  SixTwelve<-cbind(q,SixTwelve)
  SixTwelve<-na.omit(SixTwelve)
  vector1<-strptime("6:00 PM","%I:%M %p")
  vector2<-strptime("11:59 PM","%I:%M %p")
  required<-data.frame(Type.of.accident=SixTwelve[SixTwelve$Our_time>vector1&SixTwelve$Our_time<=vector2,"type.of.accident"])
  required$Type.of.accident<-tolower(required$Type.of.accident)
  requiredtable<-table(required)
  requiredframe<-as.data.frame(requiredtable)
  names(requiredframe)<-c("Type","No.of.incident")
  return(requiredframe)
}
# plot six pm totwelve
plot_SixPMtoTwelveAM<-function(q){
  colnames(q)<-tolower(colnames(q))
  SixTwelve<-data.frame(Our_time=strptime(tolower(q$time),"%I:%M %p"))
  SixTwelve<-cbind(q,SixTwelve)
  SixTwelve<-na.omit(SixTwelve)
  vector1<-strptime("6:00 PM","%I:%M %p")
  vector2<-strptime("11:59 PM","%I:%M %p")
  required<-data.frame(Type.of.accident=SixTwelve[SixTwelve$Our_time>=vector1&SixTwelve$Our_time<=vector2,"type.of.accident"])
  required$Type.of.accident<-tolower(required$Type.of.accident)
  requiredtable<-table(required)
  return(barplot(requiredtable,las=2,cex.names = .6,ylim=range(0:50)))}
  
# plot six  am to twelve
plot_SixAmtoTwelve<-function(q){
colnames(q)<-tolower(colnames(q))
SixTwelve<-data.frame(Our_time=strptime(tolower(q$time),"%I:%M %p"))
SixTwelve<-cbind(q,SixTwelve)
SixTwelve<-na.omit(SixTwelve)
vector1<-strptime("6:00 AM","%I:%M %p")
vector2<-strptime("12:00 PM","%I:%M %p")
required<-data.frame(Type.of.accident=SixTwelve[SixTwelve$Our_time>=vector1&SixTwelve$Our_time<vector2,"type.of.accident"])
required$Type.of.accident<-tolower(required$Type.of.accident)
requiredtable<-table(required)
return(barplot(requiredtable,las=2,cex.names = .6,ylim=range(0:50)))}
#plot twelve to six pm
plot_Twelvetosix<-function(q){
  colnames(q)<-tolower(colnames(q))
  SixTwelve<-data.frame(Our_time=strptime(tolower(q$time),"%I:%M %p"))
  SixTwelve<-cbind(q,SixTwelve)
  SixTwelve<-na.omit(SixTwelve)
  vector1<-strptime("12:00 PM","%I:%M %p")
  vector2<-strptime("6:00 PM","%I:%M %p")
  required<-data.frame(Type.of.accident=SixTwelve[SixTwelve$Our_time>=vector1&SixTwelve$Our_time<vector2,"type.of.accident"])
  required$Type.of.accident<-tolower(required$Type.of.accident)
  requiredtable<-table(required)
  return(barplot(requiredtable,las=2,cex.names = .6,ylim=range(0:50)))}
#plot twelve to six am 
plot_TwelveAMtosixAM<-function(q){
  colnames(q)<-tolower(colnames(q))
  SixTwelve<-data.frame(Our_time=strptime(tolower(q$time),"%I:%M %p"))
  SixTwelve<-cbind(q,SixTwelve)
  SixTwelve<-na.omit(SixTwelve)
  vector1<-strptime("12:01 AM","%I:%M %p")
  vector2<-strptime("6:00 PM","%I:%M %p")
  required<-data.frame(Type.of.accident=SixTwelve[SixTwelve$Our_time>=vector1&SixTwelve$Our_time<=vector2,"type.of.accident"])
  required$Type.of.accident<-tolower(required$Type.of.accident)
  requiredtable<-table(required)
  return(barplot(requiredtable,las=2,cex.names = .6,ylim=range(0:50)))}
