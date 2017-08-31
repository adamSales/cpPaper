data <- read.csv("totFINAL2.csv")
###NOTES: 'overall' field is either Standard or Customized
data$overall=as.factor(data$overall)
####################################### START HERE #######################
####################################### FIGURE 1 ######################### 

##NUMBER OF CURRICULUM BY STUDENT
curr1=subset(data,select=c(field_id,state,year))
curr1=curr1[!duplicated(curr1), ]
curriculum=aggregate(curr1$field_id, by=list(state=curr1$state,year=curr1$year), FUN=length)
names(curriculum)[3]<-paste("students")

### AGGREGATE NUMBER OF SECTIONS WORKED BY STUDENTS WITHIN EACH CURRICULUM
curr2=subset(data,select=c(state,year,section))
curriculum2=aggregate(curr2$section, by=list(state=curr2$state,year=curr2$year), FUN=length)
names(curriculum2)[3]<-paste("totalsections")

curriculum3=merge(curriculum,curriculum2)
curriculum3$avg=curriculum3$totalsections/curriculum3$students
#write.csv(curriculum3,"testAVGBYSTATE.csv")
library(ggplot2)

curriculum3$year1=levels(curriculum3$year)

ggplot(curriculum3, aes(state, avg,group=year)) + 
  geom_bar(stat="identity", position="dodge") +
  theme(text = element_text(size=20)) +
  ggtitle("Average Sections Attempted for Each Student by State") +
  labs(x="State",y="Average Sections") + 
  scale_fill_manual(name="Year",breaks=c(1,2),labels=c("Year 1", "Year 2"),values = c("blue", "black"))+
  scale_color_manual(name="Year", breaks=c("1", "2"),
          labels=c("Year 1", "Year 2"),values = c("blue", "black")) 

####################################### FIGURE 1 ######################### 
  
  
  
####################################### TIME SPENT ON SECTION - FIGURE 2-5 
  
timespentdata=subset(data,status=="graduated")
unique(timespentdata$status)
timespentdata=subset(timespentdata,select=c("state","overall","year","timeonsection"))  
timespentdata=timespentdata[complete.cases(timespentdata),]
avgtimespent=aggregate(list(avgtime=timespentdata$timeonsection), by=list(state=timespentdata$state,overall=timespentdata$overall,year=timespentdata$year), FUN=mean)

avgtimespent1=subset(avgtimespent,year==2)

avgtimespent1 = avgtimespent1[with(avgtimespent1, order(state, -overall)), ]

ggplot(avgtimespent1, aes(x=state, y=avgtime,fill=overall)) +
         geom_bar(stat="identity",position="dodge") +
         theme(text = element_text(size=20)) +
         ggtitle("Average Days Spent on Graduated Sections By State (Year 2)") +
         labs(x="State",y="Average Days Per Section") 

timespentdata=subset(data,status!="graduated")
unique(timespentdata$status)
timespentdata=subset(timespentdata,select=c("state","overall","year","timeonsection"))  
timespentdata=timespentdata[complete.cases(timespentdata),]
avgtimespent=aggregate(list(avgtime=timespentdata$timeonsection), by=list(state=timespentdata$state,overall=timespentdata$overall,year=timespentdata$year), FUN=mean)

####################################### TIME SPENT ON SECTION - FIGURE 2-5 


################## CP #####################################
#### AVG TIME SPENT ON FIRST 6 SECTIONS DIVIDED BY STATUS - Figure 8

timespent6=subset(data,(unit_sequence>0 & unit_sequence<7) & year==1 & overall=='Standard')
timespent6=subset(timespent6,select=c("state","overall","year","status","timeonsection"))  
timespent6=timespent6[complete.cases(timespent6),]
avgtimespent6=aggregate(list(avgtime=timespent6$timeonsection), by=list(state=timespent6$state,overall=timespent6$overall,year=timespent6$year,status=timespent6$status), FUN=mean)
avgtimespent6=subset(avgtimespent6,status!="final_or_incomplete")

ggplot(avgtimespent6, aes(x=state, y=avgtime,fill=status)) +
  geom_bar(stat="identity",position="dodge") +
  theme(text = element_text(size=20)) +
  ggtitle("Average Days Spent on First 6 Sections By State and Status (Year 1)") +
  labs(x="State",y="Average Days") 
#### AVG TIME SPENT ON FIRST 6 SECTIONS DIVIDED BY STATUS - Figure 8



#### HOW MANY CP ON THE SAME DAY?
datacp=subset(data,status=="changed placement")
datacp1=subset(datacp,select=c("state","overall","year","date1","class","field_id"))

datacp2=aggregate(list(numofstudents=datacp1$field_id), by=list(state=datacp1$state,overall=datacp1$overall,year=datacp1$year), FUN=length)


datacp2=aggregate(list(numofstudents=datacp1$field_id), by=list(state=datacp1$state,overall=datacp1$overall,year=datacp1$year,class=datacp1$class,date1=datacp1$date1), FUN=length)
datacp2=subset(datacp2,numofstudents!=1)

datacp2$group=paste(datacp2$overall," - Year ",datacp2$year)
datacp2$group1<-factor(datacp2$group,levels=c("Standard  - Year  1","Standard  - Year  2","Customized  - Year  2"))

ggplot(datacp2, aes(x=factor(group), y=numofstudents)) + geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
  labs(x="Curriculum / Year", y=paste("Number of CP Students")) + geom_point(position = position_jitter(width = 0.2)) +
  ggtitle("Number of Students CP on Same Day and in Same Class") +
  theme(text = element_text(size=20))
  
  
aggregate(list(numofstudents=datacp2$numofstudents), by=list(state=datacp2$state), FUN=mean)
### MAX NUMBER OF STUDENTS CP ON THE SAME DAY
x=aggregate(list(result=datacp2$numofstudents), by=list(state=datacp2$state,overall=datacp2$overall,year=datacp2$year), FUN=max)
x=x[with(x,order(state,overall,year)),]
x


######## SD OF CURRENT VERSUS NEXT UNIT FOR CP STUDENTS - Year 1 and Algebra ###########
datacp=subset(data,status=="changed placement" & curriculum=="algebra i" & year==1)
datacp1=subset(datacp,select=c("state","date1","teach","field_id","unit_sequence","next_unit_diff"))
datacp1$next_unit=datacp1$unit_sequence+datacp1$next_unit_diff

datacp1=datacp1[complete.cases(datacp1),]


datacp2=aggregate(datacp1$next_unit_diff, by=list(state=datacp1$state,teachid2=datacp1$teach,date=datacp1$date1), FUN=sd)
datacp3=aggregate(datacp1$field_id, by=list(state=datacp1$state,teachid2=datacp1$teach,date=datacp1$date1), FUN=length)
datacp4=aggregate(datacp1$next_unit, by=list(state=datacp1$state,teachid2=datacp1$teach,date=datacp1$date1), FUN=sd)
datacp5=aggregate(datacp1$unit_sequence, by=list(state=datacp1$state,teachid2=datacp1$teach,date=datacp1$date1), FUN=sd)
datacp1$diffabs=abs(datacp1$next_unit_diff)
datacp6=aggregate(datacp1$diffabs, by=list(state=datacp1$state,teachid2=datacp1$teach,date=datacp1$date1), FUN=mean)

colnames(datacp2)[4] <- "SD of Unit Difference"
colnames(datacp3)[4] <- "TotalCPStudentsByDay"
colnames(datacp4)[4] <- "SD of Next Section"
colnames(datacp5)[4] <- "SD of CP Section"
colnames(datacp6)[4] <- "Mean of Abs Value of Diff"

datacp23=merge(datacp2,datacp3)  ###
datacp234=merge(datacp23,datacp4)  ###
datacp2345=merge(datacp234,datacp5)  ###
datacp23456=merge(datacp2345,datacp6)  ###

#### NA means only 1 student
datacp23456=datacp23456[complete.cases(datacp23456),]

datacp23456a=aggregate(datacp23456[,c(6:8)], by=list(state=datacp23456$state), FUN=mean)
test=subset(datacp23456,"SD of Current Section" ==0.000 & "SD of Next Section" ==0.000 )
#write.csv(datacp23456,"datacp23456.csv")
datacp23456a=aggregate(datacp23456[,c(6:8)], by=list(state=datacp23456$state), FUN=mean)



#### AVG TIME SPENT ON ALL SECTIONS DIVIDED BY STATUS - Figure 12 and 13

timespent6=subset(data,select=c("state","overall","year","status","timeonsection"))  
timespent6=timespent6[complete.cases(timespent6),]
avgtimespent6=aggregate(list(avgtime=timespent6$timeonsection), by=list(state=timespent6$state,overall=timespent6$overall,year=timespent6$year,status=timespent6$status), FUN=mean)
avgtimespent6=subset(avgtimespent6,status!="final_or_incomplete")

avgtimespent7=subset(avgtimespent6,year==1)

ggplot(avgtimespent7, aes(x=state, y=avgtime,fill=status)) +
  geom_bar(stat="identity",position="dodge") +
  theme(text = element_text(size=20)) +
  ggtitle("Average Days Spent on All Sections By State and Status (Year 1)") +
  labs(x="State",y="Average Days") 

####################### FIGURE 14 #########################################
library(lattice)
datacp2=aggregate(datacp1$next_unit_diff, by=list(state=datacp1$state,date1=datacp1$date1), FUN=mean)
names(datacp2)=c("State","Date","Avg_Unit_Difference_for_CP_Students")
xyplot(datacp2$avg_unit_diff_CPstudents ~ datacp2$date1 | datacp2$State, datacp2, groups = datacp2$state, pch= 20)
xyplot(Avg_Unit_Difference_for_CP_Students ~ Date | State, datacp2, groups = datacp2$State, pch= 20)
