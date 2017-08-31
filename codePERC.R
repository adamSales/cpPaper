#### Percentage of class who graduated/promoted/cp  #####
#dataALL=totFINAL2
dataALL <- read.csv("totFINAL2.csv")
data <- subset(dataALL,curriculum=="algebra i" & version=="2007" & date1>=0)
data1 <- data

#### TO FIND OUT HOW MANY STUDENTS COMPLETED SECTION IN THE SAME CLASS BEFORE THE STUDENT
#### COMPLETED IT? DIVIDE UP BY STATUS. TOTAL REPRESENTS ONLY STUDENTS WHO COMPLETED
#### SECTION.

if (TRUE){
for (i in 1:nrow(data)) {
#for (i in 1:10) {

  id_search=data$field_id[i]
  class_search=data$class[i]
  section_search=data$section[i]
  year_search=data$year[i]
  date_search=data$date1[i]
  tot=subset(data,class==class_search & section==section_search & year==year_search ,select=c(status,date,class,field_id,date1,section,year))
  x=subset(data,class==class_search & section==section_search & year==year_search & date1<date_search ,select=c(status,date,class,field_id,date1,section,year))
  data1$total[i]=nrow(tot)
  data1$graduatedbefore[i]=nrow(subset(x,status=='graduated'))
  data1$promotedbefore[i]=nrow(subset(x,status=='promoted'))
  data1$changeplacementbefore[i]=nrow(subset(x,status=='changed placement'))
  print (i)
}
}


#### GET PERCENTAGES
data1$perc_graduatedbefore=data1$graduatedbefore/data1$total
data1$perc_promotedbefore=data1$promotedbefore/data1$total
data1$perc_changedplacementbefore=data1$changeplacementbefore/data1$total
data1$perc_totalbefore=data1$perc_changedplacementbefore+data1$perc_promotedbefore+data1$perc_graduatedbefore

mean(data1$perc_totalbefore)

xxx=aggregate(data1[,c(43,44,45)], by=list(class=data1$class,section=data1$section,total=data1$total), FUN=mean)
xxx1=aggregate(xxx[,c(4,5,6)], by=list(xxx$total), FUN=mean)


###################################################
############## GRAPHS ###########################
library(reshape)
library(ggplot2)

data2=subset(data1,status=="changed placement" & curriculum=="algebra i" )
xxx=aggregate(data2[,c(43:46)], by=list(class=data2$class,unit=data2$unit,section=data2$section,sectiontotal=data2$total), FUN=mean)
states=data.frame(unique(data2$state))
### GRAPHS BY STATE
old.par <- par(mfrow=c(6, 2))

for (i in 1:nrow(states)) {
  state1=(states[i,])
  state1data=subset(data1,state==state1 )
  print(nrow(state1data))
  xxx=aggregate(state1data[,c(43:46)], by=list(class=state1data$class,unit=state1data$unit,section=state1data$section,classtotal=state1data$total), FUN=mean)
  xxx1=aggregate(xxx[,c(5:7)], by=list(unit=xxx$unit), FUN=mean)
  xxx2=subset(xxx1,select=c(1:4))
  p.dat <- melt(xxx2,id='unit')
  ggplot(data=p.dat, aes(x=unit, y=value, fill=variable)) +
    geom_bar(stat="identity") +   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = paste("Average % of Classmates Finished Before CP Students By Unit - ",state1), x = "Unit", y = "Percentage")

  xxx1=aggregate(xxx[,c(5:7)], by=list(classtotal=xxx$classtotal), FUN=mean)
  xxx2=subset(xxx1,select=c(1:4))
  p.dat <- melt(xxx2,id='classtotal')
  ggplot(data=p.dat, aes(x=classtotal, y=value, fill=variable)) +
    geom_bar(stat="identity") +   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = paste ("Average % of Classmates Finished Before CP Students By ClassTotal - ",state1), x = "Class Total", y = "Percentage")

}

### BY UNIT
xxx1=aggregate(xxx[,c(5:7)], by=list(unit=xxx$unit), FUN=mean)
xxx2=subset(xxx1,select=c(1:4))
p.dat <- melt(xxx2,id='unit')

aggregate( value ~ variable, p.dat, mean )

ggplot(data=p.dat, aes(x=unit, y=value, fill=variable)) +
  geom_bar(stat="identity") +   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Average % of Classmates Finished Before CP Students By Unit", x = "Unit", y = "Percentage")

### BY CLASSTOTAL
xxx1=aggregate(xxx[,c(5:7)], by=list(classtotal=xxx$classtotal), FUN=mean)
xxx2=subset(xxx1,select=c(1:4))
p.dat <- melt(xxx2,id='classtotal')

aggregate( value ~ variable, p.dat, mean )

ggplot(data=p.dat, aes(x=classtotal, y=value, fill=variable)) +
  geom_bar(stat="identity") +   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Average % of Classmates Finished Before CP Students By ClassTotal", x = "Class Total", y = "Percentage")

#######  NUMBER OF CPs for Students by Unit

data1cp=subset(data1,status=="changed placement")
xxx=subset(data1cp,select=c("field_id","status","unit"))
xxx1=data.frame(table(xxx$unit,xxx$field_id))
xxx1=subset(xxx1,Freq>0)
names(xxx1) = c('unit','field_id','numberbystudent')

library(dplyr)
xxx2=rename(count(xxx1, unit, numberbystudent), Freq = n)

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add

ggplot(data=xxx2, aes(x=unit, y=Freq, fill=numberbystudent)) +
  geom_bar(stat="identity") +   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Number of Students By Number of CP By Unit", x = "Unit", y = "Number of Students")

geom_text(data=xxx2, aes(x = unit, y = Freq, label = numberbystudent),size=4, colour="white")

scale_colour_brewer(cbPalette) + guides(fill=guide_legend(reverse=T))

#####  Number of CPs by Section
#dataalg=subset(data1, curriculum=="algebra i" )
#data1=subset(data,status=="changed placement" & curriculum=="algebra i" )
xxx=aggregate(status ~ field_id, data = data1cp, FUN = length)
cpbystudent1_numberofsections=aggregate(section ~ field_id, data = data1cp, FUN = length)
cpbystudent2_numberofsections<-merge(xxx,cpbystudent1_numberofsections,by=('field_id'))
cpbystudent2_numberofunits$xbysection=cpbystudent2_numberofsections$status/cpbystudent2_numberofunits$section

ggplot(data=cpbystudent2_numberofsections, aes(x=status, y=section)) + geom_bar(stat="identity")




