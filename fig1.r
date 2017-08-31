library(dplyr)
library(ggplot2)

data <- read.csv("totFINAL2.csv")
###NOTES: 'overall' field is either Standard or Customized
data$overall=as.factor(data$overall)
####################################### START HERE #######################
####################################### FIGURE 1 #########################

### boxplots of number of sections in customized and reg curriculua by state/year
## secLevel <- aggregate(data,by=list(state=data$state,curriculum=data$curriculum,field_id=data$field_id,year=data$year,section=data$section,overall=data$overall),FUN=length)
## secByStud <- aggregate(subset(secLevel,select=c(state,year,section,field_id,overall,)),
##  by=list(secLevel$field_id),FUN=length)



pload('~/Box Sync/CT/data/problemLevelUsageData/probLevelData.RData')
hs1 <- read.csv('~/Box Sync/CT/data/RANDstudyData/H1_algebra_rcal_20121119_fieldid.csv')
hs2 <- read.csv('~/Box Sync/CT/data/RANDstudyData/H2_algebra_rcal_20121119_fieldid.csv')



stopifnot(all.equal(names(hs1)[1:100],names(hs2)[1:100]))

stud <- rbind(hs1[,1:100],hs2[,1:100])
stud$field_id <- c(hs1$field_id,hs2$field_id)
stud <- stud[stud$treatment==1,]

prob <- x
prob <- prob[prob$field_id%in%stud$field_id,]
prob <- prob[!grepl('test',prob$section,ignore.case=TRUE),]

data <- merge(prob,stud,all.x=TRUE,all.y=FALSE)

data$overall <- ifelse(grepl('cc-',data$curriculum),'Customized','Standard')

stud$obs <- stud$field_id%in%x$field_id

secByStud <- summarize(group_by(data,state,year,field_id,year,overall),numSec=n_distinct(section))


secByStud$yearFac <- c('Yr 1','Yr 2')[secByStud$year]
secByStud$Curriculum <- secByStud$overall

secStateYear <- ggplot(secByStud,aes(yearFac,numSec))+geom_boxplot()+facet_grid(~state)+ylim(0,200)+labs(x='',y='Number of Sections Per Student')
ggsave('fig1.jpg',width=6,height=3)

secStateYearCurr <- ggplot(secByStud,aes(yearFac,numSec,fill=Curriculum))+geom_boxplot()+facet_grid(~state)+ylim(0,200)+labs(x='',y='Number of Sections Per Student')
ggsave('fig1a.jpg',width=6,height=3)


perCCdat <- secByStud%>% filter(state%in%c('KY','TX','MI')) %>% group_by(state,year) %>% mutate(perCC= mean(overall=='Customized'))

percCC <- ggplot(perCCdat,aes(yearFac,perCC))+geom_col()+facet_grid(~state)+labs(x='',y='% from Customized Curriculua')
ggsave('percentCC.jpg',width=6,height=3)

#### incorporate section mastery
load('~/Box Sync/CT/data/sectionLevelUsageData/advanceData.RData')
data <- merge(data,advance,all.x=TRUE,all.y=FALSE)

secByStud2 <- data %>% group_by(state,year,field_id,year,overall) %>%
 summarize(numSec=n_distinct(section),numSecGrad <- sum(status-)


secStateYear <- ggplot(subset(secByStud,status=='graduated'),aes(yearFac,numSec))+geom_boxplot()+facet_grid(~state)+ylim(0,200)+labs(x='',y='Number of Sections Per Student')
ggsave('fig1.jpg',width=6,height=3)

