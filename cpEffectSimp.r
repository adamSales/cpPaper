library(dplyr)
library(lme4)
library(splines)
load('cpPaper.RData')

cpDat <- data%>%filter(!is.na(status)& status!='final_or_incomplete')%>%
 group_by(field_id,year,state,section,unit,race,sex,grade,spec_speced,xirt,spec_gifted,
  spec_esl,frl,pretest,y_yirt,classid2,schoolid2)%>%
 summarize(status=max(status,na.rm=TRUE),date=max(date),totalTime=sum(total_t1,na.rm=TRUE),
  ttNA=sum(is.na(total_t1)),nprob=n(),probNA=sum(is.na(Prob1)))%>%ungroup()%>%
 group_by(field_id,year,state,race,sex,grade,spec_speced,xirt,spec_gifted,spec_esl,frl,pretest,y_yirt,
  classid2,schoolid2)%>%
 summarize(nsec=n(),nprob=sum(nprob,na.rm=TRUE),totalTime=sum(totalTime,na.rm=TRUE),
  timeNA=sum(ttNA>0),probNA=sum(probNA>0),ncp=sum(status=='changed placement'),
  nprom=sum(status=='promoted'))%>%mutate(gainScore=y_yirt-pretest)%>%droplevels()



## mode imputation:
for(vv in c('race','sex','grade','spec_speced','spec_gifted','spec_esl','frl')){
    cpDat[[paste0(vv,'Imp')]] <- cpDat[[vv]]
    cpDat[[paste0(vv,'Imp')]][is.na(cpDat[[vv]])] <- names(which.max(table(cpDat[[vv]])))
}



cpDat <- mutate(cpDat,totalTime=totalTime/3600000,totalTime=ifelse(totalTime<0,NA,totalTime),
                totalTime=ifelse(totalTime>360,NA,totalTime))

cpDat$ncpCat <- factor(ifelse(cpDat$ncp>=3,'3+',cpDat$ncp))
cpDat$everCP <- cpDat$ncp>0

#### models!!
mod1 <- lmer(gainScore~ncp+nsec+state+year+(1|classid2)+(1|schoolid2),data=cpDat)
mod1.1 <- update(mod1,.~.+race+sex+grade+spec_speced+spec_gifted+spec_esl+frl)
mod2 <- lmer(gainScore~year*(ncp+nsec)+state+(1|classid2)+(1|schoolid2),data=cpDat)
mod2.1 <- update(mod2,.~.+race+sex+grade+spec_speced+spec_gifted+spec_esl+frl)
mod3 <- lmer(gainScore~everCP+state+year+(1|classid2)+(1|schoolid2),data=cpDat)
mod3.1 <- update(mod3,.~.+nsec)
mod3.1.1 <- update(mod3.1,.~.+race+sex+grade+spec_speced+spec_gifted+spec_esl+frl)
mod3.2 <- update(mod3,.~.+race+sex+grade+spec_speced+spec_gifted+spec_esl+frl)
mod4 <- lmer(gainScore~ncpCat+nsec+state+year+(1|classid2)+(1|schoolid2),data=cpDat)
mod4.1 <- update(mod4,.~.+race+sex+grade+spec_speced+spec_gifted+spec_esl+frl)

save(list=grep('mod',ls()),file='outcomeMods.RData')


