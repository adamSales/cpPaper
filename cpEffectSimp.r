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

exirt <- stud[match(cpDat$field_id,stud$field_id),grep('Exirt2',names(stud))]
names(exirt) <- gsub('_0','_',names(exirt))
for(n in names(exirt)) cpDat[[n]] <- exirt[[n]]


#### models!!
mod1 <- lmer(gainScore~ncp+state+year+(1|classid2)+(1|schoolid2),data=cpDat)
mod1.1 <- update(mod1,.~.+race+sex+grade+spec_speced+spec_gifted+spec_esl+frl)
mod2 <- lmer(gainScore~year*(ncp)+state+(1|classid2)+(1|schoolid2),data=cpDat)
mod2.1 <- update(mod2,.~.+race+sex+grade+spec_speced+spec_gifted+spec_esl+frl)
mod3 <- lmer(gainScore~everCP+state+year+(1|classid2)+(1|schoolid2),data=cpDat)
mod3.1 <- update(mod3,.~.+race+sex+grade+spec_speced+spec_gifted+spec_esl+frl)
mod4 <- lmer(gainScore~ncpCat+state+year+(1|classid2)+(1|schoolid2),data=cpDat)
mod4.1 <- update(mod4,.~.+race+sex+grade+spec_speced+spec_gifted+spec_esl+frl)
cpDat$Year=as.factor(cpDat$year)
mod5 <- lmer(gainScore~everCP*Year+state+(1|classid2)+(1|schoolid2),data=cpDat)
mod5.1 <- update(mod5,.~.+race+sex+grade+spec_speced+spec_gifted+spec_esl+frl)
mod6 <- lmer(gainScore~everCP*Year+state+(1|classid2)+(everCP|schoolid2),data=cpDat)
mod6.1 <- update(mod6,.~.+race+sex+grade+spec_speced+spec_gifted+spec_esl+frl)
mod7 <- lmer(gainScore~everCP+year+state+(everCP|classid2)+(1|schoolid2),data=cpDat)
mod7.1 <- update(mod7,.~.+race+sex+grade+spec_speced+spec_gifted+spec_esl+frl)

for(mm in grep('mod',ls(),value=TRUE))
    print(summary(assign(paste0(mm,'Y'),update(get(mm),y_yirt~.+pretest))))

save(list=grep('mod',ls(),value=TRUE),file='outcomeMods.RData')





miMod <- function(mod){
    ests <- NULL
    covs <- list()
    for(i in 1:20){
        formNew <- as.formula(paste0('.~.-pretest+Exirt2_',i))
        modNew <- update(mod,formNew)
        ests <- cbind(ests,fixef(modNew))
        covs[[i]] <- vcov(modNew)
    }
    return(list(ests=ests,covs=covs))
}
miPool <- function(fitMods){
    ests <- fitMods$ests
    covs <- fitMods$covs
    interest <- grep('CP',rownames(ests),ignore.case=TRUE)
    SEs <- sqrt(rowMeans(do.call('cbind',lapply(covs,function(cc) diag(cc[interest,interest]))))+
                1.05*apply(ests[interest,],1,var))
    ests <- rowMeans(ests[interest,])
    cbind(ests,SEs)
}

mi <- function(mod){
    miPool(miMod(mod))
}

ggplot(filter(cpDat,state%in%c('TX','KY','MI')),aes(ncp,gainScore))+geom_jitter()+geom_boxplot(aes(group=ncp),alpha=0.5)+geom_smooth(method='loess')+facet_grid(year~.)

library(optmatch)
propScore <- glm(everCP~(race+sex+grade+spec_speced+spec_gifted+spec_esl+frl)*schoolid2,family=binomial,da



