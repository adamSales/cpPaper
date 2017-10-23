library(dplyr)
library(lme4)
library(splines)
library(ggplot2)
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
    num <- is.numeric(cpDat[[vv]])
    cpDat[[paste0(vv,'MIS')]] <- is.na(cpDat[[vv]])
    cpDat[[vv]][is.na(cpDat[[vv]])] <- names(which.max(table(cpDat[[vv]])))
    if(num) cpDat[[vv]] <- as.numeric(cpDat[[vv]])

}

cpDat$grade <- factor(ifelse(cpDat$grade==9,'9','10+'))
levels(cpDat$race) <- list(White=c('WHITE NON-HISPANIC','ASIAN / PACIFIC ISLANDER'),Black=c('BLACK NON-HISPANIC','OTHER RACE / MULTI-RACIAL'),Hispanic=c('HISPANIC','AMERICAN INDIAN / ALASKAN NATIVE'))

cpDat <- mutate(cpDat,totalTime=totalTime/3600000,totalTime=ifelse(totalTime<0,NA,totalTime),
                totalTime=ifelse(totalTime>360,NA,totalTime))

cpDat$ncpCat <- factor(ifelse(cpDat$ncp>=4,'4+',cpDat$ncp))
cpDat$everCP <- cpDat$ncp>0

exirt <- stud[match(cpDat$field_id,stud$field_id),grep('Exirt2',names(stud))]
names(exirt) <- gsub('_0','_',names(exirt))
for(n in names(exirt)) cpDat[[n]] <- exirt[[n]]


#### models!!
## ind var: ncp
addCovs <- .~.+pretest+race+sex+grade+spec_speced+spec_gifted+spec_esl+frl+frlMIS
mod1 <- lm(y_yirt~ncp+classid2,data=cpDat)
mod1 <- lmer(y_yirt~ncp+state+year+(1|classid2)+(1|schoolid2),data=cpDat)
mod1.1 <- update(mod1,addCovs)

## ind var: ncpCat
mod2 <- lmer(y_yirt~ncpCat+state+pretest+year+(1|classid2)+(1|schoolid2),data=cpDat)
mod2.1 <- update(mod4,addCovs)

## ind var: everCP
mod3 <- lm(y_yirt~everCP+classid2,data=cpDat)
mod3 <- lmer(y_yirt~everCP+state+pretest+year+(1|classid2)+(1|schoolid2),data=cpDat)
mod3.1 <- update(mod3,addCovs)


### heterogeneity
## year
cpDat$Year=as.factor(cpDat$year)
mod4 <- lmer(y_yirt~everCP*Year+state+pretest+(1|classid2)+(1|schoolid2),data=cpDat)
mod4.1 <- update(mod6,addCovs)

## classroom
mod5 <- lmer(y_yirt~everCP+year+state+pretest+(everCP|classid2)+(1|schoolid2),data=cpDat)
mod5.1 <- update(mod7,addCovs)

## for(mm in grep('mod',ls(),value=TRUE))
##     print(summary(assign(paste0(mm,'Y'),update(get(mm),y_yirt~.+pretest))))

save(list=grep('mod',ls(),value=TRUE),file='outcomeMods.RData')



## classes with some cp
cpClass <- unique(cpDat$classid2[cpDat$everCP])
ols5.1 <- lm(y_yirt~everCP+race+sex+grade+spec_speced+spec_gifted+spec_esl+frl+pretest+classid2,
           data=cpDat,subset=classid2%in%cpClass)#)$coef['everCPTRUE',]
ols5.1.1 <- lm(y_yirt~everCP+race+sex+grade+spec_speced+spec_gifted+spec_esl+frl+ns(pretest,5)+classid2,
           data=cpDat,subset=classid2%in%cpClass)#$coef['everCPTRUE',]

ols1 <- lm(y_yirt~ncp+race+sex+grade+spec_speced+spec_gifted+spec_esl+frl+pretest+classid2,
           data=cpDat,subset=classid2%in%cpClass)
ols4 <- lm(y_yirt~ncpCat+race+sex+grade+spec_speced+spec_gifted+spec_esl+frl+pretest+classid2,
           data=cpDat,subset=classid2%in%cpClass)


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





byYear <- ggplot(cpDat,aes(ncp,gainScore))+geom_jitter()+geom_boxplot(aes(group=ncp),alpha=0.5)+geom_smooth()+facet_grid(year~.)

byState1 <- ggplot(filter(cpDat,year==1 & state %in% c('TX','KY','MI')),aes(ncp,gainScore))+geom_jitter()+geom_boxplot(aes(group=ncp),alpha=0.5)+geom_smooth(method='lm')+facet_grid(state~.)

byState2 <- ggplot(filter(cpDat,year==2 & state %in% c('TX','KY','MI')),aes(ncp,gainScore))+geom_jitter()+geom_boxplot(aes(group=ncp),alpha=0.5)+geom_smooth(method='lm')+facet_grid(state~.)



#### classroom level
classLevel <- aggregate(cpDat[,sapply(cpDat,class)%in%c('numeric','integer','logical')],
                        by=list(classid2=cpDat$classid2),FUN=mean,na.rm=TRUE)
classLevel$state <- cpDat$state[match(classLevel$classid2,cpDat$classid2)]
classLevel$schoolid2 <- cpDat$schoolid2[match(classLevel$classid2,cpDat$classid2)]


qplot(ncp,gainScore,data=classLevel)+geom_smooth()
qplot(ncp,gainScore,data=classLevel)+geom_smooth()+facet_grid(year~.)


summary(lm(gainScore~I(ncp==0)+ncp+state+year,data=classLevel))
summary(lm(gainScore~ncp+state+year,data=classLevel))
summary(lm(gainScore~I(ncp==0)+ncp+schoolid2+year,data=classLevel,subset=state!='LA'))

summary(lm(y_yirt~I(ncp==0)+ncp+ns(xirt,3)+grade+spec_speced+spec_gifted+spec_esl+frl+schoolid2+year,data=classLevel,subset=state!='LA'))

summary(lm(y_yirt~I(ncp==0)+ncp+xirt+schoolid2+year,data=classLevel,subset=state!='LA'))



qplot(ncp,pretest,data=classLevel)+geom_smooth()

### figure out negative "gifted" and "esl" coefs in mod4.1Y

## take out "npCat"
summary(update(mod4.1Y,.~.-ncpCat))

## take

## what happens if we run it on raw data?
hs1 <- read.csv('~/Box Sync/CT/data/RANDstudyData/H1_algebra_rcal_20121119_fieldid.csv')
hs2 <- read.csv('~/Box Sync/CT/data/RANDstudyData/H2_algebra_rcal_20121119_fieldid.csv')
raw <- rbind(hs1[,intersect(names(hs1),names(hs2))],hs2[,intersect(names(hs1),names(hs2))])
raw$pretest <- rowMeans(raw[,grep('Exirt2',names(raw))],na.rm=TRUE)

summary(update(mod4.1Y,.~.-ncpCat,data=raw))

### take out "pretest"
summary(update(mod4.1Y,.~.-ncpCat-pretest))
## OK that explains things


### xirt vs pretest
summary(update(mod4.1Y,.~.-ncpCat-pretest+xirt))
## weird

##components of "pretest
for(i in 1:20){
    newForm <- as.formula(paste0('.~.-ncpCat-pretest+Exirt2_',i))
    print(fixef(update(mod4.1Y,newForm))[c('spec_gifted','spec_esl')])
}

## missing xirt?
for(i in 1:20){
    newForm <- as.formula(paste0('.~.-ncpCat-pretest+Exirt2_',i))
    print(fixef(update(mod4.1Y,newForm,subset=!is.na(xirt)))[c('spec_gifted','spec_esl')])
}

### also, "pretest" predicts Y much better than xirt. cuz Y was part of estimation of "pretest"?
### I thought that's what's supposed to happen....

## does this change results of itnerest?
summary(update(mod4.1Y,.~.-pretest+xirt))
## larger effects with xirt, significance unchanged
