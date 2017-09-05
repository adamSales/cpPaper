anitaDat <- read.csv("totFINAL2.csv")
###NOTES: 'overall' field is either Standard or Customized
anitaDat$overall=as.factor(anitaDat$overall)
                                        #
secByStudA <- summarize(group_by(anitaDat,state,year,field_id,year,overall),numSec=n_distinct(section))


summarize(group_by(secByStudA,year),avSec=mean(numSec))

subset(secByStudA,field_id%in%secByStud$field_id)%>%group_by(year)%>%summarize(avSec=mean(numSec))
