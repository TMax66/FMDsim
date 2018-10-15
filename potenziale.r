potenziale=function(rischio,totale.animali,distanze.indice#beta1=1,beta2=1
,probs=.999)
 {

 pot=(rep(totale.animali[caso.indice],length(totale.animali))*totale.animali)/distanze.indice
 pot[caso.indice]=NA
 pot.norm=(pot-min(pot,na.rm=T))/(max(pot,na.rm=T)-min(pot,na.rm=T))
 linear.pred=rbeta.pert(1,0.01,1,1.2)*pot.norm+rbeta.pert(1,0.01,1,1.2)*rischio
 logistic.prop=exp(linear.pred)/(1+exp(linear.pred))
 nuovi.casi1=as.character(codall[which(logistic.prop>quantile(logistic.prop,probs=runif(1,probs,1),
 na.rm=T)),drop=T])
 #nuovi.casi1=factor(nuovi.casi1)
# nuovi.casi1=as.character(codall[which(logistic.prop>quantile(logistic.prop,probs=runif(1,probs,1),
 #na.rm=T)),drop=T])
 nuovi.casi1=as.character(nuovi.casi1)
 nuovi.casi1
 }
 
