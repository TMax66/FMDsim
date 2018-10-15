pp=function(x=2,caso.indice,cod.caso.indice,distanze.indice,
rbl=RBL,rbs=RBS,rss=RSS,rsl=RSL,other=OTHER,latency,frailt,probs=runif(1,.9999,1))
{
require(Hmisc,quietly=T)
set.seed(x)
day= trunc(runif(1,1,7))       ### randomizzazione giorno inzio
n.contatti=as.numeric(1)

tt=table(tipologia)

if(day!=6|day!=7) n.contatti=c(sample(rbl,tt[1],replace=T),sample(rbs,tt[2],replace=T),
             sample(rss,tt[3],replace=T),sample(rsl,tt[4],replace=T),sample(rss,tt[5],replace=T))else
             if(day==6) n.contatti=c(sample(rbl,tt[1],replace=T),sample(rbs,tt[2],replace=T),
             sample(rss,tt[3],replace=T),sample(rsl,tt[4],replace=T),sample(rss,tt[5],replace=T))/3 else
             n.contatti=c(sample(rbl,tt[1],replace=T),sample(rbs,tt[2],replace=T),
             sample(rss,tt[3],replace=T),sample(rsl,tt[4],replace=T),sample(rss,tt[5],replace=T))/5


## primo giorno
numero.gg=1
 
rischio=nuovo.rischio(n.contatti,caso.indice)

set.seed(x+day+as.numeric(substr(date(),18,19))+runif(1,10,100))
if(rbinom(1,1,.05)==1)aa=unique(c(cod.caso.indice,
as.character(potenziale(rischio,totale.animali,distanze.indice,probs)))) else aa=cod.caso.indice

#if(length(aa)==0&numero.gg==1)aa=cod.caso.indice else aa=unique(c(cod.caso.indice,as.character(aa)))## 4 nov ore 16
latenze=latency[which(codall%in%aa)]
#if(length(aa)==1)min.latency=0 else
#min.latency=min(latency[which(codall%in%aa)],na.rm=T)## 4 nov

ll=unique(aa)

#minimi.lat=min.latency

lunghezza=length(ll)

bb=latenze[which(ll%in%c(cod.caso.indice))]
ab=ifelse(length(latenze[which(ll%nin%cod.caso.indice)])==0,0,
min(latenze[which(ll%nin%cod.caso.indice)],na.rm=T))
#altri giorni
#numero.gg=1
limite=-rbeta.pert(1,3,7,14)#-runif(1,3,7)
while((ab>-1)&(bb>limite))  ## 4 nov  prima era meno 15
{
day=day+1
set.seed(as.numeric(substr(date(),18,19)))
 if(day==8) day=1
 if(day!=6|day!=7)n.contatti=c(sample(RBL,tt[1],replace=TRUE),sample(RBS,tt[2],replace=TRUE),
             sample(RSS,tt[3],replace=TRUE),sample(RSL,tt[4],replace=TRUE),sample(RSS,tt[5],replace=TRUE))else
              if(day==6) n.contatti=c(sample(RBL,tt[1],replace=TRUE),sample(RBS,tt[2],replace=TRUE),
             sample(RSS,tt[3],replace=TRUE),sample(RSL,tt[4],replace=TRUE),sample(RSS,tt[5],replace=TRUE))/3 else
              n.contatti=c(sample(RBL,tt[1],replace=TRUE),sample(RBS,tt[2],replace=TRUE),
              sample(RSS,tt[3],replace=TRUE),sample(RSL,tt[4],replace=TRUE),sample(RSS,tt[5],replace=TRUE))/5

#dati.fin=dati.fin[which(codall%nin%ll),]# riduzione data.frame eliminando gli allevamenti infettati

rischio=nuovo.rischio(n.contatti,caso.indice)  # normalizza i n.contatti
                                               # calcola il rischio sommando frailty, susce trasm
set.seed(x+day+as.numeric(substr(date(),18,19)))
aa=potenziale(rischio,totale.animali,distanze.indice,probs)#,beta1=rbeta(1,2,1),beta2=rbeta(1,2,1))

totale.casi1=aa[which(aa%nin%ll)]

latenze1=latency[which(codall%in%c(as.character(totale.casi1)))]
if(length(totale.casi1)==0)latenze=latenze-1 else latenze=c(latenze-1,latenze1)



ll=unique(c(as.character(ll),as.character(totale.casi1)) )

#min.latency=min(latency[which(codall%in%ll)],na.rm=T)  # 4 nov

#minimi.lat=c(minimi.lat,min.latency)

lunghezza=c(lunghezza,length(ll))
numero.gg=numero.gg+1
bb=latenze[which(ll%in%c(cod.caso.indice))]
ab=ifelse(length(latenze[which(ll%nin%cod.caso.indice)])==1,0,
min(latenze[which(ll%nin%cod.caso.indice)],na.rm=T))
#latency=latenze
#rm(RBL,RBS,RSL,OTHER,RSS)
probs=probs-.0001
}
 casi.infettanti=ll[which(latenze<c(-1))]
  casi.sintomatici=ll[which(latenze<c(-3))]
llist=list(day=day,casi=ll,
#minimi.latency=minimi.lat,
lunghezza=lunghezza,latenze=latenze,casi.infettanti=casi.infettanti,
numero.gg=numero.gg)
llist
}
