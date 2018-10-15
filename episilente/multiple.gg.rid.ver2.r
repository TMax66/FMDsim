multiple.gg.giorni=function(x=as.numeric(substr(date(),18,19)),giorni=20,caso.indice,cod.caso.indice,distanze.indice,
rbl=RBL,rbs=RBS,rss=RSS,rsl=RSL,other=OTHER,latency,frailt,uu,probs=.9999999)
{
latenze=uu$latenze#uu[[1]]$latenze
ll=uu$casi#uu[[1]]$casi
lunghezza=uu$lunghezza#uu[[1]]$lunghezza
casi.infettanti=uu$casi.infettanti#uu[[1]]$casi.infettanti
numero.gg=length(uu$lunghezza)#uu[[1]]$numero.gg
#minimi.lat=uu$minimi.lat#uu[[1]]$minimi.lat
day=uu$day
#########
while(numero.gg<giorni) # giorni sono i giorni per cui si vuole far correre l'infezione dopo che oltre al caso
                        # indice ci sono altri allevamenti che propagano l'infezione
{
tt=table(tipologia)

day=day+1
set.seed(x+day)
 if(day==8) day=1
 if(day!=6|day!=7)n.contatti=c(sample(rbl,tt[1],replace=T),sample(rbs,tt[2],replace=T),
             sample(other,tt[3],replace=T),sample(rsl,tt[4],replace=T),sample(rss,tt[5],replace=T))else
              if(day==6) n.contatti=c(sample(rbl,tt[1],replace=T),sample(rbs,tt[2],replace=T),
             sample(other,tt[3],replace=T),sample(rsl,tt[4],replace=T),sample(rss,tt[5],replace=T))/3 else
              n.contatti=c(sample(rbl,tt[1],replace=T),sample(rbs,tt[2],replace=T),
              sample(other,tt[3],replace=T),sample(rsl,tt[4],replace=T),sample(rss,tt[5],replace=T))/5


casi.indice=unique(c(cod.caso.indice,as.character(casi.infettanti)) )
n.infect=length(casi.indice)
n.cod=which(codall%in%as.character(casi.indice))
pp1=rep(0,dim(dati.fin)[1])
tt=as.numeric(1)
for(i in 1:n.infect)
 {
  distanze.index=sqrt(dsquare(matrice,matrice[n.cod[i],,drop=F]))/1000
  distanze.index=ifelse(dati.fin[,1]==dati.fin[,1][n.cod[i]],distanze.index/4,distanze.index)
  #distanze.index=dist.index(matrice,n.cod[i])



  pot=(rep(totale.animali[n.cod[i]],length(totale.animali))*totale.animali)/distanze.index

  pot[n.cod]=NA
  pp1=pp1+pot
  tt=tt+transm.all[i]
}

pot.norm=(pp1-min(pp1,na.rm=T))/(max(pp1,na.rm=T)-min(pp1,na.rm=T))

n.contatti.norm=(n.contatti-min(n.contatti,na.rm=T))/(max(n.contatti,na.rm=T)-min(n.contatti,na.rm=T))

rischio=susce.norm+n.contatti.norm+frailt+tt

linear.pred=(rbeta.pert(1,0.01,1,1.2)*pot.norm+rbeta.pert(1,0.01,1,1.2)*rischio)
logistic.prop=exp(linear.pred)/(1+exp(linear.pred))
nuovi.casi1=as.character(codall[which(logistic.prop>probs
#uantile(logistic.prop,probs=runif(1,probs,1),na.rm=T)
),drop=T])
#nuovi.casi1=factor(nuovi.casi1)

#totale.casi1=nuovi.casi1
if(length(nuovi.casi1)==0)totale.casi1=c('') else
totale.casi1=as.character(nuovi.casi1[which(nuovi.casi1%nin%ll)])
rm(nuovi.casi1)
latenze1=latency[which(codall%in%c(as.character(totale.casi1)))]
if(length(totale.casi1)==0)latenze=latenze-1 else latenze=c(latenze-1,latenze1)

#min.latency=min(latenze,na.rm=T)
#ml=min.latency
ll=c(as.character(ll),as.character(totale.casi1))
ll=ll[which(ll%nin%c(''))]
casi.infettanti=ll[which(latenze<c(-1))]

lunghezza=c(lunghezza,length(ll))
numero.gg=numero.gg+1
probs=probs-0.01

}
list(ll=as.character(ll),
lunghezza=lunghezza,latenze=latenze,day=day,
casi.infettanti=casi.infettanti)
}
