multiple.gg.post3=function(x=1,raggio=10,risultati.pre,#casi.scoperti=index.caso.sinto,
probs=.90,sinto=-7,day.post=1)
{
#####   raggio della zona di controllo in metri
#####   risultati: lista dei risulati precedenti
#####   caso.scoperto: # riga caso individuato
latenze=risultati.pre$latenze
ll=risultati.pre$ll
lunghezza=risultati.pre$lunghezza
casi.scoperti=ll[which(risultati.pre$latenze<sinto)]
casi.infettanti=risultati.pre$casi.infettanti[which(risultati.pre$casi.infettanti%nin%casi.scoperti)]
numero.gg=risultati.pre$numero.gg
minimi.lat=risultati.pre$minimi.lat
day=risultati.pre$day
mm=table(tipologia)
if(day.post==1)esclusi=rep(0,dim(dati.fin)[1])else
{
esclusi=risultati.pre$esclusi
casi.diag=ll[which(esclusi>0)]
casi.scoperti=unique(c(casi.scoperti,casi.diag))#codall[which(esclusi>0)]))
}
#while(numero.gg<giorni)
#{
day=day+1
set.seed(as.numeric(substr(date(),15,16)))
if(day==8) day=1
if(day!=6|day!=7)n.contatti=c(sample(RBL,mm[1],replace=TRUE),sample(RBS,mm[2],replace=TRUE),
            sample(RSS,mm[3],replace=TRUE),sample(RSL,mm[4],replace=TRUE),sample(RSS,mm[5],replace=TRUE))else
              if(day==6) n.contatti=c(sample(RBL,mm[1],replace=TRUE),sample(RBS,mm[2],replace=TRUE),
              sample(RSS,mm[3],replace=TRUE),sample(RSL,mm[4],replace=TRUE),sample(RSS,mm[5],replace=TRUE))/3 else
               n.contatti=c(sample(RBL,mm[1],replace=TRUE),sample(RBS,mm[2],replace=TRUE),
               sample(RSS,mm[3],replace=TRUE),sample(RSL,mm[4],replace=TRUE),sample(RSS,mm[5],replace=TRUE))/5
lung.casi.scopert=length(casi.scoperti)
#esclusi=rep(0,dim(dati.fin)[1])
for(i in 1:lung.casi.scopert)
{
cc=cerchio0(matrice,which(codall==casi.scoperti[1]))
esclusi=esclusi+as.numeric(cc<=raggio )
}
n.contatti=ifelse(esclusi>0,0,n.contatti)

rm(cc)
#n.contatti=ifelse(esclusi==T,n.contatti,0) # se gli allevamenti si trovano entro 5 km hanno un numero contatti uguale a 0

casi.indice=c(cod.caso.indice,as.character(casi.infettanti))
#casi.indice=casi.indice[which(casi.indice%nin%casi.scoperti)]
# la funzione multiple.cases calcola i nuovi casi infetti sommando il potenziale e il rischio per
# ciascun allevamento come somma dei potenziali e dei rischi derivanti dai diversi allevamenti divenuti casi
# infettanti
n.infect=length(casi.indice)
n.cod=which(codall%in%as.character(casi.indice))
 pp1=as.numeric(dim(dati.fin)[1])
 tt1=as.numeric(1)
 for(i in 1:n.infect)
 {
  distanze.index=sqrt(dsquare(matrice,matrice[n.cod[i],,drop=F]))/1000
  distanze.index=ifelse(dati.fin[,1]==dati.fin[,1][n.cod[i]],distanze.index/4,distanze.index)
  #distanze.index=dist.index(matrice,n.cod[i])
  pot=(rep(totale.animali[n.cod[i]],length(totale.animali))*totale.animali)/distanze.index
  pot[n.cod]=NA
  pp1=pp1+pot
  tt1=tt1+transm.all[i]
}
set.seed(as.numeric(substr(date(),18,19))+day)
pot.norm=(pp1-min(pp1,na.rm=T))/(max(pp1,na.rm=T)-min(pp1,na.rm=T))

n.contatti.norm=(n.contatti-min(n.contatti,na.rm=T))/(max(n.contatti,na.rm=T)-min(n.contatti,na.rm=T))

rischio=susce.norm+n.contatti.norm+frailt+tt1
rischio=ifelse(codall%in%casi.scoperti,0,rischio)
linear.pred=(pot.norm+rischio)
logistic.prop=exp(linear.pred)/(1+exp(linear.pred))
nuovi.casi1=as.character(codall[which(logistic.prop>probs
 #quantile(logistic.prop,probs=runif(1,probs,1),na.rm=T)
 ),drop=T])
 nuovi.casi1=factor(nuovi.casi1)
 #totale.casi1=as.numeric(length(nuovi.casi1))
totale.casi1=nuovi.casi1
totale.casi1=totale.casi1[which(totale.casi1%nin%ll)]

latenze1=latency[which(codall%in%c(as.character(totale.casi1)))]
if(length(totale.casi1)==0)latenze=latenze-1 else latenze=c(latenze-1,latenze1)

min.latency=min(latenze,na.rm=T)

ll=c(as.character(ll),as.character(totale.casi1))
#ll=ll[which(ll%nin%casi.scoperti)]
casi.infettanti=ll[which(latenze<c(-1))]
minimi.lat=c(minimi.lat,min.latency)

lunghezza=c(lunghezza,length(ll))
numero.gg=numero.gg+1
#}
list(ll=as.character(ll),minimi.lat=minimi.lat#,numero.gg=numero.gg
,lunghezza=lunghezza,day=day,esclusi=esclusi,casi.scoperti=casi.scoperti,
latenze=latenze,casi.infettanti=casi.infettanti)
}
