fix.elem<-function(dati, tipo.all.caso='',
trasm.cattle=4.30*10e-07, trasm.sheep=2.67*10e-07,
 trasm.pigs=6.67*10e-07, susce.cattle=15.2, susce.sheep=1, susce.pigs=1#,b1=T,b2=T
 , RBLmin=0, RBLml=13, RBLmax=30,RBSmin=0, RBSml=5, RBSmax=7, RSLmin=0, RSLml=11, RSLmax=15,RSSmin=0, RSSml=6, RSSmax=15)
{

  
  trasm.cattle=4.30*10e-07
  trasm.sheep=2.67*10e-07
  trasm.pigs=6.67*10e-07
  susce.cattle=15.2
  susce.sheep=1
  susce.pigs=1
  RBLmin=0
  RBLml=13
  RBLmax=30
  RBSmin=0
  RBSml=5
  RBSmax=7
  RSLmin=0
  RSLml=11
  RSLmax=15
  RSSmin=0
  RSSml=6
  RSSmax=15
  tipo.all.caso=''
  
  
require(Hmisc,quietly=T)
set.seed(as.numeric(substr(date(),18,19)))
df<-dati
df$totale.animali<-apply(df[, c('nbov','nsuis','novic')], 1, sum)
df<-subset(df, df$totale.animali>0)
herds<-dim(df)[1]
matrice=matrix(cbind(df[,'est'], df[,'nord']), herds, 2)
df$herdtype=factor(ifelse(df[,'nbov']>0.8*(df$totale.animali),"B",
ifelse(df[,'nsuis']>0.8*(df$totale.animali),"S",ifelse(df[, 'novic']>0.8*(df$totale.animali),"O","M"))))
df$herdtrasm<-df[,'nbov']*trasm.cattle+df[,'nsuis']*trasm.pigs+df[,'novic']*trasm.sheep
df$herdsusc<-df[,'nbov']*susce.cattle+df[,'nsuis']*susce.pigs+df[,'novic']*susce.sheep
df$susce.norm=(df$herdsusc-min(df$herdsusc,na.rm=T))/(max(df$herdsusc,na.rm=T)-min(df$herdsusc,na.rm=T))
df$transm.all=(df$herdtrasm-min(df$herdtrasm,na.rm=T))/(max(df$herdtrasm,na.rm=T)-min(df$herdtrasm,na.rm=T))
#gini index#
#require(ineq)
#gini=aggregate(df$totale.animali,list(df[,10]),ineq)
#names(gini)=c("codcom","gini")
#df<-merge(df, gini, all.x=T)
df$tipologia=factor(ifelse(df$herdtype=='B'&df$totale.animali>400,'BL',
ifelse(df$herdtype=='B'&df$totale.animali<=400,'BS',
ifelse(df$herdtype=='S'&df$totale.animali>1000,'SL',
ifelse(df$herdtype=='S'&df$totale.animali<=1000,'SS','OTHER')))))
if(tipo.all.caso==''|tipo.all.caso%nin%df$tipologia)index=trunc(runif(1,1,
dim(df)[1]))else index=sample(which(df$tipologia==tipo.all.caso),1)
cod.caso.indice=as.character(df[,'codall'][index])




c1 = c(x=3, y=4)
euc.dist <- function(x1) sqrt(sum((x1 - c1) ^ 2))
set.seed(123)
mydf <- data.frame(x=runif(10), y=runif(10))
apply(mydf, 1, euc.dist)





distanze.indice=sqrt(dsquare(matrice,matrice[index,,drop=F]))/1000 # diviso per 1000













distanze.indice=ifelse(df[,2]==df[,2][index],distanze.indice/100,distanze.indice) # posto 100
df$latency=ifelse(df$herdtype=="S",rbeta.pert(herds,1,6,14),rbeta.pert(herds,1,4,14))#ifelse(df$herdtype=="S",rnorm(herds,6,.9),rnorm(herds,4,.8))
df$latency=ifelse(df$codall==cod.caso.indice,-1,df$latency)## 4 nov
df$frailt=rnorm(herds,0,.5)#ifelse((distanze.indice#/1000
#)<5,rbeta.pert(herds,0,.5,1),0)
df=df[order(df$tipologia),]
RBL=rbeta.pert(2*nrow(df),RBLmin,RBLml,RBLmax)

RBS=rbeta.pert(2*nrow(df),RBSmin,RBSml,RBSmax)

RSL=rbeta.pert(2*nrow(df),RSLmin,RSLml,RSLmax)

OTHER=RSS=rbeta.pert(2*nrow(df),RSSmin,RSSml,RSSmax)
#if(b1==F|b2==F)
#{
#beta1=rnorm(nrow(df),1,.1)
#beta2=rnorm(nrow(df),1,.1)
#}
#else
#{
#beta1=1
#beta2=1
#}

return(list(df,caso.indice=index,cod.caso.indice=cod.caso.indice,distanze.indice=distanze.indice,
RBL=RBL,RBS=RBS,OTHER=OTHER,RSS=RSS,RSL=RSL#,beta1=beta1,beta2=beta2
,matrice=matrice))
}
