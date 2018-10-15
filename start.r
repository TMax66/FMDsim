library(splancs)
library(maptools)
source('fix.elem.r')
source('fix.elem.rid.r')
source('multiple.gg.rid1.r')
source('multiple.gg.rid.ver2.r')
source('multiple.gg.sinto.r')
source('multiple.gg.free.r')
source('pp.function.r')
source('potenziale.r')
source('multiple.post2.r')
source('multiple.post3.r')
source('cerchio.r')
source('rbeta.pert.r')
source('nuovo.rischio.r')

print(date())

dati=fix.elem(dati)

dati.fin=dati[[1]]
attach(dati.fin)

caso.indice=dati$caso.indice
cod.caso.indice=dati$cod.caso.indice
distanze.indice=dati$distanze.indice
RBL=dati$RBL
RBS=dati$RBS
OTHER=dati$OTHER
RSL=dati$RSL
RSS=dati$RSS
#beta1=dati$beta1
#bata2=dati$beta2
latency=dati.fin$latency
frailt=dati.fin$frailt
matrice=dati$matrice
