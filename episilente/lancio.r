source('start.r')
tt=uu=list()
jj=1


for(i in 1:jj)
uu[[i]]=pp(as.numeric(substr(date(),19,19)),caso.indice=caso.indice,cod.caso.indice=cod.caso.indice,
distanze.indice=distanze.indice,
latency=latency,frailt=frailt,probs=.99999)


set.seed(as.numeric(substr(date(),19,19)))
for(i in 1:jj)
tt[[i]]=multiple.gg.free(x=as.numeric(substr(date(),19,19)),caso.indice=caso.indice,cod.caso.indice=cod.caso.indice,distanze.indice=distanze.indice,
latency=latency,frailt=frailt,uu=uu[[i]],probs=.99)
