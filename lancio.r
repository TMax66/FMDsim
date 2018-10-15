source('start.r')
tt1=tt2=tt3=tt4=tt5=tt6=tt7=tt8=tt9=tt10=tt11=tt12=tt13=tt14=tt15=tt16=tt17=tt18=tt19=tt20=uu=list()
jj=10

for(i in 1:jj)uu[[i]]=pp(i
,caso.indice=caso.indice,cod.caso.indice=cod.caso.indice,
distanze.indice=distanze.indice,
latency=latency,frailt=frailt,probs=.99999)#runif(1,.99,.9999))



for(i in 1:jj)
{
for(k in .99999)#seq(.89,.92,.01) )
{
set.seed(as.numeric(substr(date(),19,19)))
tt1[[i]]=multiple.gg.sinto(x=as.numeric(substr(date(),19,19)),sinto=-7,caso.indice=caso.indice,cod.caso.indice=cod.caso.indice,distanze.indice=distanze.indice,
latency=latency,frailt=frailt,uu=uu[[i]],probs=k)

}
}

for(i in 1:jj)
{
for(k in .999)#seq(.9,.94,.05) )
{
set.seed(as.numeric(substr(date(),19,19)))
tt2[[i]]=multiple.gg.post2(x=as.numeric(substr(date(),19,19)),
risultati.pre=tt1[[i]],probs=k,sinto=-rbeta.pert(1,1,3,6),day.post=1)

}
}

for(i in 1:jj)
{
for(k in .995)#seq(.89,.92,.01) )
{
set.seed(as.numeric(substr(date(),19,19)))
tt3[[i]]=multiple.gg.post2(x=as.numeric(substr(date(),19,19)),
risultati.pre=tt2[[i]],probs=k,sinto=-rbeta.pert(1,1,2,5),day.post=2)

}
}

for(i in 1:jj)
{
for(k in .996)#seq(.8,.92,.05) )
{
set.seed(as.numeric(substr(date(),19,19)))
tt4[[i]]=multiple.gg.post2(x=as.numeric(substr(date(),19,19)),
risultati.pre=tt3[[i]],probs=k,sinto=-rbeta.pert(1,1,2,5),day.post=2)

}
}
rm(tt3)
for(i in 1:jj)
{
for(k in .996)#seq(.96,.97,.001) )
{
set.seed(as.numeric(substr(date(),19,19)))
tt5[[i]]=multiple.gg.post2(x=as.numeric(substr(date(),19,19)),
risultati.pre=tt4[[i]],probs=k,sinto=-rbeta.pert(1,1,2,4),day.post=2)

}
}
rm(tt4)
for(i in 1:jj)
{
for(k in .9965)#seq(.96,.97,.001) )
{
set.seed(as.numeric(substr(date(),19,19)))
tt6[[i]]=multiple.gg.post2(x=as.numeric(substr(date(),19,19)),
risultati.pre=tt5[[i]],probs=k,sinto=-rbeta.pert(1,1,2,4),day.post=2)

}
}
rm(tt5)
for(i in 1:jj)
{
for(k in .99654)#seq(.96,.97,.001) )
{
set.seed(as.numeric(substr(date(),19,19)))
tt7[[i]]=multiple.gg.post2(x=as.numeric(substr(date(),19,19)),
risultati.pre=tt6[[i]],probs=k,sinto=-rbeta.pert(1,1,2,4),day.post=2)

}
}
rm(tt6)
for(i in 1:jj)
{
for(k in .99653)#seq(.96,.97,.001) )
{
set.seed(as.numeric(substr(date(),19,19)))
tt8[[i]]=multiple.gg.post2(x=as.numeric(substr(date(),19,19)),
risultati.pre=tt7[[i]],probs=k,sinto=-rbeta.pert(1,1,2,4),day.post=2)

}
}
rm(tt7)
for(i in 1:jj)
{
for(k in .99654)#seq(.96,.97,.001) )
{
set.seed(as.numeric(substr(date(),19,19)))
tt9[[i]]=multiple.gg.post2(x=as.numeric(substr(date(),19,19)),
risultati.pre=tt8[[i]],probs=k,sinto=-rbeta.pert(1,1,2,4),day.post=2)

}
}
rm(tt8)
for(i in 1:jj)
{
for(k in .99999)#seq(.96,.97,.001) )
{
set.seed(as.numeric(substr(date(),19,19)))
tt10[[i]]=multiple.gg.post2(x=as.numeric(substr(date(),19,19)),
risultati.pre=tt9[[i]],probs=k,sinto=-rbeta.pert(1,1,2,4),day.post=2)

}
}
rm(tt9)
for(i in 1:jj)
{
for(k in .99999)#seq(.96,.97,.001) )
{
set.seed(as.numeric(substr(date(),19,19)))
tt11[[i]]=multiple.gg.post2(x=as.numeric(substr(date(),19,19)),
risultati.pre=tt10[[i]],probs=k,sinto=-rbeta.pert(1,1,2,4),day.post=2)

}
}

rm(tt10)
for(i in 1:jj)
{
for(k in .99999)#seq(.96,.97,.001) )
{
set.seed(as.numeric(substr(date(),19,19)))
tt12[[i]]=multiple.gg.post2(x=as.numeric(substr(date(),19,19)),
risultati.pre=tt11[[i]],probs=k,sinto=-rbeta.pert(1,1,2,4),day.post=2)

}
}
rm(tt11)
for(i in 1:jj)
{
for(k in .99999)#seq(.96,.97,.001) )
{
set.seed(as.numeric(substr(date(),19,19)))
tt13[[i]]=multiple.gg.post2(x=as.numeric(substr(date(),19,19)),
risultati.pre=tt12[[i]],probs=k,sinto=-rbeta.pert(1,1,2,4),day.post=2)

}
}
rm(tt12)
for(i in 1:jj)
{
for(k in .99999)#seq(.96,.97,.001) )
{
set.seed(as.numeric(substr(date(),19,19)))
tt14[[i]]=multiple.gg.post2(x=as.numeric(substr(date(),19,19)),
risultati.pre=tt13[[i]],probs=k,sinto=-rbeta.pert(1,1,2,4),day.post=2)

}
}
rm(tt13)
for(i in 1:jj)
{
for(k in .99999)#seq(.96,.97,.001) )
{
set.seed(as.numeric(substr(date(),19,19)))
tt15[[i]]=multiple.gg.post2(x=as.numeric(substr(date(),19,19)),
risultati.pre=tt14[[i]],probs=k,sinto=-rbeta.pert(1,1,2,4),day.post=2)

}
}
rm(tt14)
for(i in 1:jj)
{
for(k in .99999)#seq(.96,.97,.001) )
{
set.seed(as.numeric(substr(date(),19,19)))
tt16[[i]]=multiple.gg.post2(x=as.numeric(substr(date(),19,19)),
risultati.pre=tt15[[i]],probs=k,sinto=-rbeta.pert(1,1,2,4),day.post=2)

}
}
rm(tt15)
for(i in 1:jj)
{
for(k in .99999)#seq(.96,.97,.001) )
{
set.seed(as.numeric(substr(date(),19,19)))
tt17[[i]]=multiple.gg.post2(x=as.numeric(substr(date(),19,19)),
risultati.pre=tt16[[i]],probs=k,sinto=-rbeta.pert(1,1,2,4),day.post=2)

}
}
rm(tt16)
for(i in 1:jj)
{
for(k in .99999)#seq(.96,.97,.001) )
{
set.seed(as.numeric(substr(date(),19,19)))
tt18[[i]]=multiple.gg.post2(x=as.numeric(substr(date(),19,19)),
risultati.pre=tt17[[i]],probs=k,sinto=-rbeta.pert(1,1,2,4),day.post=2)

}
}
rm(tt17)
for(i in 1:jj)
{
for(k in .99999)#seq(.96,.97,.001) )
{
set.seed(as.numeric(substr(date(),19,19)))
tt19[[i]]=multiple.gg.post2(x=as.numeric(substr(date(),19,19)),
risultati.pre=tt18[[i]],probs=k,sinto=-rbeta.pert(1,1,2,4),day.post=2)

}
}
rm(tt18)
for(i in 1:jj)
{
for(k in .99999)#seq(.96,.97,.001) )
{
set.seed(as.numeric(substr(date(),19,19)))
tt20[[i]]=multiple.gg.post2(x=as.numeric(substr(date(),19,19)),
risultati.pre=tt19[[i]],probs=k,sinto=-rbeta.pert(1,1,2,4),day.post=2)

}
}
