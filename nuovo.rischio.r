nuovo.rischio<-function( n.contatti,caso.indice)
 {


 n.contatti.norm=(n.contatti-min(n.contatti,na.rm=T))/(max(n.contatti,na.rm=T)-min(n.contatti,na.rm=T))

 rischio=susce.norm+n.contatti.norm+frailt+transm.all[caso.indice]

  rischio
}
