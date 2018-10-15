fix.elem.rid<-function(dati)
{

df<-dati
df$totale.animali<-apply(df[, c('nbov','nsuis','novic')], 1, sum)
df<-subset(df, df$totale.animali>0)
df$herdtype=factor(ifelse(df[,'nbov']>0.8*(df$totale.animali),"B",
ifelse(df[,'nsuis']>0.8*(df$totale.animali),"S",ifelse(df[, 'novic']>0.8*(df$totale.animali),"O","M"))))
df$tipologia=factor(ifelse(df$herdtype=='B'&df$totale.animali>400,'BL',
ifelse(df$herdtype=='B'&df$totale.animali<=400,'BS',
ifelse(df$herdtype=='S'&df$totale.animali>1000,'SL',
ifelse(df$herdtype=='S'&df$totale.animali<=1000,'SS','OTHER')))))
return(df)
}
