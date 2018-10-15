cerchio0=function(matrice,caso.scoperto)
{
## la funzione cerchio0 calcola la distanza tra  gli allevamenti e il caso scoperto
#matrice: coordinate allevamenti
# indice del caso infettante o scoperto
dis=sqrt(dsquare(matrice,matrice[caso.scoperto,,drop=F]))/1000
dis
}

