rbeta.pert=function(n,a,b,c)
{
 #x=x/(c-a)
 if(b==(c+a)/2)b=b+.01
 mu=(a+4*b+c)/6
 alpha1=(mu-a)*(2*b-a-c)/((b-mu)*(c-a))
  alpha2=alpha1*(c-mu)/(mu-a)
  p=rbeta( n, alpha1,  alpha2)*(c-a)+a
  p
}

