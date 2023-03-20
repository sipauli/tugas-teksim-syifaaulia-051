#Tugas Pertemuan 2 Teknik Simulasi 
#Syifa Aulia_B2A020051
#Z0=21139, a=45, m=417, c=437, n=150, p=0.83

Additive_RNG<-function(a,z0,c,m,n){
  xs<-matrix(NA,n,3)
  colnames(xs)<-c("aZ(i-1)+c","Xs","Us")
  for (s in 1:n) 
  {
    xs[s,1]<-(a*z0+c)
    xs[s,2]<-xs[s,1]%%m
    xs[s,3]<-xs[s,2]/m
    z0<-xs[s,2]
  }
  hist(xs[,3])
  View(xs)
}
Additive_RNG(45,21139,150,21139,150)

#Bernoulli_2
bernouli_2<-function(n,p){
  s<-n
  p<-p
  X<-runif(n)
  Y<-(X<=p)+0
  (tabel<-table(Y)/length(Y))
}
View(bernouli_2(150,0.83))
View(bernouli_2(6900,0.83))
View(bernouli_2(14704,0.83))

     