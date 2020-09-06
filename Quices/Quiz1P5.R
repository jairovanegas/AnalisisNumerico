iterativo<-function(n, e){
  options(digits = 22)
  i<-n
  k<-(1/i)+(i/2)
  er<-abs(k-i)
  while(er>e){
    i<-k
    k<-(1/i)+(i/2)
    er<-abs(k-i)
  }
  return(k)
}

iterativo(2, 1.1277376112344212e-12)

p5<-function(){
  options(digits = 22)
  techo<- 1e-14
  step<- -1e-16
  piso<- 1e-16
  pool<-seq(techo, piso, step)
  s1<-seq(techo, piso, step)
  s2<-seq(techo, piso, step)
  s3<-seq(techo, piso, step)
  i<-1
  for(n in pool){
    #Cuando n tiende a valores muy cercanos a cero pierde significancia y se elimina
    s1[i]<-(1-(1-n)^3)/n
    s2[i]<-n^2-3*n+3 #Formas alternativas
    s3[i]<-(n-3)*n+3
    i<-i+1
  }
  t <- data.frame(n, s1, s2, s3)
  print(t)
}

p5()

0.5e-8 == 0.000000005
install.packages("Rmpfr")
