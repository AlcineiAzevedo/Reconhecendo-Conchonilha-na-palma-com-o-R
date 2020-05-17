remove(list=ls())
require(OpenImageR)
setwd("D:/BKP/Backup Pendrive/Estudo/Analise Imagem/palma2")
palma=readImage("palma.jpg")
#imageShow(palma)
fundo=readImage("fundo.jpg")
#imageShow(fundo)
planta=readImage("pm.jpeg")
#imageShow(planta)
concho=readImage("concho.jpg")
#imageShow(concho)

PreencherMascaraNegra =function(img2,perc,imagem=T,individual=F){
  if(imagem==T){t=img2@.Data[,,1]}
  if(imagem==F){t=img2}
  
  if(individual==F){
    
    
    n=round(perc*min(c(ncol(t),nrow(t))),0)
    
    p1=function(t){
      t2=t
      for( i in 2:(nrow(t)-n-1)){
        for(j in 1:ncol(t)){
          if(t[i,j]==1){
            if(t[i-1,j]==0){
              
              a=0
              while(a<n){
                a=a+1
                
                if(sum(t[i:(i+a),j]==1)<a){t2[i:(i+a),j]=0;a=n}
                
              }
              
            }
          }
        }
      }
      return(t2)
    }
    
    Pp=p1(t)
    Pp=p1(t(Pp))
    return(t(Pp))
  }
  
  if(individual==T){
    
    t2=Contorno(t,imagem = F)
    display(t2)
    m=cbind(expand.grid(1:nrow(t2),1:ncol(t2)),c(t2))
    m=as.matrix(m[m[,3]<1,])
    
    ind=unique(m[,1])
    for(y in 1:length(ind)){
      t2[ind[y],min(m[m[,1]==ind[y],2]):max(m[m[,1]==ind[y],2])]=0
    }
    
    
    return(t2)
    
  }
}
#

Dfundo=cbind(R=c(fundo[,,1]),G=c(fundo[,,2]),B=c(fundo[,,3]))
DPlanta=cbind(R=c(planta[,,1]),G=c(planta[,,2]),B=c(planta[,,3]))
DConcho=cbind(R=c(concho[,,1]),G=c(concho[,,2]),B=c(concho[,,3]))

Dfundo=Dfundo[sample(1:nrow(Dfundo)),]
DPlanta=DPlanta[sample(1:nrow(DPlanta)),]
DConcho=DConcho[sample(1:nrow(DConcho)),]



require(RSNNS)
Dados=rbind(Dfundo[1:2000,],DPlanta[1:1000,],DConcho[1:1000,])
D=rbind( cbind(rep(1,2000),rep(0,2000)),
       cbind(rep(0,1000),rep(1,2000)))
Dados=cbind(Dados,D)
Dados=Dados[sample(1:nrow(Dados)),]

Dados2=splitForTrainingAndTest(Dados[,1:3],Dados[,4:5],ratio = 0.3)



D=as.data.frame(Dados2$inputsTrain)
colnames(D)=c("R","G","B")

model=glm(Dados2$targetsTrain[,1]~R*G*B,data=D,family =binomial("logit"))

D2=as.data.frame(Dados2$inputsTest)
colnames(D2)=c("R","G","B")
pred=round(predict(model,newdata=D2,"response"),0)
obs=Dados2$targetsTest[,1]

mean(pred==obs)




P=data.frame(R=c(planta[,,1]),G=c(planta[,,2]),B=c(planta[,,3]))
pred=round(predict(model,newdata=P,"response"),0)

m=matrix(pred,ncol=ncol(planta[,,1]))
imageShow(planta)
imageShow(m)

m2=PreencherMascaraNegra(m,perc=0.005,imagem=F,individual=F)
imageShow(m2)




Dados=rbind(DPlanta[1:2000,],DConcho[1:2000,])
D=c(rep(1,2000),rep(0,2000))
Dados=cbind(Dados,D)
Dados=Dados[sample(1:nrow(Dados)),]

Dados2=splitForTrainingAndTest(Dados[,1:3],Dados[,4],ratio = 0.3)

D=as.data.frame(Dados2$inputsTrain)
colnames(D)=c("R","G","B")

model=glm(Dados2$targetsTrain~R*G*B,data=D,family =binomial("logit"))

D2=as.data.frame(Dados2$inputsTest)
colnames(D2)=c("R","G","B")
pred=round(predict(model,newdata=D2,"response"),0)
obs=Dados2$targetsTest
mean(pred==obs)



P=data.frame(R=c(planta[,,1]),G=c(planta[,,2]),B=c(planta[,,3]))
id=c(m2)==0
p2=P[id,]
colnames(p2)=c("R","G","B")




pred=round(predict(model,newdata=p2,"response"),0)
P[id,]=pred

P2=array(NA,dim=c(nrow(planta[,,1]),ncol(planta[,,1]),3))
P2[,,1]=matrix(P[,1],nrow=nrow(planta[,,1]))
P2[,,2]=planta[,,2]
P2[,,3]=planta[,,3]

imageShow(P2)



