install.packages("rgl")
install.packages("FuzzyToolkitUoN")
library(rgl)
library(FuzzyToolkitUoN)
#+++++++++++++++++++first version++++++++++++++++
fis= newFIS("HT1");

tempu= seq(30,46,0.1)
hru= 30:100
urgency= 0:100

fis= addVar(fis,"input", "Temperature",tempu); #temperature
fis= addMF(fis,"input",1,triMF("cold",tempu,c(30,32.5,35, 1))); 
fis= addMF(fis,"input",1,triMF("normal",tempu,c(35,37,39, 1))); 
fis= addMF(fis,"input",1,triMF("v.hot",tempu,c(39,44,46,1))); 

fis= addVar(fis,"input","Heart Rate",hru); #Heart Rate
fis= addMF(fis,"input",2,triMF("Slow",hru,c(30,35,40,1 ))); 
fis= addMF(fis,"input",2,triMF("Normal",hru,c(40,58,76,1))); 
fis= addMF(fis,"input",2,triMF("Fast",hru,c(76,88,100,1))); 

fis= addVar(fis,"output","Urgency",urgency); # urgency Level
fis= addMF(fis,"output",1,triMF("fine",urgency,c(0,20,40,1 ))); 
fis= addMF(fis,"output",1,triMF("Concerning",urgency,c(40,55,70, 1))); 
fis= addMF(fis,"output",1,triMF("Urgent",urgency,c(70,85,100,1))); 

rulelist = rbind(
c(1,1,3,1,1),
#c(2,1,2,1,1),
c(2,2,1,1,1),
#c(3,2,2,1,1),
c(3,3,3,1,1));

fis= addRule(fis, rulelist) 



#show rules for easier reading
showrules <- function(fis) {
  NumInputs= length(fis$input)
  NumOutputs= length(fis$output)
  NumRules= nrow(fis$rule)
  frow= 0
 
  if ( !is.null(NumRules) ) {
    for ( i in 1:NumRules ) {
      frow= frow + 1
      cat(frow, '. If ', sep='');
      for ( j in 1:NumInputs )
      {
        if ( fis$rule[i,j] != 0 )
        {
          cat('(', fis$input[[j]]$inputName, ' is ', sep='')
          if ( fis$rule[i,j] < 0 ) cat('not ', sep='')
          cat(fis$input[[j]]$membershipFunctionList[[abs(fis$rule[i,j])]]$mfName, ') ', sep='')
        }
        if ( j < NumInputs && fis$rule[i,j] != 0 && fis$rule[i,j+1] != 0 )
        {
          if ( fis$rule[i,NumInputs+NumOutputs+2] == 1 )
            cat('and ', sep='')
          else
            cat('or ', sep='')
        }
      }
      cat('then ', sep='')
      for ( j in 1:NumOutputs )
      {
        if ( fis$rule[i,NumInputs+j] != 0 )
        {
          cat('(', fis$output[[j]]$outputName, ' is ', sep='')
          if ( fis$rule[i,NumInputs+j] < 0 ) cat('not ', sep='')
          cat(fis$output[[j]]$membershipFunctionList[[abs(fis$rule[i,NumInputs+j])]]$mfName, ') ', sep='')
        }
      }
      cat('(', fis$rule[i,NumInputs+NumOutputs+1], ')\n', sep='')
    }
  }
}

#evaluation section
inputs = rbind(
c(22,30),c(14,36),c(32,42),c(33,50),c(33,60),c(21,66),c(19,70),c(20,75),c(28,82),c(11,88),c(30,91),
c(33.5,30),c(33.5,36),c(34,42),c(34.5,50),c(35,60),c(35.5,66),c(36,70),c(33.2,75),c(33.5,82),c(34.5,88),c(36,91),
c(35.5,30),c(36,36),c(36.5,42),c(36,50),c(36.2,60),c(36.5,66),c(37.2,70),c(37.5,75),c(37,82),c(35.5,88),c(36.7,91),
c(37.6,30),c(38,36),c(38.5,42),c(39,50),c(39.5,60),c(40,66),c(40.5,70),c(41,75),c(40.5,82),c(38,88),c(37.7,91),
c(41.5,30),c(42,36),c(42.5,42),c(43,50),c(43.5,60),c(46,66),c(45,70),c(45.5,75),c(42,82),c(43,88),c(45.5,91));

evalFIS(inputs,fis)
par(mfrow=c(3,1)) 
plotMF(fis,"input",1) 
plotMF(fis,"input",2) 
plotMF(fis,"output",1) 
showFIS(fis)
showrules(fis)




#gensurf3d(fis)
