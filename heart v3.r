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
fis= addMF(fis,"input",1,gaussMF("v,cold",tempu,c(0.89,30,1))); 
fis= addMF(fis,"input",1,gaussMF("cold",tempu,c(0.75,33.75,1))); 
fis= addMF(fis,"input",1,gaussMF("m.cold",tempu,c(0.17,34.75,1)));
fis= addMF(fis,"input",1,gaussMF("normal",tempu,c(0.60,36.5,1))); 
fis= addMF(fis,"input",1,gaussMF("m.hot",tempu,c(0.32,37.5,1))); 
fis= addMF(fis,"input",1,gaussMF("hot",tempu,c(1.04,39.25,1))); 
fis= addMF(fis,"input",1,gaussMF("v.hot",tempu,c(1.62,46,1))); 

fis= addVar(fis,"input","Heart Rate",hru); #Heart Rate
fis= addMF(fis,"input",2,gaussMF("v.Slow",hru,c(3.16,30,1 ))); 
fis= addMF(fis,"input",2,gaussMF("Slow",hru,c(2,42,1 ))); 
fis= addMF(fis,"input",2,gaussMF("Slowish",hru,c(3.16,50,1 )));
fis= addMF(fis,"input",2,gaussMF("Normal",hru,c(4.61,62.5,1))); 
fis= addMF(fis,"input",2,gaussMF("Fastish",hru,c(2.87,73.5,1))); 
fis= addMF(fis,"input",2,gaussMF("Fast",hru,c(2,81,1))); 
fis= addMF(fis,"input",2,gaussMF("v.Fast",hru,c(4.61,100,1))); 

fis= addVar(fis,"output","Urgency",urgency); # urgency Level
fis= addMF(fis,"output",1,gaussMF("fine",urgency,c(11.83,0,1 ))); 
fis= addMF(fis,"output",1,gaussMF("Concerning",urgency,c(9.233,54.5, 1))); 
fis= addMF(fis,"output",1,gaussMF("Urgent",urgency,c(9.233,100,1))); 

rulelist = rbind(
c(0,1,3,1,1),
c(1,0,3,1,1),
c(2,0,3,1,1),
c(3,2,3,1,1),c(3,3,2,1,1),c(3,4,2,1,1),c(3,5,3,1,1),c(3,6,3,1,1),
c(4,2,1,1,1),c(4,3,1,1,2),c(4,4,1,1,2),c(4,5,1,1,2),c(4,6,1,1,2),
c(5,2,2,1,1),c(5,3,1,1,1),c(5,4,2,1,1),c(5,5,1,1,1),c(5,6,2,1,1),
c(6,2,3,1,1),c(6,3,3,1,1),c(6,4,2,1,1),c(6,5,3,1,1),c(6,6,3,1,1),
c(0,7,3,1,2),
c(7,0,3,1,2)
);

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
c(30,35),c(30,45),c(30,55),c(30,63),c(30,74),c(30,80),c(30,85),
c(34,35),c(34,45),c(34,55),c(34,63),c(34,74),c(34,80),c(34,85),
c(35,35),c(35,45),c(35,55),c(35,63),c(35,74),c(35,80),c(35,85),
c(36,35),c(36.5,45),c(36.5,55),c(37,63),c(37,74),c(37,80),c(37,85),
c(36.9,35),c(37.3,45),c(37.5,55),c(37.6,63),c(37.9,74),c(38,80),c(38,85),
c(37.9,35),c(39.5,45),c(40.5,55),c(41,63),c(38,74),c(36,80),c(38,85),
c(40.9,35),c(45,45),c(44,55),c(43,63),c(45,74),c(41.9,80),c(43,85));


showrules(fis)

evalFIS(inputs,fis)
par(mfrow=c(3,2)) 
plotMF(fis,"input",1) 
plotMF(fis,"input",2) 
plotMF(fis,"output",1) 
#showFIS(fis)

#par(mfrow=c(3,2)) 



#gensurf3d(fis)
