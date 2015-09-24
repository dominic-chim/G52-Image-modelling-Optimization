install.packages("FuzzyToolkitUoN") 
library(FuzzyToolkitUoN) 
ls("package:FuzzyToolkitUoN")

fis= newFIS("Test");
fis= addVar(fis,"input", "age",c(0, 70)); 
fis= addMF(fis,"input",1,gaussMF("young",c(0:70),c(10, 0, 1))); 
fis= addMF(fis,"input",1,gaussMF("middleaged",c(0:70),c(10, 40, 1))); 
fis= addMF(fis,"input",1,gaussMF("old",c(0:70),c(10, 70, 1))); 

fis= addVar(fis,"input","male weight",c(65,120));
fis= addMF(fis,"input",2,triMF("small",c(65:120),c(65, 70,75,1 ))); 
fis= addMF(fis,"input",2,triMF("medium",c(65:120),c(75, 85,95, 1))); 
fis= addMF(fis,"input",2,triMF("large",c(65:120),c(95, 108,120,1))); 

fis= addVar(fis,"input","male feet size",c(0,14));
fis= addMF(fis,"input",3,trapMF("very small",c(0:14),c(0,0,2,3,1))); 
fis= addMF(fis,"input",3,trapMF("small",c(0:14),c(2,3,5,6,1))); 
fis= addMF(fis,"input",3,trapMF("medium",c(0:14),c(5,6,8,9,1))); 
fis= addMF(fis,"input",3,trapMF("big",c(0:14),c(8,9,11,12,1 ))); 
fis= addMF(fis,"input",3,trapMF("very big",c(0:14),c(11,12,14,14, 1))); 

par(mfrow=c(3,1)) 
plotMF(fis,"input",1) 
plotMF(fis,"input",2) 
plotMF(fis,"input",3) 

#part 6
fis= newFIS("Part6");
fis= addVar(fis,"input", "Salary",c(0, 100000)); 
fis= addMF(fis,"input",1,trapMF("low",c(0:100000),c(0, 0, 15000,20000,1))); 
fis= addMF(fis,"input",1,trapMF("medium",c(0:100000),c(15000, 20000, 60000,70000,1))); 
fis= addMF(fis,"input",1,trapMF("high",c(0:100000),c(60000, 70000, 100000,100000,1))); 

fis= addVar(fis,"input", "age",c(0, 40)); 
fis= addMF(fis,"input",2,triMF("very short",c(0:40),c(0,1,2,1))); 
fis= addMF(fis,"input",2,triMF("short",c(0:40),c(1,5,8,1))); 
fis= addMF(fis,"input",2,triMF("med",c(0:40),c(8,19,30,1))); 
fis= addMF(fis,"input",2,triMF("long",c(0:40),c(28,32,35,1))); 
fis= addMF(fis,"input",2,triMF("very long",c(0:40),c(34,7,40,1))); 

fis= addVar(fis,"input", "Decision",c(0, 100)); 
fis= addMF(fis,"input",3,triMF("No",c(0:100),c(0,15,20,1))); 
fis= addMF(fis,"input",3,trapMF("Maybe",c(0:100),c(20,30,70,80,1))); 
fis= addMF(fis,"input",3,triMF("Yes",c(0:100),c(80,95,100,1))); 

#//rules
rulelist = rbind(c(3,5,3,1,1), c(3,4,2,1,1), c(3,3,2,1,1), c(3,2,1,1,1), c(3,1,1,1,1), c(2,5,1,1,1),c(2,4,2,1,1),c(2,3,2,1,1),c(2,2,2,1,1),c(2,1,1,1,1),c(1,5,2,1,1),c(1,4,2,1,1),c(1,3,2,1,1),c(1,2,1,1,1),c(1,1,1,1,1)) 
fis= addRule(fis, rulelist) 
showFIS
fis$ruleList

#//part 7

fis=newFIS("tipper");
fis= addVar(fis,"input", "service",c(0, 10));
fis= addMF(fis,"input",1,triMF("poor",c(0:10),c(0,0.75,1.5, 1))); 
fis= addMF(fis,"input",1,triMF("good",c(0:10),c(1.5,3.25,5, 1))); 
fis= addMF(fis,"input",1,triMF("excellent",c(0:10),c(1.5,5.75,10, 1))); 

fis= addVar(fis,"input", "food",c(0, 10));
fis= addMF(fis,"input",2,triMF("rancid",c(0:10),c(0, 0, 1,3,1))); 
fis= addMF(fis,"input",2,triMF("delicious",c(0:10),c(0.920455026455025, 8.91645502645503, 10.926455026455,18.926455026455,1))); 

fis= addVar(fis,"output", "tip",c(0,30));
fis= addMF(fis,"output",1,triMF("cheap",c(0:30),c(0,5,10,1))); 
fis= addMF(fis,"output",1,triMF("average",c(0:30),c(10,15,20,1))); 
fis= addMF(fis,"output",1,triMF("generous",c(0:30),c(20,25,30,1))); 

rulelist = rbind(c(1,1,1,1,2), c(2,0,2,1,1), c(3,2,3,1,2));
fis= addRule(fis, rulelist) 

par(mfrow=c(3,1)) 
plotMF(fis,"input",1) 
plotMF(fis,"input",2) 
plotMF(fis,"output",1) 
showFIS(fis)


evalFIS(c(1,2),fis) 
inputs= rbind(c(3,7),c(2,7)) 
evalFIS(inputs,fis) 
outputs= evalFIS(inputs,fis)  
par(mfrow=c(1,1)) 

gensurf(fis) 

install.packages("rgl")
library(rgl)
gensurf3d(fis)