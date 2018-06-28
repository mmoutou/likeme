# Working file for inferring Self-esteem functions.
# Please copy 'finished' functions to LikeMe.R

## Student, start by selecting and running (Control-Return in Linux and Windoze)
## lines 1-36, then go to line 91 and read on, down to
## 'End of counting model / SLPsocio1 block'

remove(list=ls());     # clear the decks (UNLIKE LikeMe.R)
# ---- First find out from where this source code and so set paths -----
# Set the base and code directories consistently for different
# users of the main code, LikeMe.R etc.
sewd <- getwd();       # find out where we are
# Depending on what the path contains, decide who is the user. Student PLS EDIT YOUR ENTRY:
if (grepl('C:/Users/Will',sewd)){ whoami <- 'Student'}
if (grepl('/home/hopper',sewd)){whoami <- 'WillLinux'}
if (grepl('C:/Users/mmoutou',sewd)){ whoami <- 'SpectreMM'}
if (grepl('michael',sewd)){ whoami <- 'LinuxMM'}
if (grepl('geert-jan',sewd)){ whoami <- 'GeertJanMac'}
# Adjust the base directdory accoriding.  Student PLS EDIT YOUR ENTRY :
switch(whoami,
       Student  = {baseDir <- "C:/Users/student/SelfEvalModeling/"; },
       WillLinux = {baseDir <- "/home/hopper/Dropbox/SelfEvalMEV";},
       SpectreMM = {baseDir <- "C:/Users/mmoutou/OneDrive/SharePoint/Low, An Yee/Summer Project - Alexis An Yee Low/";},
       LinuxMM = {baseDir <- "/home/michael/gitwork/LikeMe/";},
       GeertJanMac= {baseDir <- "/Users/geert-janwill/Dropbox/GJW_LikeMe/"; })

codeDir <- paste(baseDir,"Rscripts/",sep='')
# ----------------- Load data and key functions  --------------------

setwd(baseDir)
load('GJW03.RData');
# sourcing & libraries:
try(source(paste(baseDir,'/Rscripts/LikeMe.R',sep='')));
library(ppcor);

# ---- Student summer project - please familiarise yourself from this point ----
#              down to 'fit whole real dataset with SLPsocio1'
#
#  SLPsocio1 - 'counting model', first for just participant 17
#
D <- D17;
#     Following line for older version from GJW03, which doesn't have 'nofb' column:
#     load('GJW03.RData');  D <- datArW03[,,17]
# parm is aBase > 1, nBase > aBase+1, a0min, n0 such that a0min/n0 < aBase/nBase, Nmax > n0+nBase, Tresp
# parm  <- c(4, 5.5, 6, 10.00,  20.0,   0.2);   # shorter format
parm <- c(5,6.5,5,7,15,0.2);
#parM1  <- Par6toPar7LP1(parm); 
slp1.17 <- SLPsocio1(parm,D);
plot(slp1.17[[3]][,'expSE',1],t='b');  lines(slp1.17[[3]][,'genSE',1],t='l',col='pink3');
simD <- slp1.17$genD;   

# If desired, replace SEs with NAs to have it a la Robb:
v <- D[,'SE',]; v <- 1+v; v<- v/v;  # create vector of 1's and NA's
#simD[,'SE',] <- v*simD[,'SE',];

simSLP1 <- SLPsocio1(parm,simD); 
cat('    predSLnP           SESLnP\n',paste(c(simSLP1[[1]],simSLP1[[2]])))
# a  wrong one:
p1tr <- nat2trLP1(parm);
p1tr <- 1.2*p1tr;
pm1b   <- tr2natLP1(p1tr);
#parM1b <- Par6toPar7LP1(pm1b);
simSLP1b <- SLPsocio1(pm1b,simD);
cat('    predSLnP           SESLnP\n',paste(c(simSLP1b[[1]],simSLP1b[[2]])))

# fit simulated data with a fairly easy starting point, the slightly
# wrong one above :
simFit <- nlm(msLP1tr, p1tr, simD, print.level=2, iterlim=100);
estp <- (tr2natLP1(simFit$estimate)) ;
print( round(estp,3) ); 
fitSLP1 <- SLPsocio1(estp,simD); print(cbind(fitSLP1[[1]],fitSLP1[[2]])); 
print(parm);
fitTrue <- SLPsocio1(parm,simD); print(cbind(fitTrue[[1]],fitTrue[[2]])); 

# fit 'good example':
# First define some weak prior beliefs about the parameters, in terms of gamma
# distribution means and SDs. These refer to the parameters in native space 
# and the simple values here are 'derived from the the interquartile range of a 
# first-pass fit 'common sense' but compatible with a first-pass 

Par0 <- matrix(NA,nrow=2,ncol=6);     # prior beliefs about params - gamma means and SDs
Par0[1,] <- 2*parm; Par0[1,6] <- 2;   # prior mean temperature set higher than typical values
                                      # on purpose; may help 'annealing' but see also below
Par0[2,] <- 0.9*Par0[1,];
fit17 <- nlm(msLP1tr, p1tr, D17, Par0, print.level=2, iterlim=100); 
SLP17 <- SLPsocio1(tr2natLP1(fit17$estimate),D17); 
cat('\nMAP estimated parameters for pt. 17:\n'); print(round(SLP17$ptPar,2),quote='F');

# ----- fit whole real dataset with SLPsocio1 -----
ml1fit <- list();   # will hold 'counting model' ('1') fits
ml1res <- matrix(NA,nrow=dim(datArW03)[3],ncol=8);
dimnames(ml1res)[[2]] <- c('aBase','nBase','a0min','n0','nMax','Tresp','predSLP','SESLD');
# load('socio1fit02.RData'); # init. cond. to be infromed by a half-decent fit ...

Par0[1,]  <-  2*pm1b;      Par0[1,6] <- 1;   # from first pass fit
Par0[2,]  <-  0.9*Par0[1,];  # Set the SD a little less than tne mean to get a broad curve
#trPar0[1,]  <-  c(1.039721, -0.1884999,  -0.9571506, -0.404949,  0.6931460, -1.273097);
tryP=c(1.9,3.0,1.8,4.1,7.2,0.5); iniLen=length(tryP); # first initialization by hand.

  
for (ptN in c(4,7,11)){ #1:dim(datArW03)[3] ){
  
  D <- array(NA,c(dim(datArW03)[1:2],1));  # Create & clear the working array
  D[,,1] <- datArW03[,,ptN];
  dimnames(D)[[2]] <- c('gp','pred','obs','SE','nofb');
  dimnames(D)[[3]] <- ptN;
  
  ml1fit[[ptN]] <- list();
  mPD <- Inf;
  for (attempt in 1:10){  # 2-10 for testing; try (10*iniLen) for real {
  
   if (attempt == 1){
     iniTrPar <- nat2trLP1(tryP);
   } else {
     iniTrPar <- nat2trLP1(tryP) * runif(6,0.75,1.25); 
   }
   # atI <- attempt %% iniLen; if (atI == 0){atI <- iniLen;};
   #if (attempt > 11) {
   #    iniTrPar <- as.numeric(iniTrParM[atI,] * runif(6,0.8,1.2));
   #}
   
   print(paste('ptN:',ptN,';  fit attempt:', attempt));  print(paste('Init. Cond:', paste(round(tr2natLP1(iniTrPar),3),collapse=',')));
   try( fitAttempt <- nlm(msLP1tr, iniTrPar, D, Par0, print.level=2, iterlim=500) ); # Par0, print.level=2, iterlim=500) );
   if (vecTRUE(length(fitAttempt$estimate)>1)){
     if ( vecTRUE(fitAttempt$minimum < mPD) || !(vecTRUE(length(ml1fit[[ptN]][[1]])>1)) ){
        mPD <- fitAttempt$minimum;
        ml1fit[[ptN]][[1]] <- fitAttempt;
     }
   }
  }  # End exploration of initial conditions
    
  est6p <- (tr2natLP1(ml1fit[[ptN]][[1]]$estimate)) ; 
  ml1fit[[ptN]][[2]] <- SLPsocio1(est6p, D);
  
  
  names(ml1fit[[ptN]]) <- c('NLM','SLP')
  
  # output array storage
  ml1res[ptN,1:6] <- tr2natLP1(ml1fit[[ptN]][[1]]$estimate);
  ml1res[ptN,7]   <- ml1fit[[ptN]][[2]][[1]]; 
  ml1res[ptN,8]   <- ml1fit[[ptN]][[2]][[2]]; 
  
  # Prepare for graphs with real & randomly generated data for visual inspection:
  v <- D[,'SE',1]; v <- 1+v; v<- v/v;  # create vector of 1's and NA's
  expSE <- ml1fit[[ptN]][[2]][[3]][,'expSE',1]*c(NA,v);  expSE <- expSE[-1];
   
  # a coarse analysis to see how much pts. SE responed to positive
  # feedback etc:
  d <- ml1fit[[ptN]][[2]][[3]][,,1] ;  d <- na.omit(TDSE(d));
  c <- round(cor(d[,c('sPE','TDSE')])[1,2],2);
  plot(na.omit(d[,c('sPE','TDSE')]), main=paste('pt',ptN,'  cor=',c)); 
  c <- round(cor(d[,c('sAp','TDSE')])[1,2],2);
  plot(na.omit(d[,c('sAp','TDSE')]), main=paste('pt',ptN,'  cor=',c)); 
  
  # Plotting of expected, measured and generated SE : 
  c <- round(cor(na.omit(data.frame(D[,'SE',1], expSE)))[1,2],2)
  d2plot <- (ml1fit[[ptN]][[2]][[3]][,c('SE','expSE','genSE'),1])
  plot(d2plot[,'SE'],t='p',col='green4',pch=19,lwd=5,
       main=paste('MAP fit, counting model (SLPsocio1):   pt',ptN,';  cor=',c,'\n[green: measured;   blue:fitted,  pink: generated from fit]'),
       xlab='trial number',
       ylab='Self Evaluation'); 
  lines(d2plot[,'expSE'],t='l',col='blue',lwd=3);
  lines(d2plot[,'genSE'],t='l',col='pink3');
  
  print(ml1res[1:ptN,])
  save.image(paste(baseDir,"socio1fitTEST.RData"))
  
}

# ---- Plotting some resuts from SLPsocio1 ----

expv <- 0;
ahd=c('a1','a2','a3','a4'); bhd=c('b1','b2','b3','b4');  # to calc belief precision etc later ..
for (ptN in 1:length(ml1fit)){
  
  D <- array(NA,c(dim(datArW03)[1:2],1));  # Create & clear the working array
  D[,,1] <- datArW03[,,ptN];
  dimnames(D)[[2]] <- c('gp','pred','obs','SE');
  dimnames(D)[[3]] <- ptN;
  
  # Prepare for graphs with real & randomly generated data for visual inspection:
  v <- D[,'SE',1]; v <- 1+v; v<- v/v;  # create vector of 1's and NA's
  expSE <- ml1fit[[ptN]][[2]][[3]][,'expSE',1]*c(NA,v);  expSE <- expSE[-1];
  
  # a coarse analysis to see how much pts. SE responed to positive
  # feedback etc:
  d <- ml1fit[[ptN]][[2]][[3]][,,1] ;  d <- na.omit(TDSE(d));
  pc <- pcor(d[,c('sPE','TDSE')]); 
  c <- round(pc$est[1,2],2);   psig <- round(pc$p[1,2],3); 
  #plot(na.omit(d[,c('sPE','TDSE')]), main=paste('pt',ptN,'  cor=',c,'; p.val=',psig)); 
  c <- round(pc$est[1,2],2);   pc <- pcor(d[,c('sAp','TDSE')]);   
  psig <- round(pc$p[1,2],3);
  #plot(na.omit(d[,c('sAp','TDSE')]), main=paste('pt',ptN,'  cor=',c,'; p.val=',psig)); 
  
  # Plotting of expected, measured and generated SE, as well as sd of expSE : 
  pc <- pcor(na.omit(data.frame(D[,'SE',1], expSE)))
  c <- round(pc$est[1,2],2);   psig <- round(pc$p[1,2],3); 
  d2plot <- (ml1fit[[ptN]][[2]][[3]][,c('SE','expSE','genSE'),1])
  plot(d2plot[,'SE'],t='p',col='blue',lwd=2, ylim=c(0,1),
       main=paste('Counting / popularity model pt',ptN,';  cor=',c,'; p.val=',psig,'(MAP fit)'),
       xlab='trial',ylab='reported (raw) SE'); 
   lines(d2plot[,'expSE'],t='l',col='orange3',lwd=3);
   # Now would like to plot +/- sd of expSE :
   ptPar = ml1fit[[ptN]][[2]][[5]];
   sd = rep(NA,dim(ml1fit[[ptN]][[2]][[3]])[1]);
   # See ' # calc. a and b again, as may not have been calculated above.' in LikeMe re lines below:
   a = rowSums(ml1fit[[ptN]][[2]][[3]][,ahd,1]) + ptPar[1];
   b = rowSums(ml1fit[[ptN]][[2]][[3]][,bhd,1]) + ptPar[2] -  ptPar[1];
   sd = sqrt( (a*b)/((a+b)*(a+b)*(a+b+a)) )
   lines(d2plot[,'expSE']+sd,t='l',col='orange');
   lines(d2plot[,'expSE']-sd,t='l',col='orange');
   
   expv <- expv + (sign(pc$est[1,2])*pc$est[1,2]^2) / length(ml1fit);
}
print(paste('Popularity model mean explained variance:',expv)); 

#  End of simplest inference-by-counting model / SLPsocio1 block  #


# ############# ~~~~ Testing SLPsocio3 ~~~~ #######################
# SLPsocio3  is the more advanced 'inference by counting' model 

# Start again with just participant 17
D <- D17;
#         c('SEb', 'sensi','sesh',  'a0min',   'n0', 'nMax','Tresp', 'Bresp') };
par3a <-  c(0.67,   1.5,     1,      1.5,       4,     6,     0.15,    0.5 );
slp3.17 <- SLPsocio3(par3a,D);
plot(slp3.17[[3]][,'expSE',1],t='b',ylim=c(0,1));  lines(slp3.17[[3]][,'genSE',1],t='l',col='pink3');
simD <- slp3.17$genD;   

# If desired, replace SEs with NAs to have it a la Robb:
v <- D[,'SE',]; v <- 1+v; v<- v/v;  # create vector of 1's and NA's
#simD[,'SE',] <- v*simD[,'SE',];

sim3a <- SLPsocio3(par3a,simD); 
cat('    predSLnP           SESLnP\n',paste(c(sim3a[[1]],sim3a[[2]])))
# Now for a  wrong one. p in form 'SEb', 'sensi','sesh', 'a0min', 'n0', 'nMax','Tresp', 'Bresp'
p3a <- par3a;
p3aTr <- nat2trLP3(p3a);
p3bTr <- p3aTr;
parErr <- rep(0.1,8);
# p3bTr <- p3bTr + parErr;
v=2; p3bTr[v] <- p3bTr[v] + parErr[v];
par3b   <- tr2natLP3(p3bTr); 
sim3b <- SLPsocio3(par3b,simD);
cat('    predSLnP           SESLnP\n',paste(c(sim3b[[1]],sim3b[[2]])))
# simple param vs. log lik to check ... doesn't work as yet ... :-( 
pn=2; 
lp<-matrix(NA,51,2); 
for (i in (-25:25)){
  parErr=0.025*i; 
  p3bTr=p3aTr; 
  p3bTr[pn]=p3bTr[pn]+parErr; 
  par3b=tr2natLP3(p3bTr); 
  print(as.vector(par3b));
  sim3b=SLPsocio3(par3b,simD); 
  lp[26+i,1]=sim3b[[1]]; 
  lp[26+i,2]=sim3b[[2]];
  #if ((parErr > 0.2) && (parErr < 0.35)){ print(c(par3b,lp[11+i,2])) }
};
plot( (-25:25)*0.025, lp[,2],t='b',col='blue3',main=colnames(p3bTr)[pn]); 
lines((-25:25)*0.025, lp[,1]+lp[,2],t='b',lwd=3); abline(0,10^9)

# attempt a fit without priors:
p0tr <- nat2trLP0(p0bTr)
fit0.17 <- nlm(msLP0tr, p0tr, D, print.level=2, iterlim=500); 
# This produces approx. 
# check it
p0na <- tr2natLP0(fit0.17$estimate);         # short form
p0nat <- c(p0na[1],NA,NA,NA,p0na[2:6])          # long form
slp0.17 <- SLPsocio0(p0nat,D,0, gpAcc);
plot(slp0.17[[3]][,'expSE',1],t='l',col='darkseagreen4',ylim=c(0,1));  
lines(slp0.17[[3]][,'genSE',1],t='l',col='darkseagreen3');
lines(slp0.17[[3]][,'SE',1],t='p',col='black')
# Try with prior
pri0 <- matrix(c(11,10,0,10,1000,700,11,10,0,50,11,10),nrow=2)
p0tr <- nat2trLP0(c(0.5,   0.6 ,  0.5,   0.2 ,   0.15, (0.85-0.15)/3 ))
fit0.17 <- nlm(msLP0tr, p0tr, D, pri0, print.level=2, iterlim=500); 
# check it
p0na <- tr2natLP0(fit0.17$estimate);               # short form
p0nat <- c(p0na[1:5],p0na[5]+(1:3)*p0na[6])      # long form
print(p0nat);
slp0.17 <- SLPsocio0(p0nat,D);
plot(slp0.17[[3]][,'vt',1],t='l',col='blue');  
lines(slp0.17[[3]][,'genSE',1],t='l',col='pink3');
lines(slp0.17[[3]][,'SE',1],t='p',col='black')

# -------- Debugging SLPsocio3 -------------
# Inference by counting model incorp. response function:
# a little bit of progress on debugging - changed SLPsocio3 to provide synthetic
# data based on means. Now the following produces reproducible error:

D <- D17;
#         c('SEb', 'sensi','sesh',  'a0min',   'n0', 'nMax','Tresp', 'Bresp') };
par3a <-  c(0.67,   1.5,     1,      1.5,       4,     6,     0.15,    0.5 );
slp3.17 <- SLPsocio3(par3a,D);
plot(slp3.17[[3]][,'expSE',1],t='b',ylim=c(0,1));  lines(slp3.17[[3]][,'genSE',1],t='l',col='pink3');
simD <- slp3.17$genD;   

# If desired, replace SEs with NAs to have it a la Robb:
v <- D[,'SE',]; v <- 1+v; v<- v/v;  # create vector of 1's and NA's
#simD[,'SE',] <- v*simD[,'SE',];

sim3a <- SLPsocio3(par3a,simD); 
cat('    predSLnP           SESLnP\n',paste(c(sim3a[[1]],sim3a[[2]])))
# Now for a  wrong one. p in form 'SEb', 'sensi','sesh', 'a0min', 'n0', 'nMax','Tresp', 'Bresp'
p3a <- par3a;
p3aTr <- nat2trLP3(p3a);
p3bTr <- p3aTr;
parErr <- rep(0.1,8);
# p3bTr <- p3bTr + parErr;
v=2; p3bTr[v] <- p3bTr[v] + parErr[v];
par3b   <- tr2natLP3(p3bTr); 
sim3b <- SLPsocio3(par3b,simD);
cat('    predSLnP           SESLnP\n',paste(c(sim3b[[1]],sim3b[[2]])))
# simple param vs. log lik to check ... doesn't work as yet ... :-( 

plot( sim3a[[3]][,'SEPD',1], sim3b[[3]][,'SEPD',1]); abline(0,1)

#  ---- ~~~~~~~~~~~~~~~~~~~~~ ----
#            end of file


