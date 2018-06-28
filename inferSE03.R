# Key functions for inferring self-esteem by evidence accummulation and mapping
# beliefs to responses using a sigmoid function
#
# Michael Moutoussis & Alexis (An Yee) Low, 2018

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
# --------------------------------------------------------------------------

# LikeMe.R has most of the functions that for this project :
source(paste(codeDir,'LikeMe.R',sep='')); 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# SLPsocio3 is the second, more advanced 'Inference by counting model' (Michael's model B)
SLPsocio3 <- function( parMat, datAr,onlySLP=0, check=1){
# parMat has rows with the parameters for each pt.
# datAr has a page for each pt, and Ntr rows., gp pred  obs SE cols.
#
# par. row must be: c('SEb', 'sensi','sesh', 'a0min', 'n0', 'nMax','Tresp', 'Bresp')
# e.g.   parMat =   c(0.67,     2,     1.5,    2,       4,    6,     0.2,    0.1 ) 
#        sensitivity of pAcc->SE, threshold of pAcc->SE etc.

 M <- 4;          # Number of rater groups.
 SEgenMeth <- 2;  # should generated SE be random (1) from component (group) distros, 
                  # expectation (0), overall beta (2) ...
 eps <- 1e-10;    # a very small number to catch ln(0) etc ...

 if (check){
  datDim <- dim(datAr);
  if (length(datDim)<3){   # if 2D object was provided, convert to 3D.
    hd <- colnames(datAr);
    datAr <- array(as.matrix(datAr),c(datDim,1));
    colnames(datAr) <- hd;
   }
  if (is.null(dim(parMat))) {
    parMat <- array(parMat,c(1,length(parMat)));
  }
 }
 colnames(parMat) <- c('SEb','sensi','sesh','a0min','n0','nMax','Tresp','Bresp');
 
 Nptot <- dim(datAr)[3];
 Ntrtot <- dim(datAr)[1];# this will sadly have to be fiddled with later, usually to
                 # Cater somehow for the fact that scanned task contains interruptions.
  bakD <- datAr;     # backup copy to restore later ...
  stimTr <- array(NA,c(Ntrtot,Nptot));  # will store positions of stimulus-endowed trials.
  
 # Array to hold alpha, beta, n-so-far for each group of raters,
 # as well as the probability / prob. dens. for the responses emitted,
 # and generated acceptance prediction and SE. expSE is the 'the SE' point estimate.
 # Note has one more row than trials as starts from 'priors'.
 abnPol <- array(dim=c(Ntrtot+1,3*M+5,Nptot)); 
 dimnames(abnPol)[[2]] <- c(paste(c('a','b','n'),repAdjVec(1:M,3),sep=''),
                            'obsP','SEPD','accP','genSE','expSE');
 aInd <- (0:(M-1))*3+1;   # Indices of a in abnG
 bInd <- aInd+1;
 nInd <- bInd+1;
 obsPI <- 3*M+1;         SEPDI  <- obsPI+1 ;
 accPI <- SEPDI+1;       genSEI <- accPI+1;     expSEI <- genSEI+1;
 
 for (ptN in 1:Nptot){
     
    # Cater somehow for the fact that scanned task contains interruptions
    # including at the very start. Awkward - to start with, just exclude:
    stimTr[,ptN] <- !is.na(datAr[,'gp',ptN]);
    Ntrtot <- sum(stimTr[,ptN]);    # update to exclude non-stimulus trials. Was dim(datAr)[1] above.
    datAr[1:Ntrtot,,ptN] <- datAr[stimTr[,ptN],,ptN];  
    if (Ntrtot<dim(datAr)[1]) {
      datAr[(Ntrtot+1):dim(datAr)[1],,ptN] <- NA; 
    }
    
   # Initial beliefs about ratings by others (groups)
   # alpha0s spaced up from a0min geometrically so that aBase makes sense :
   d <- 1 - parMat[ptN,4]/parMat[ptN,5]
   r <- exp(2/3*log((1-parMat[ptN,1])/d));
   abnPol[1,aInd,ptN] <- parMat[ptN,5]*(1-d*r^(0:3)); 
       # linear, when parMat gave a0max and a0min, was
       # ((M-1):0)*(parMat[ptN,3]-parMat[ptN,4])/(M-1) + parMat[ptN,4];
   abnPol[1,bInd,ptN] <- parMat[ptN,5]-abnPol[1,aInd,ptN];    
   abnPol[1,nInd,ptN] <- parMat[ptN,5];    

   nMax <- parMat[ptN,6];   # the whole of this available for task in this version.
   adecay <- (nMax-3)/(nMax-2) ;  # to be used for 'too much data to remember'

   # parameters for mapping acceptance py to SE (don't use lower case a, b !)
   A <- parMat[ptN,'sensi'];        B <- parMat[ptN,'sesh'];
   
   if (check){
     if (nMax-parMat[ptN,5] < 0) {
       print(paste('At ptN',ptN,' params:')); print(parMat[ptN,]);
       stop('Please make sure Nmax-n0 >= 0');
     }
   }
   
   # make sure 0 < SE <1 :
   datAr[vecTRUE(datAr[,4,ptN] <=0),4,ptN] <- eps  ;
   datAr[vecTRUE(datAr[,4,ptN] >=1),4,ptN] <- 1-eps;

   # Now for acceptance prediction response function parameters:
   Tresp = parMat[ptN,'Tresp'];   # this and Hresp below scaled to be
   Hresp = parMat[ptN,'Hresp'];   # tuned to probability-like calcs ...
     
   # loop over trials
   for (trN in 1:Ntrtot){
     abnPol[trN+1,,ptN] <- abnPol[trN,,ptN];   # Initialise-most will remain same.
    
     gpI <- datAr[trN,1,ptN];  # which group rated this pt
     if (is.na(gpI)){  # if there was no valid group i.e. no 'rater' was presented
           # just keep propagated beliefs but don't attepmt anything of substance
      abnPol[trN+1,c(SEPDI,obsPI,accPI,genSEI,expSEI),ptN] <- NA;
      
     } else { # if there was valid group i.e. valid 'rater' was presented
     # Prob. of 'accept' response emitted (this is BEFORE rating seen),
     # if present. NB we will use beliefs after last trial, i.e. abnPol[trN,...
     ratingP <- 1/(1+exp((1-2*(abnPol[trN,aInd[gpI],ptN]/abnPol[trN,nInd[gpI],ptN] +
                                          Hresp))/Tresp));
     abnPol[trN+1,accPI,ptN] <- ratingP;  # Store                          
    
     predAcc <- datAr[trN,2,ptN]; # rating that actual participant predicted.
     if (!is.na(predAcc)){    
       if (predAcc > 0.5){  # i.e. if it's 1 
         abnPol[trN+1,obsPI,ptN] <- ratingP;
       } else {
         abnPol[trN+1,obsPI,ptN] <- 1-ratingP;
       }
     } # End if valid prediction predAcc
    
     nSoFar <- abnPol[trN,nInd[gpI],ptN];
     nofb = datAr[trN,'nofb',ptN];  # 0 if feedback given, 1 otherwise, so if no
                                        # feedback given don't augment evidence index
     if (!nofb) {  # If not feedback was given, leave all the a,b,n alone ...
       apprfb = (datAr[trN,3,ptN]+1)/2 ; # approval or not, i.e. convert from -1 1 to 0 1
       if ((nSoFar+1) <= nMax){
        abnPol[trN+1,nInd[gpI],ptN] <- abnPol[trN,nInd[gpI],ptN]+1;
        # Adjust a, b according to the observation made:
        abnPol[trN+1,aInd[gpI],ptN] <- abnPol[trN,aInd[gpI],ptN]+apprfb ;
        # Just for completeness and checking:
        abnPol[trN+1,bInd[gpI],ptN] <- abnPol[trN,bInd[gpI],ptN]+1-apprfb ;
      
       } else { # if nSoFar has max'ed to nMax :
        abnPol[trN+1,nInd[gpI],ptN] <- abnPol[trN,nInd[gpI],ptN];
        # a only allowed to go to nMax-1, to allow b >= 1 :
        abnPol[trN+1,aInd[gpI],ptN] <- 1+ datAr[trN,3,ptN] +
                                       adecay*(abnPol[trN,aInd[gpI],ptN]-1) ;                
        # Again, just for completeness, b:
        abnPol[trN+1,bInd[gpI],ptN] <- nMax - abnPol[trN+1,aInd[gpI],ptN];    
    
       } # end if we have or haven't maxed out the trials to calc. over
    }
         
    # Consider SE as a map from prob. of acceptance to a scale over c(0,1)
    # & calc. p density at the new SE reported, if valid. IT HAS TO CORRESPOND TO THE
    # WAY SE IS GENERATED (e.g. for synthetic data ...)
    # We'll express the overall SE distribution as a mixture of Beta distros.
    #datAr[trN,4,ptN]
    #  First express the actually expressed SE from experiment in terms of an
    #  acceptance probability :
    SEdat = datAr[trN,4,ptN] ; 
    experAccP <- SE2accP( SEdat,A,B); 
    if (is.na(datAr[trN,4,ptN])) {
      abnPol[trN+1,SEPDI,ptN] <- NA ;
    } else if ((SEgenMeth==0) || (SEgenMeth==1)) {
      # Actually for ==0 above isn't quite consistent ...
      accPdens <- mean(dbeta( experAccP, 
                    abnPol[trN+1,aInd,ptN], abnPol[trN+1,bInd,ptN]));
      # SE density also scaled by the slope of the accP(SE) map : 
      abnPol[trN+1,SEPDI,ptN] <- accPdens * slopeSE2accP(SEdat,A,B,accPdens);
    } else if (SEgenMeth==2) {
      a <- sum(abnPol[trN+1,aInd,ptN]) ;
      b <- sum(abnPol[trN+1,bInd,ptN]) ;  
      accPdens <- dbeta( experAccP , a, b); 
      # SE density scaled by the slope of the accP(SE) map : 
      abnPol[trN+1,SEPDI,ptN] <- accPdens * slopeSE2accP(SEdat,A,B,accPdens);
    }
    
    
    ##                      'reported' and generated-data  SE 
    #  - note selection var SEgenMeth
    if (SEgenMeth==1)  {  # should generated SE be random (1) by component
                          # expectation (0) or ...
      
      # first select which component to sample from:
      comp <- sample(1:M,1);
      # The SE beta distros here do NOT include 'private data' :
      a <- abnPol[trN+1,aInd[comp],ptN]  ; 
      b <- abnPol[trN+1,bInd[comp],ptN]  ; 
      # Now fll in expected and sampled SE !
      abnPol[trN+1,expSEI,ptN] <- accP2SE(a/(a+b), A, B);  
      abnPol[trN+1,genSEI,ptN] <- accP2SE(rbeta(1,a,b), A, B);  
      
    } else if (SEgenMeth==0) {  # 'reported' SE in genSE col. as
          # TRANSFORMED overall expectation:
          accSum <- 0;
          for (i in 1:M){
            accSum <- accSum + abnPol[trN+1,aInd[i],ptN]/abnPol[trN+1,nInd[i],ptN];   
          }  
          abnPol[trN+1,genSEI,ptN] <- accP2SE(accSum/M, A, B); 
          abnPol[trN+1,expSEI,ptN] <- abnPol[trN+1,genSEI,ptN];   # identical copy
    } else if (SEgenMeth==2) {  # 'reported' SE in genSE col. from an
                                #    boverall beta ... 
      # calc. a and b again, as may not have been calculated above. 
      a <- sum(abnPol[trN+1,aInd,ptN]) ;
      b <- sum(abnPol[trN+1,bInd,ptN]) ;   
      abnPol[trN+1,expSEI,ptN] <- accP2SE(a/(a+b), A,B);
      # for debug:
      abnPol[trN+1,genSEI,ptN] <- abnPol[trN+1,expSEI,ptN]; 
      #abnPol[trN+1,genSEI,ptN] <- accP2SE(rbeta(1,a,b), A,B);
      
    }  # end if else 
    # End generate SE value to report. 
    
    } # end if there was a valid group i.e. a 'rater' was indeed presented.
    
   } # end loop over trials.
   
  } # end loop over pts

  # Create objects for output
  SLP1 <- sum(log(na.omit(as.vector(abnPol[,obsPI,]))));
  SLP2 <- sum(log(na.omit(as.vector(abnPol[,SEPDI,]))));

  if (onlySLP){
    return( SLP1+SLP2 );
  } else {
    SLPetc <- list();
    SLPetc[[1]] <- SLP1;
    SLPetc[[2]] <- SLP2;
    # Next to combine both exp. data, beliefs, policies etc.
    # Therefore has extra gp, pred, obs, SE and genPred cols :
    colN <- dim(abnPol)[2]+6;
    DatBelPol <- array(NA,c(Ntrtot+1,colN,Nptot));
    dimnames(DatBelPol)[[2]] <- c(colnames(datAr), colnames(abnPol), 'genPred')
    DatBelPol[2:(Ntrtot+1),1:5,] <- datAr;
    DatBelPol[,(5+1):(5+dim(abnPol)[2]),] <- abnPol;
    for (ptN in 1:Nptot) {
      for (trN in 1:Ntrtot) {
          DatBelPol[trN+1,'genPred',ptN] <- rbinom(1,1,DatBelPol[trN+1,'accP',ptN]);
      }
    }
    SLPetc[[3]] <- DatBelPol;
    # 4th element to have generated data only :
    SLPetc[[4]] <- NA*datAr;  # shortest scripting to preserve dimentionality ...
    SLPetc[[4]][,,] <- DatBelPol[2:(Ntrtot+1), c('gp','genPred','obs','genSE','nofb'),] ;
    colnames(SLPetc[[4]]) <- c('gp','pred','obs','SE','nofb'); # Just like real expt. data ...
    SLPetc[[5]] <- parMat;
    colnames(SLPetc[[5]]) <- c('SEb','sensi','sesh','a0min','n0','nMax','Tresp','Bresp');
    names(SLPetc) <-c('predSLnP','SESLnP','DatBelPol','genD','ptPar');
    
    return(SLPetc);
  }
    
} # end of SLPsocio3  - 'Counting model' B (Michael's)

# -----------------------------------------------------------------------------

# Param transf. for SLPsocio3, using 8-element input.
# par. row must be: c('SEb', 'sensi','sesh', 'a0min', 'n0', 'nMax','Tresp', 'Bresp')
# e.g.   parMat =   c(0.67,     2,     1.5,    2,       4,    6,     0.2,    0.1 )
#  to  trPramat   ln parMat[ 1:3], ln(a0min-1), ln(n0-a0min-1), ln(nMax-n0), ln parMat[7:8]
#        sensitivity of pAcc->SE, threshold of pAcc->SE etc.

nat2trLP3 <- function(p,check=1){  
  # p here is of form  :  c('SEb', 'sensi','sesh', 'a0min', 'n0', 'nMax','Tresp', 'Bresp')
  # so  trp to be of form:
  #  to  trPramat   ln parMat[ 1:3], ln(a0min-1), ln(n0-a0min-1), ln(nMax-n0-1), ln parMat[7:8]

  eps   <- exp(-25);   # So that for R 1+eps > 1
  minLn <- -1000;   # so that for R exp(minLn) == 0, exp(-minLn) == +Inf
  
  # Basic check - of argument format
  if (check > 0){ if (is.null(dim(p))){   # convert vec to mat if need be
    p <- matrix(p,nrow=1,byrow=TRUE) }   }    
  ptTot <- dim(p)[1]; 
  # Detailed check
  if (check > 1){
    for (ptN in 1:ptTot) {
      if (sum(p[ptN,1:3] < -2*eps)) { 
        print(paste('parMat 1:3 =',p[ptN,1:3]));
        stop('--> ln of -ve    error' ); 
      }
      if ((p[ptN,4]-1) < -2*eps) {
        stop('ln(a0min-1) error' ); 
      }
      if ((p[ptN,5]-p[ptN,4]-1) < -2*eps) {
        stop('ln(n0-a0min-1) error' ); 
      }
      if ((p[ptN,6]-p[ptN,5]-1) < -2*eps){
        stop('ln(nMmax-n0) error : restrict to above 1 so that  nMax=Ntask > n0+1 > 2+1 and want Ntask > 3 so the delta learning rule works as per aDecay = (Ntask-3)/(Ntask-2) > 0 .')
      }
    }
  }
  
  trp <- matrix(NA,nrow=ptTot,ncol=dim(p)[2]); 
   
  y <- p[,4]-1;        y[y<eps] <- eps; 
  trp[,4] <- log(y);            # ln(a0min-1)
  y <- p[,5]-p[,4]-1;  y[y<eps] <- eps; 
  trp[,5] <- log(y);            # ln(n0-a0min-1)
  y <- p[,6]-p[,5]-1  ;  y[y<eps] <- eps; 
  trp[,6] <- log(y);            # ln(nMax-n0-1)

  # the others :
  trp[,1]  <- atanh(2*p[,1]-1);
  y <- p[,c(2,3,7,8)];           y[y<eps] <- eps;   trp[,c(2,3,7,8)] <- log(y);
  # bound under / overflows:
  trp[trp < minLn] <- minLn;    trp[trp > -minLn] <- -minLn;
  

  if (check){ colnames(trp)<- c('atanh(2SEb-1', 'ln(sensi)','ln(sesh)', 'ln(a0min-1)', 'ln(n0-a0min-1)', 'ln(nMax-n0-1)','ln(Tresp)', 'ln(Bresp)') }
  return(trp);

}  # end of nat2trLP3

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

tr2natLP3 <- function(trp,check=1){
#  trp is as follows;
#      c('atanh(2SEb-1', 'ln(sensi)','ln(sesh)', 'ln(a0min-1)', 'ln(n0-a0min-1)', 'ln(nMax-n0-1)','ln(Tresp)', 'ln(Bresp)')
#  Returns: c('SEb', 'sensi','sesh', 'a0min', 'n0', 'nMax','Tresp', 'Bresp')
  
  eps <- 1e-10;  #  eps is a tiny constant guaranteeing Nmax > n0+nBase as is needed.
  
  if (check){ if (is.null(dim(trp))){   # convert vec to mat if need be
      trp <- matrix(trp,nrow=1,byrow=TRUE) }   }
  ptTot <- dim(trp)[1]; 
  p <- matrix(NA,nrow=ptTot,ncol=dim(trp)[2]); 

  p[,4] <- exp(trp[,4]) + 1;        # a0min
  p[,5] <- p[,4]+1 + exp(trp[,5]);  # n0 :  restrict to above 2 so that  nMax=Ntask > n0+1 > 2+1 and want Ntask > 3 so the delta learning rule works as per aDecay = (Ntask-3)/(Ntask-2) > 0 ..
  p[,6] <-  p[,5] + 1 +  exp(trp[,5]);  # as per rationale above.

  p[,1] <- 0.5*(1 + tanh(trp[,1])) ; # SEb
  p[,c(2,3,7,8)] <- exp(trp[,c(2,3,7,8)]);  #  'sensi','sesh', 'Tresp', 'Bresp' 
      
  if (check){ colnames(p) <- c('SEb', 'sensi','sesh', 'a0min', 'n0', 'nMax','Tresp', 'Bresp') };
  return(p); 
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#                                          end of file

