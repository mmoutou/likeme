# Key functions for inferring self-esteem by evidence accummulation and mapping
# beliefs to responses using a sigmoid function
#
# Michael Moutoussis & Alexis (An Yee) Low, 2018

# ---- First find out from where this source code and so set paths -----
# Set the base and code directories consistently for different
# users of the main code, LikeMe.R etc.
sewd <- getwd();       # find out where we are
# Depending on what the path contains, decide who is the user. Student PLS EDIT YOUR ENTRY:
if (grepl('Alexis',sewd)){ whoami <- 'Student'}
if (grepl('/home/hopper',sewd)){whoami <- 'WillLinux'}
if (grepl('C:/Users/mmoutou',sewd)){ whoami <- 'SpectreMM'}
if (grepl('michael',sewd)){ whoami <- 'LinuxMM'}
if (grepl('geert-jan',sewd)){ whoami <- 'GeertJanMac'}
# Adjust the base directdory accoriding.  Student PLS EDIT YOUR ENTRY :
switch(whoami,
       Student  = {baseDir <- "X:/OneDrive - University College London/Summer Project - Alexis An Yee Low/"; },
       WillLinux = {baseDir <- "/home/hopper/Dropbox/SelfEvalMEV";},
       SpectreMM = {baseDir <- "C:/Users/mmoutou/OneDrive - University College London/SharePoint/Low, An Yee/Summer Project - Alexis An Yee Low/";},
       LinuxMM = {baseDir <- "/home/michael/gitwork/LikeMe/";},
       GeertJanMac= {baseDir <- "/Users/geert-janwill/Dropbox/GJW_LikeMe/"; })

codeDir <- paste(baseDir,"likeme-Socio3/",sep='')
# --------------------------------------------------------------------------

# LikeMe.R has most of the functions that for this project :
source(paste(codeDir,'LikeMe.R',sep='')); 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# SLPsocio3 is the second, more advanced 'Inference by counting model' (Michael's model B)
# To test it insides, run the first 55 lines of roughInferSE.R, incl. D <- D17 , then:
#                                                accP0 sensi sesh a0min n0 nMax Tpred Bpred nBal #A: what is accP0, sensi, sesh, Bpred and nBal
# ptN=1; datAr=D; onlySLP=0; check=1; parMat =  c(0.67, 1.5, 1.25,  1.1,  4,  8,   0.15,  0.2,  5 ) 
# To re. approval predictions, try: test <- SLPsocio3(parMat,D); testD <- test$genD; l=matrix(NA,60,3); for (k in 1:60){ test <- SLPsocio3((0.7+0.01*k)*parMat,testD); l[k,] <- c((0.7+0.01*k),test$predSLnP, test$SESLnP); }; plot(l[,1],l[,2],t='l', main='appr. predictions SumLL'); abline(v=1); abline(h=(max(l[,2]-3)))
#A: l is a matrix created w parameters (no data, 60 rows, 3 columns). then for 1 to 60 put in the output of SLPSocio3 when applied to testD using different multiples of parameter, from 0.7 to 1.3. to see which is the parameter that gives these results as most likely. (ideally 1). y axis is 1, x axis is peak minus 3 (to show 95% CI?)
SLPsocio3 <- function( parMat, datAr,onlySLP=0, check=1){
# parMat has rows with the parameters for each pt.
# datAr has a page for each pt, and Ntr rows., gp pred  obs SE cols.
#
# par. row must be: c('accP0', 'sensi', 'sesh', 'a0min', 'n0', 'nMax','Tpred', 'Bpred','nBal')
# e.g.   parMat =   c(0.67,     0.75,     2,     0.5,      4,    6,     0.2,    0.1,    5 ) 
#        sensitivity of pAcc->SE, threshold of pAcc->SE etc.  #A: how 'accurate probability' is mapped to SE. the former is using a/b values in pt's head. (I think)

 M <- 4;          # Number of rater groups.
 SEgenMeth <- 2;  # should generated SE be random (1) from component (group) distro, 
                  # expectation (0), overall beta (2) ... #A: combining all 4 groups into 1 beta distribution.
 eps <- 1e-10;    # a very small number to catch ln(0) etc ...

 if (check){
  datDim <- dim(datAr);
  if (length(datDim)<3){   # if 2D object was provided, convert to 3D. #A: matrix to array I think
    hd <- colnames(datAr);
    datAr <- array(as.matrix(datAr),c(datDim,1));
    colnames(datAr) <- hd; #A: hd is header?
   }
  if (is.null(dim(parMat))) {
    parMat <- array(parMat,c(1,length(parMat)));
  }
 }
 colnames(parMat) <- c('accP0', 'sensi', 'sesh', 'a0min', 'n0', 'nMax','Tpred', 'Bpred','nBal')
 
 Nptot <- dim(datAr)[3]; #A: dim(datAr) is in the former rows, columns, pages.
 Ntrtot <- dim(datAr)[1];# this will sadly have to be fiddled with later, usually to
                 # Cater somehow for the fact that scanned task contains interruptions.
  bakD <- datAr;     # backup copy to restore later ...
  stimTr <- array(NA,c(Ntrtot,Nptot));  # will store positions of stimulus-endowed trials. #A: empty array with #trials rows #pts columns
  
 # Array to hold alpha, beta, n-so-far for each group of raters,
 # as well as the probability / prob. dens. for the responses emitted,
 # and generated acceptance prediction and SE. expSE is the 'the SE' point estimate.
 # Note has one more row than trials as starts from 'priors'.
 abnPol <- array(dim=c(Ntrtot+1,3*M+5,Nptot)); 
 dimnames(abnPol)[[2]] <- c(paste(c('a','b','n'),repAdjVec(1:M,3),sep=''), #A: assign the column (aka dimension bc column is a dimension) headers for the second page
                            'obsP','SEPD','accP','genSE','expSE'); #A: paste is contatenate. so a1, b1, n1, a2, b2...... n4
 aInd <- (0:(M-1))*3+1;   # Indices of a in abnG
 bInd <- aInd+1;
 nInd <- bInd+1;
 obsPI <- 3*M+1;         SEPDI  <- obsPI+1 ;
 accPI <- SEPDI+1;       genSEI <- accPI+1;     expSEI <- genSEI+1;
 
 for (ptN in 1:Nptot){
     
    # Cater somehow for the fact that scanned task contains interruptions #A: what is scanned task
    # including at the very start. Awkward - to start with, just exclude:
    stimTr[,ptN] <- !is.na(datAr[,'gp',ptN]); #A: if there is a group indicated, a trial happened, so put 'true'. result - column of TRUEs
    Ntrtot <- sum(stimTr[,ptN]);    # update to exclude non-stimulus trials. Was dim(datAr)[1] above. #A: find the actual # of trials that happened
    datAr[1:Ntrtot,,ptN] <- datAr[stimTr[,ptN],,ptN];  #A: use the data from stimTR[,ptN] (true, false) to select what to continue to include in datAr
    if (Ntrtot<dim(datAr)[1]) { #A: first thing ins dim(datAr) is the number of rows. remember #rows ia a dimension. so this means if number is lower after exclusion
      datAr[(Ntrtot+1):dim(datAr)[1],,ptN] <- NA; #A: then assign NA to rest of the rows.
    }
    
   # Initial beliefs about ratings by others (groups)
   # alpha0s spaced up from a0min geometrically so that aBase makes sense :
   d <- 1 - parMat[ptN,4]/parMat[ptN,5]          # 1- a0min / n0 #A: initial beta
   r <- exp(2/3*log((1-parMat[ptN,1])/d)); #A: parMat columns: 'accP0', 'sensi', 'sesh', 'a0min', 'n0', 'nMax','Tpred', 'Bpred','nBal'. this is something like getting a/b for 4 diff groups? parMat [ptN,1] is accP0. but note accP0 = aBal/nBal (ballast) not a0/n0... so there's a difference used here?
   abnPol[1,aInd,ptN] <- parMat[ptN,5]*(1-d*r^(3:0)); #A: first row is no trial yet. initial stage, this sets each group's first a to be n0 * different acceptance prob, generated by alpha = 1 - different betas
       # linear, when parMat gave a0max and a0min, was
       # ((M-1):0)*(parMat[ptN,3]-parMat[ptN,4])/(M-1) + parMat[ptN,4];
   abnPol[1,bInd,ptN] <- parMat[ptN,5]-abnPol[1,aInd,ptN];    
   abnPol[1,nInd,ptN] <- parMat[ptN,5];    

   nMax <- parMat[ptN,6];   # the whole of this available for task in this version.
   adecay <- (nMax-3)/(nMax-2) ;  # to be used for 'too much data to remember'

   # parameters for mapping acceptance py to SE (don't use lower case a, b !)
   A <- parMat[ptN,'sensi'];          B <- parMat[ptN,'sesh'];
   # The shift or offset sesh has to be consistent with baseline SE and other beliefs.
   # so that if expectations above the approval rates etc. turned out to be true, 
   # then self-evaluation would, remain stable. So, if the above were true, the 
   # average acceptance rate would be:
   nBal <- parMat[ptN,'nBal'];
   aBal <- parMat[ptN,'accP0']*nBal;    bBal <- nBal - aBal; 
   n0   <- parMat[ptN,'n0'];
   accPeff <- ( aBal + mean(abnPol[1,aInd,ptN])*nMax/n0) / #A: (mean of all the initial alphas divided by initial n then multiplied by nnmax = alpha if nmax, then add ballast alpha
              ( nBal + nMax);
   abnPol[1,'accP',ptN] <- accPeff; #A: this is the actual experimental initial acceptance probability in pt's head generated from all these parms!!!
   # And this would correspond to an 'equilibrium SE' of:
   abnPol[1,'expSE',ptN] <- accP2SE(accPeff,A,B); #A: using a function that maps acceptance probability to SE.
   
   # Reminder - n0 is the denominator for a0, but it is included in the data that
   # will be modified, so the notional data it denotes is subset of the max that will
   # be included in the Nmax :
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
   Tpred = parMat[ptN,'Tpred'];   # this and Bpred below scaled to be
   Bpred = parMat[ptN,'Bpred'];   # tuned to probability-like calcs ...
     
   # loop over trials
   for (trN in 1:Ntrtot){
     abnPol[trN+1,,ptN] <- abnPol[trN,,ptN];   # Initialise-most will remain same.
  
     gpI <- datAr[trN,1,ptN];  # which group rated this pt #A: I needed to add an extra 1 to work 1,1
     if (is.na(gpI)){  # if there was no valid group i.e. no 'rater' was presented
           # just keep propagated beliefs but don't attepmt anything of substance
      abnPol[trN+1,c(SEPDI,obsPI,accPI,genSEI,expSEI),ptN] <- NA;
      
     } else { # if there was valid group i.e. valid 'rater' was presented
     # Prob. of 'accept' response emitted (this is BEFORE rating seen),
     # if present. NB we will use beliefs after last trial, i.e. abnPol[trN,...(see notes if confused) 
     ratingP <- 1/(1+exp((1-2*(abnPol[trN,aInd[gpI],ptN]/abnPol[trN,nInd[gpI],ptN] +
                                          Bpred))/Tpred)); 
     abnPol[trN+1,accPI,ptN] <- ratingP;  # Store                          
    
     predAcc <- datAr[trN,2,ptN]; # rating that actual participant predicted. #A: binary, 1 or -1
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
     if (!nofb) {  # If not feedback was given, we' leave all the a,b,n alone, which 
                   # are already in place. !nofb means that feedback was given, so:
        apprfb = (datAr[trN,3,ptN]+1)/2 ; # approval or not, i.e. convert from -1 1 to 0 1

        # a only allowed to go to nMax-1, to allow b >= 1 :
        abnPol[trN+1,aInd[gpI],ptN] <- 1+ apprfb +
                                       adecay*(abnPol[trN,aInd[gpI],ptN]-1) ;  
        # Similar constraint on b gives condition for n : 
        abnPol[trN+1,nInd[gpI],ptN] <- 3 + adecay*(nSoFar - 2);
       
        # Again, just for completeness, b:
        abnPol[trN+1,bInd[gpI],ptN] <- abnPol[trN+1,nInd[gpI],ptN] - abnPol[trN+1,aInd[gpI],ptN];
    
     } # end if valid feedback given
         
     # Consider SE as a map from prob. of acceptance to a scale over c(0,1)
     # & calc. p density at the new SE reported, if valid. IT HAS TO CORRESPOND TO THE
     # WAY SE IS GENERATED (e.g. for synthetic data ...)
     # Generated SE may be: (1) random from the component (group) distro at hand; or
     #                      (2) random from an 'overall' distro, where we express the 
     #                      overall SE distribution as derived from a mixture of Beta distros.
     #               
     #                      ( or possibly from some central tendency with independent noise)
     #
     
     #  ------------ 'reported' and generated-data SE -----------------------
     if (SEgenMeth==1)  {  
       # The SE beta distros here do include 'ballast' or 'activated schema'
       # notional data :
       a <- abnPol[trN+1,aInd[gpI],ptN] + aBal ; 
       b <- abnPol[trN+1,bInd[gpI],ptN] + bBal ; 
       # Now fill in expected and sampled SE !
       abnPol[trN+1,expSEI,ptN] <- accP2SE(a/(a+b), A, B);#A: remember: generate SE from probability, sensitivity, and shift. higher sensitivity = steeper, higher shift = more self esteem,
       abnPol[trN+1,genSEI,ptN] <- accP2SE(rbeta(1,a,b), A, B);  #A: similar, but for probability it's a (n =1) random probability from a distribution of probabilities
       
     } else if (SEgenMeth==2) {  # 'reported' SE in genSE col. from an
       #    overall beta ... 
       a <- mean(abnPol[trN+1,aInd,ptN]) + aBal;
       b <- mean(abnPol[trN+1,bInd,ptN]) + bBal;   
       abnPol[trN+1,expSEI,ptN] <- accP2SE(a/(a+b), A,B);
       # for debug:  abnPol[trN+1,genSEI,ptN] <- abnPol[trN+1,expSEI,ptN]; 
       abnPol[trN+1,genSEI,ptN] <- accP2SE(rbeta(1,a,b), A,B);
       
     }  # end if-else about how to map beliefs to SE
     # End generate SE values, expected and generated 'to report'. 
     
     #  Now for the probability density at the actually measured SE
     #  in the experiment, if valid, using a and b calculated above:
     SEdat = datAr[trN,4,ptN] ; 
     if (is.na(SEdat)) {
       abnPol[trN+1,SEPDI,ptN] <- NA ;
     } else if (SEgenMeth==1) {
       #  Expressed SE in terms of an acceptance probability :
       experAccP <- SE2accP( SEdat,A,B); #A: accurate probability from experiment. measure then map from SE to accP
       #  Acceptance belief density at that point acc. to the
       #  belief density about the current group (rater):
       accPdens <- dbeta( experAccP, a, b) ;
       # To get SE density, scale by the slope of the accP(SE) map : 
       abnPol[trN+1,SEPDI,ptN] <- accPdens * slopeSE2accP(SEdat,A,B,accPdens);
     } else if (SEgenMeth==2) {
       #  Expressed SE in terms of an acceptance probability :
       experAccP <- SE2accP( SEdat,A,B); #A: this function converts experimental SE to experimental AccP
       #  Acceptance belief density at that point acc. to 
       #  'mean parameters' over all the groups:
       accPdens <- dbeta( experAccP , a, b); #A: this one calculates p density at x = experAccP in the beta function
       # SE density scaled by the slope of the accP(SE) map : 
       abnPol[trN+1,SEPDI,ptN] <- accPdens * slopeSE2accP(SEdat,A,B,accPdens); #A: probability density * ... don't entirely understand this but some scaling
     }
     # end if there was a valid SE measurement i.e. if VAS rating was obtained.
    
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
    colnames(SLPetc[[5]]) <- c('accP0','sensi','sesh','a0min','n0','nMax','Tpred','Bpred','nBal');
    names(SLPetc) <-c('predSLnP','SESLnP','DatBelPol','genD','ptPar');
    
    return(SLPetc);
  }
    
} # end of SLPsocio3  - 'Counting model' B (Michael's)

# -----------------------------------------------------------------------------

# Param transf. for SLPsocio3, using 9-element input.
# par. row must be: c('accP0', 'sensi', 'sesh', 'a0min', 'n0', 'nMax','Tpred', 'Bpred','nBal')
# e.g.   parMat =   c(0.67,     0.75,     2,     0.5,      4,    6,     0.2,    0.1,    5 ) 
#  to       atanh(2accP0-1),ln sensi,ln sesh,ln a0min,ln(n0-a0min),ln(nMax-n0),ln Tpred, Bpred,ln (nBal-1)
#        sensitivity of pAcc->SE, threshold of pAcc->SE etc.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

tr2natLP3 <- function(trp,check=1){  # from transformed, i.e. -inf to inf, to native space
#  trp to be as follows;
#      c('atanh(2accP0-1)', 'ln(sensi)','ln(sesh)', 'ln(a0min)', 'ln(n0-a0min-1)', 'ln(nMax-n0-2)','ln(Tpred)', 'Bpred', 'ln(nBal-1)') 
#      Note in particular how we restrict n0 to above 1 and  nMax=Ntask > n0+2 > 1+2, as we want Ntask > 3 
#      so the delta learning rule wil work as per  aDecay = (Ntask-3)/(Ntask-2) > 0 ..
#  Returns: c('accP0', 'sensi','sesh', 'a0min', 'n0', 'nMax','Tpred', 'Bpred','nBal')
  
  eps <- 1e-10;  #  eps is a tiny constant that can be used to guarantee
                 #  that rounding errors, underflows etc. don't ruin strict inequalities.
  
  if (check){ if (is.null(dim(trp))){   # convert vec to mat if need be
      trp <- matrix(trp,nrow=1,byrow=TRUE) }   }
  ptTot <- dim(trp)[1]; 
  p <- matrix(NA,nrow=ptTot,ncol=dim(trp)[2]); 

  p[,4] <- exp(trp[,4]) ;           # a0min can go down to almost zero
  p[,5] <- p[,4]+1 + exp(trp[,5]);  # n0 :  restrict to above 1 so that  nMax=Ntask > n0+2 > 1+2
                                    # we want Ntask > 3 so the delta learning rule works as per
				                            # aDecay = (Ntask-3)/(Ntask-2) > 0 ..
  p[,6] <-  p[,5] + 2 +  exp(trp[,6]);  # as per rationale above.

  p[,1] <- 0.5*(1 + tanh(trp[,1])) ; # accP0
  p[,c(2,3,7)] <- exp(trp[,c(2,3,7)]);  #  'sensi','sesh', (a0min done already), 'Tpred' 
  p[,8] <- trp[,8];
  p[,9] <- exp(trp[,9]) + 1;  # nBal
  
      
  if (check){ colnames(p) <- c('accP0', 'sensi','sesh', 'a0min', 'n0', 'nMax','Tpred', 'Bpred','nBal') };
  return(p); 
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

nat2trLP3 <- function(p,check=1){  # From native to transformed.
  #  Returns trp as follows;
  #      c('atanh(2accP0-1)', 'ln(sensi)','ln(sesh)', 'ln(a0min)', 'ln(n0-a0min-1)', 'ln(nMax-n0-2)','ln(Tpred)', 'Bpred', 'ln(nBal-1)') 
  #      Note in particular how we restrict n0 to above 1 and  nMax=Ntask > n0+2 > 1+2, as we want Ntask > 3 
  #      so the delta learning rule wil work as per  aDecay = (Ntask-3)/(Ntask-2) > 0 ..
  
  
  eps   <- exp(-25);   # So that for R 1+eps > 1
  minLn <- -1000;   # so that for R exp(minLn) == 0, exp(-minLn) == +Inf
  
  # Basic check - of argument format
  if (check > 0){ if (is.null(dim(p))){   # convert vec to mat if need be
    p <- matrix(p,nrow=1,byrow=TRUE) }   }    
  ptTot <- dim(p)[1]; 
  # Detailed check
  if (check > 1){
    for (ptN in 1:ptTot) {
      if (sum(p[ptN,c(2,3,4,7)] < -2*eps)) { 
        print(paste('parMat 2,3,7 =',p[ptN,c(2,3,4,7)]));
        stop('--> ln of -ve    error' ); 
      }
       if ((p[ptN,5]-p[ptN,4]-1) < -2*eps) {
        stop('ln(n0-a0min-1) error' ); 
      }
      if ((p[ptN,6]-p[ptN,5]-2) < -2*eps){
        stop('ln(nMmax-n0-2) error : restrict so that  nMax=Ntask > n0+2 > 1+2 as want Ntask > 3 so the delta learning rule works as per aDecay = (Ntask-3)/(Ntask-2) > 0 .')
      }
      if ((p[ptN,9]-1) < -2*eps) {
        stop('ln(nBal-1) error' ); 
      }
      
    }
  }
  
  trp <- matrix(NA,nrow=ptTot,ncol=dim(p)[2]); 
   
  y <- p[,4];        y[y<eps] <- eps; 
  trp[,4] <- log(y);            # ln(a0min)
  y <- p[,5]-p[,4]-1;  y[y<eps] <- eps; 
  trp[,5] <- log(y);            # ln(n0-a0min-1)
  y <- p[,6]-p[,5]-2  ;  y[y<eps] <- eps; 
  trp[,6] <- log(y);            # ln(nMax-n0-2)
  y <- p[,9]-1;
  trp[,9] <- log(y);            # ln(nBal-1)
  

  # the others :
  trp[,1]  <- atanh(2*p[,1]-1);
  y <- p[,c(2,3,7)];           y[y<eps] <- eps;   trp[,c(2,3,7)] <- log(y);
  trp[,8] <- p[,8];           # Bpred doesn't need to be transformed at all
  # rough bounding of under / overflows:
  trp[trp < minLn] <- minLn;    trp[trp > -minLn] <- -minLn;
  

  if (check){ colnames(trp)<- c('atanh(2accP0-1)', 'ln(sensi)','ln(sesh)', 'ln(a0min)', 'ln(n0-a0min-1)', 'ln(nMax-n0-2)','ln(Tpred)', 'Bpred', 'ln(nBal-1)') }
  return(trp);

}  # end of nat2trLP3

msLP3tr <- function(trParM, datAr, gamPri=NA, check=0){
  # trParM: transf. directly by tr2natLP1  
  #   c('tr(SEb,SEmin)','tr(aB OR bB)','ln(a0min-1)',
  #                               'ln(n0-a0min-1)','tr(Nmax etc)','ln(Tresp)')
  # gamPri has the means (row1) and sd's (row2) for gamma priors on ParM IN NATIVE
  # SPACE !!!
  
  if (is.null(dim(trParM))){   # turn trParM into matrix if it's a vector
    trParM <- matrix(trParM,nrow=1,byrow=TRUE)      }
  
  if (check){
    if ((dim(datAr)[2]<4) || (dim(trParM)[2]<9)){
      stop('arguments trParM or datAr appear to have wrong dimensions');  }
  }
  
  
  parM <- matrix(NA,nrow=dim(trParM)[1],ncol=dim(trParM)[2]+1); 
  
  #A: If using this function in a situation where 1 or more parameters 
  #are fixed, add this line (402). Change as necessary. For how nlm was done 
  #with fixed parameters, see end of file
  #trParM <- cbind( trParM[,1], log(5), log(2), trParM[,2], trParM[,3], trParM[,4], trParM[,5], trParM[,6], trParM[,7])
  parM <- tr2natLP3(trParM)
  
  # Cacl. the log prior for MAP purposes etc:
  mSLPrior <- 0;
  if (length(gamPri)>1){  # legit prior must have 12 elements or so!
    for (ptN in 1:dim(trParM)[1]) {
      mSLPrior <- mSLPrior - sum(dgammaMS(parM[ptN,], gamPri[1,],gamPri[2,], log=TRUE)); 
    }
  } 
  
  # debug line:
  #print(paste('Prior density:',mSLPrior))
  #print(cbind(parM,trParM));
  
  if (mSLPrior == Inf){  # If we are in an a priori prohibited parameter region
    # do not attempt to calculated the likelihood - it will be nonsense anyway.
    return(Inf); 
  } else {
    return ( mSLPrior - SLPsocio3( parM, datAr, onlySLP=1, check) ); #A: this is what is minimized - the difference between priors and actual (as gone through SLPsocio1)
  }
  
}  # end of msLP3tr 

#A: How to do nlm with fixed parameter(s)

# parMat = c(0.5, 5, 2, 0.5, 4, 6, 0.2, 0.1, 10)
# slp3.17 <- SLPsocio3(parMat,D)
# simD <- slp3.17$genD; 
# simSLP3 <- SLPsocio3(parMat,simD);
# cat('    predSLnP           SESLnP\n',paste(c(simSLP3[[1]],simSLP3[[2]]))) 
# #skipping the 1.2x stuff for now
# p3tr <- nat2trLP3(parMat)
# p3tr <- p3tr[,-2]
# p3tr <- matrix(p3tr,nrow=1,byrow=TRUE)
# p3tr <- p3tr[,-3]
# p3tr <- matrix(p3tr,nrow=1,byrow=TRUE)
# simFit <- nlm(msLP3tr, p3tr, simD, print.level=2, iterlim=100);
# simFit$estimate <- c(simFit$estimate[1], log(parMat[2]), log(parMat[3]), simFit$estimate[2:7]) #if fixing parameter 2
# estp <- (tr2natLP3(simFit$estimate)) ; 
# print( round(estp,3) ); #A: although this says estp, 'correct' parameters for the fixed parameter value(s) have been 'slotted' back in

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#                                          end of file

