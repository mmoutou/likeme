# Main file with stable self-esteem functions.
# Please use roughLikeMe as a working file and copy 'finished' functions here.


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
       SpectreMM = {baseDir <- "C:/Users/mmoutou/OneDrive/SharePoint/Low, An Yee/Summer Project - Alexis An Yee Low/";},
       LinuxMM = {baseDir <- "/home/michael/gitwork/LikeMe/";},
       GeertJanMac= {baseDir <- "/Users/geert-janwill/Dropbox/GJW_LikeMe/"; })

codeDir <- paste(baseDir,"likeme-Socio3/",sep='')
# --------------------------------------------------------------------------


# Misc. auxiliaries / demos:
#   d200mock <- function(SE0=2/3, L0=0.3, eps=0.01)
#
#
#   apprPr( th=1, Sth=1, Mth=0, debug=0) : approx. proportion of the
#        population would be laiked by a group with threshold th etc.
#   aNew(thisApr, aPrev, N) : update a if SE is a belief distro of Beta
#        form with a poss. going from 1 to N+1.
# -------------------------------------------------------------------------
# Geert-Jan task models furninshing generated data and SLnP for real data.
# All can produce parametrisation of evolving belief, followed by
#    ... ...  obsP    SEPD     accP     genSE
#   where the obsP -> Py of experimentally observed prediction,   
#   SEPD -> Prob. density at expt. obs. SE, accP -> (generated) acceptance P,
#   genSE -> generated (point estimate) SE.  
#   
#   SLPsocio0(parMat, datAr, onlySLP=0, fixAcc=NA, check=1){
#        parMat has rows with the parameters for each pt.
#        datAr has a page for each pt, and Ntr rows.
#       par. row: SEb, acc0max, acc0min, eta, gam, wexp, wrpe, Tresp,  sig
#   msLP0tr(trParM, datAr, check=0)
#        trParM only to have atanh(2*SEb-1), log(gam,  wexp,  wrpe, Tresp,  sig)
#   mse(parM,check=1)
#   tr2natLP0(trParM,check=1)
#
#   SLPsocio1( parMat, datAr, onlySLP=0, check=1) : Based on 'beta beliefs'
#           datAr as above,
#           parMat e.g.:  aBase, bBase, a0max, a0min, n0,   Nmax, Tresp
#                           20.0,  5.0,  7.0,   4.0,  10.0,  33.0   0.2
#   SLPsocio2( parMat, datAr, onlySLP=0, check=1) : Based on 'Scale of
#           likeability' by Karl
#
#   - - - - - - - - - - - - - - - - - 
#   genLikeMe( SLP ) 
#       Generative, to make fake data: Take output of SLPsocio* (full version!)
#       and put together an object with pages of      gp  pred  obs       SE

## First, misc. demos and auxiliaries ####################################

try(source(paste(codeDir,'gen_ut.R',sep='')));
#try(source(paste(baseDir,'Dropbox/GJW_LikeMe/Rscripts/gen_ut.R',sep='')));
# import key data:
try(load(paste(baseDir,'datAr61.RData',sep='')));  
#try(load(paste(baseDir,'/Dropbox/GJW_LikeMe/datAr61.RData',sep='')));  

#w03 <- importCSV(paste(baseDir,'Dropbox/BASOR/BASOR_output/GJW_LikeMe/GJW03.csv',sep=''));
#ptTotN03 <- w03[dim(w03)[1],1];    # ID of last pt ... ?
#totTrN <- c(); for (i in 1:ptTotN03){
#  totTrN[i] <- dim(w03[w03[,1]==i,])[1]; 
#}
#totTrN <- na.omit(totTrN); # no pts. with NA trials !
#ptTotN03 <- length(totTrN);
#datArW03 <- array(NA,c(max(totTrN),dim(w03)[2]-1,ptTotN03));
#for (i in 1:ptTotN03){
#  datArW03[1:totTrN[i],,i] <- as.matrix(w03[w03[,1]==i,2:dim(w03)[2]]);
#}
#colnames(datArW03) <- c('gp','pred','obs','SE');
Dhd =  c('gp','pred','obs','SE','nofb');
datArW03 <- datAr61[,Dhd,];
ptTotN03 <- dim(datArW03)[3];
totTrN <- c(); for (i in 1:ptTotN03){
  totTrN[i] <- length(na.omit(datArW03[,1,i]));
}


# useful individual participants - first the ones Geert-Jan used as examples:
D04 <- array(NA,c(dim(datArW03)[1:2],1));
D14 <- array(NA,c(dim(datArW03)[1:2],1));
D19 <- array(NA,c(dim(datArW03)[1:2],1));
# and another few: 12, 17, 1 ... 
D12 <- array(NA,c(dim(datArW03)[1:2],1));
D17 <- array(NA,c(dim(datArW03)[1:2],1)); # This has marked linear trend.
D01 <- array(NA,c(dim(datArW03)[1:2],1));
D04[,,1] <- datArW03[,,4];
dimnames(D04)[[2]] <-Dhd;
dimnames(D04)[[3]] <- 4;
D12[,,1] <- datArW03[,,12];
dimnames(D12)[[2]] <-Dhd;
dimnames(D12)[[3]] <- 12;
D17[,,1] <- datArW03[,,17];
dimnames(D17)[[2]] <-Dhd;
dimnames(D17)[[3]] <- 17;
D19[,,1] <- datArW03[,,19];
dimnames(D19)[[2]] <-Dhd;
dimnames(D19)[[3]] <- 19;
D14[,,1] <- datArW03[,,14];
dimnames(D14)[[2]] <-Dhd;
dimnames(D14)[[3]] <- 14;
D01[,,1] <- datArW03[,,1];
dimnames(D01)[[2]] <-Dhd;
dimnames(D01)[[3]] <- 1;


FixAcc <- c(0.85, 0.7, 0.3, 0.15);   # the default acceptance rates ... 

# ========================================================================
# Function to check to what extend participant's SE responded to being
# liked / disliked. SEdat can be e.g. the 3rd list element of the 
# detailed version of output of SLPsocio1 : 
TDSE <- function( SEdat ) {
  # Make sure SEdat has 'pred','obs','SE' columns
  # Elementary transf. to see how much SE goes up and down
  # as a fn. of sum of PEs and of approvals since last 
  # trial where SE was enquired about
  
  rN <- dim(SEdat)[1]; # no. of rows 
  d <- as.data.frame(matrix(NA,nrow=rN,ncol=6))
  colnames(d) <- c('pred','obs','SE','sPE','sAp','TDSE');
  d[,c('pred','obs','SE')] <- SEdat[,c('pred','obs','SE')];
  
  T <- 1;
  while(T <= rN) {
    # find next valid SE
    while(is.na(d[T,'SE']) && T<=rN){ T<-T+1; }
    
    # commence accum. PEs just up to previous valid SE
    sPE <- d[T,'obs'] - d[T,'pred'];  # itit. at present trial
    sAp <- d[T,'obs'] ;
    # start summing backwards :
    t<- 1;
    while(is.na(d[T-t,'SE']) && (T-t > 0)){
      if (!is.na(d[T-t,'obs'])){
        sPE <- sPE + d[T-t,'obs'] - d[T-t,'pred'];
        sAp <- sAp +  d[T-t,'obs'] ; 
      }
      t <- t+1;
    }
    d[T,'sPE']  <- sPE;          d[T,'sAp'] <- sAp; 
    if ((T-t)>0) {
      d[T,'TDSE'] <- d[T,'SE'] - d[T-t,'SE'];
    }
    T <- T+1;
  }
  
  return(d);
}



# What approx. proportion of the population would be liked by a group with
# threshold th and T=1 logistic likeing if the likeability was Norm. distr. 
# in the pop with mean Ml and sigma Sl ?


apprPr <- function( th=1, Sth=1, Mth=0, debug=0){
  # ap <- apprPr(th=3.5*((1:4)-2.5),  Sth=5  )
  # ... will give something roughly comparable to Geert-Jan's proportions.
  
  apprInf <- 10; # Ignore stuff more than apprInf*sd off mean
  res <- 20;    # resolution per SD. NB needs to be >> 1 so at least 20
  l <- Mth + Sth*(-(apprInf*res):(apprInf*res) )/res ;
  # check with plot(l,dnorm(l,Mth,Sth))
  # Now get the prob density of l :
  lpd <- dnorm(l,Mth,Sth)/res;  lpd <- lpd/sum(lpd);
  # Now the approval density:
  thN <- length(th);
  lN  <- length(l);
  apd <- matrix(nrow=lN,ncol=thN);
  for (i in 1:thN){
    apd[,i] <- 1/(1+exp(th[i] - l));
  }
  
  aPr <- as.vector(lpd %*% apd);
  if(debug==0){
    return(aPr);
  } else {
    apPr <- list();
    apPr[[1]] <- aPr;
    apPr[[2]] <- data.frame(l,lpd,apd);
    colnames(apPr[[2]]) <- c('l','lpd','apd');
    names(apPr) <- c('aPr','D');
    return(apPr); 
  }
  
}
 
# -------------------------------------------------------------------
# Function to create 200 nice pseudodata trials for 1 pt, 
# for understanding the models:

d200mock <- function(SE0=2/3, L0=0.3, eps=0.01) {
  
  # Construct some orderly stimuli for understanding the models.
  d0a <- array(rep(NA,200*5*1),c(200,5,1)); 
  colnames(d0a) <- c('gp','pred','obs','SE','nofb');
  # Ordered presentation of groups :
  d0a[,1,1] <- rep(1:4,50);
  # 4 out of 5 of group 1 give approval, 3/5 of gp 2 and so on:
  d0a[d0a[,1,1]==1,'obs',1] <- rep(c(1,1,0,1,1),10);
  d0a[d0a[,1,1]==2,'obs',1] <- rep(c(1,0,0,1,1),10)
  d0a[d0a[,1,1]==3,'obs',1] <- rep(c(1,0,0,0,1),10);
  d0a[d0a[,1,1]==4,'obs',1] <- rep(c(0,1,0,0,0),10);
  truAcc <- c(4/5,3/5,2/5,1/5);   # true acceptances
  # Assume this model agent has a Tres of 0.2 and uses the same sigmoid
  # to map beliefs to responses, and has correct beliefs:
  Tresp <- 0.2;
  #SE0 <- 1/3;              # This person starts with lowish SE.
  #L0 <- 0.2;               # Base learning rate for SE.
  for (trN in 1:200){
    ansP <- 1/(1+exp((1-2*truAcc[d0a[trN,1,1]])/Tresp));
    d0a[trN,'pred',1] <- rbinom(1,1,ansP); 
  }
  # The following just gradually adjusts SE to the 
  # overall acceptance rate experienced, using true beliefs
  # for rpe :
  SE <- SE0;
  for (trN in 1:200) {
    rpe <- d0a[trN,'obs',1] - truAcc[d0a[trN,'gp',1]]; 
    
    # Adjust w.r.t. unit interval:
    if (rpe > 0){ rpe <- (1-SE)*rpe; };
    if (rpe < 0){ rpe <- SE*rpe; }
    
    SE <-  SE + (L0/trN)*rpe;
    
    # Now perturb with some rpe-like noise
    if (eps > 0){
      rpn <- rnorm(1,0,eps);
      if (rpn > 0){ SE  <- SE + tanh(rpn)*(1-SE)};
      if (rpn < 0){ SE  <- SE + tanh(rpn)*SE;   };
    }
    
    d0a[trN,'SE',1] <- SE;
  }
  
  return(d0a);
  
}



# ----------------------------------------------------------  
# update a if SE is a belief distro of Beta form
# with a poss. going from 1 to N+1. It is implied that
# similarly b = N+2-a . REM in R the mimimal hump is beta(x,2,2)
aNew <- function(thisApr, aPrev, N){
 if ((aPrev < 1) || (aPrev > N+1)){
   stop('must have 1 <= aPrev < N+1')
 }
 return( 1 + (1- 1/N)*(aPrev-1) + thisApr );
}
 
## ###############################################################
## #             Models for Geert-Jan's experiment               #
## ###############################################################

# --------------- 'Happiness-like model' (Robb's) ----------------

## SE model based on the 'happiness equation',
#  SE = SE0 + wrpe \Sum RPE_t gam^t  + wexp \Sum expSE gam^t
#  and group acceptances learn w. initial values acc0[1:M] and learning eta
#  Published in https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5655144/

SLPsocio0 <- function( parMat, datAr, onlySLP=0, fixAcc=NA, check=1){
  # parMat has rows with the parameters for each pt.,
  # par. row: SEb, acc0max, acc0min, eta, gam, wexp, wrpe, Tresp, sig
  # datAr has a page for each pt, and Ntr rows.
  #
  # For testing & example use see roughLikeMe.R, wherein 
  # can use parMat = par0a; datAr = D; onlySLP=0; fixAcc=gpAcc; check = 1; 
  # and at the end remove(parMat,datAr,onlySLP,fixAcc,check,hapPol)
  
  M <- 4; # Number of rater groups.
  knmax <- 200; # 10;  # max. depth of kernel summation.
  gamNorm = 0;  # 0 ;= don't normalize the decay kernel
  
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
  # To check and add accHalf as last entry of parMat, so that 50% response becomes:
  # ratingP <- 1/(1+exp((parMat[ptN,'accHalf']-hapPol[trN,expInd[gpI],ptN])/
  #                                    (parMat[ptN,'accHalf']*parMat[ptN,'Tresp'])));
  colnames(parMat) <- c('SEb','acc0max','acc0min','eta','gam','wexp','wrpe','Tresp','Hresp','sig');
  Nptot <- dim(datAr)[3];
  
  Ntrtot = dim(datAr)[1];  # this will sadly have to be fiddled with later, usually to
                 # Cater somehow for the fact that scanned task contains interruptions.
  bakD <- datAr;     # backup copy to restore later ...
  stimTr <- array(NA,c(Ntrtot,Nptot));  # will store positions of stimulus-endowed trials.
  
  # Array to hold expectations for each group of raters, RPE,
  # as well as the probability / prob. dens. for the responses emitted,
  # and generated acceptance prediction and SE. So each completed row
  # refers to the values at the END OF THAT TRIAL, i.e. includes the
  # RPE experienced on the basis of the EV JUST BEFORE the trial but the
  # updated EVs AFTER updating with the observation.
  # Note has one more row than trials as starts from 'priors'.
  hapPol <- array(dim=c(Ntrtot+1,M+6,Nptot)); 
  dimnames(hapPol)[[2]] <- c(paste('exp',1:M,sep=''), 'RPE',
                             'obsP','SEPD','accP','genSE','expSE');
  expInd <- 1:M    # Indices of group expectations in hapPol 
  rpeInd <- M+1;   # index for  RPE, 
  obsPI <- rpeInd+1;       SEPDI  <- obsPI+1 ;
  accPI <- SEPDI+1;       genSEI <- accPI+1;   expSEI <- genSEI+1;
  
  for (ptN in 1:Nptot){
    
    # Cater somehow for the fact that scanned task contains interruptions
    # including at the very start. Awkward - to start with, just exclude:
    stimTr[,ptN] <- !is.na(datAr[,'pred',ptN]); #altered GJW
    Ntrtot <- sum(stimTr[,ptN]);    # update to exclude non-stimulus trials. Was dim(datAr)[1] above.
    datAr[1:Ntrtot,,ptN] <- datAr[stimTr[,ptN],,ptN];  
    if (Ntrtot<dim(datAr)[1]) {
      datAr[(Ntrtot+1):dim(datAr)[1],,ptN] <- NA; 
    }
    
    # Initial beliefs about ratings by others (groups):
    if (!is.na(sum(fixAcc))){
      parMat[ptN,'eta'] <- 0.0;
      # hapPol[1,expInd,ptN] <- fixAcc[expInd];   # this will hopefully fail if rubbish fixAcc is given.
      # Scaled a la Geert-Jan: 
      hapPol[1,expInd,ptN] <- 2*fixAcc[expInd]-1;   # this will hopefully fail if rubbish fixAcc is given.
    } else {
      accP0 <-  ((M-1):0)*(parMat[ptN,'acc0max']-parMat[ptN,'acc0min'])/(M-1) + parMat[ptN,'acc0min'];
      hapPol[1,expInd,ptN] <- 2*accP0 - 1;  # Apply same to Hresp below.
    }
    # Stuff to apply to all trials:
    gam <- as.numeric(parMat[ptN,'gam']);
    # Poss. include a normalizing constant to gam, equal to 1/sum(kernel vector), or not ...
    # z = 1; #
    if (gamNorm) {
      z = 1/sum(c(1, cumprod(rep(gam, (sumN-1)))));
    } else {
      z = 1;
    }
    # ... to be included in the weights:  
    We = z*parMat[ptN,'wexp'];           Wr = z*parMat[ptN,'wrpe'];
    # Now for acceptance prediction response function parameters:
    Tresp = 2*parMat[ptN,'Tresp'];    # this and Hresp below scaled to be directly
    Hresp = 2*parMat[ptN,'Hresp']-1;  #   comparable with the values and returns ...
    
    # loop over trials
    for (trN in 1:Ntrtot){
      hapPol[trN+1,,ptN] <- hapPol[trN,,ptN];   # Initialise-most will remain same.
      
      gpI <- datAr[trN,1,ptN];  # which group rated this pt
      
      # Prob. of 'accept' response emitted (this is BEFORE rating seen),
      # if present. NB we will use beliefs after last trial, i.e. hapPol[trN,...
      ratingP <- 1/(1+exp((-Hresp-hapPol[trN,expInd[gpI],ptN])/Tresp));
      hapPol[trN+1,accPI,ptN] <- ratingP;  # Store      
      
      # Now calc. obsP, the probability under the current model that the
      # participant emits the acceptance prediction in datAr (the experimentally observed one).
      predAcc <- datAr[trN,2,ptN]; # rating that actual participant predicted.
      if (!is.na(predAcc)){    
        if (predAcc > 0.5){  # i.e. if it's 1 
          hapPol[trN+1,obsPI,ptN] <- ratingP;
        } else {
          hapPol[trN+1,obsPI,ptN] <- 1-ratingP;
        }
      } # End if valid prediction predAcc

      # Sum happinesSE eqn
      # First find out how many timepoints to sum over. 
      sumN <- knmax; if ( trN < knmax ){ sumN <- trN; };
      
      # RPE for this trial. Note that the EV is from the end
      # of the trial before ( hapPol[trN,... ) but the outcome is from this one,
      #  datAr[trN, ... :
      nofb = datAr[trN,'nofb',ptN];  # 0 if feedback given, 1 otherwise
             # so if no feedback given the outcome is said to be 0 acc. to GJW / RR model.
      EV = hapPol[trN,expInd[datAr[trN,'gp',ptN]],ptN]; 
      hapPol[trN+1,rpeInd,ptN] <-  (1-nofb)*datAr[trN,'obs',ptN]  - EV ;
      # Now for the SE sum:
      SE <- as.numeric(parMat[ptN,'SEb']);
      j = sumN;
      while (j>0) {  # done with a while loop to skip it if sumN already 0, 
                     # which may be the case if we want to skip trials by using remainder
                     # to detect block starts for fmri version ... :-(
        SE <- SE + gam^(j-1)*
          (  We* hapPol[trN-j+1,expInd[datAr[trN-j+1,'gp',ptN]],ptN] +
               Wr* hapPol[trN-j+2,rpeInd,ptN] )
        
           j=j-1;
      }
      # Store this SE as the current expected SE:
      
      # if loop below zapped as in the first instance NA group trials excluded already ...
      # if (!is.na(datAr[trN,'gp',ptN])){  # if valid group stimulus was presented
         hapPol[trN+1,expSEI,ptN] <- SE;

         # Determine density at the actual response:
         if (!is.na( datAr[trN, 'SE',ptN] )) {
           hapPol[trN+1,SEPDI,ptN] <- dnorm( datAr[trN, 'SE',ptN], SE, parMat[ptN,'sig'] );
         } else {
           hapPol[trN+1,SEPDI,ptN] <- NA;
         }
         

         # generate pseudodatum:
         hapPol[trN+1,genSEI,ptN] <- rnorm(1, SE, parMat[ptN,'sig'] );
       
         # Don't forget to update the EV for the gp we've just been rated by:
         hapPol[trN+1,expInd[gpI],ptN] <- hapPol[trN+1,expInd[gpI],ptN] + 
                                         parMat[ptN,'eta']* hapPol[trN+1,rpeInd,ptN] ; 
         
      #} else {  # if no group stimulus was presented, fill in with NAs. Note
      #          # that the group expectations are not touched - they just stay as they are.
      #    hapPol[trN+1,c(expSEI,SEPDI,genSEI),ptN] = c(NA,NA,NA) ; 
      #}
      
    } # end of loop over trials


  } # end loop over pts

  # Create objects for output
  SLP1 <- sum(log(na.omit(as.vector(hapPol[,obsPI,]))));
  SLP2 <- sum(log(na.omit(as.vector(hapPol[,SEPDI,]))));
  
  if (onlySLP ==1){          # sLP pertaining to all observations
    return( SLP1+SLP2 );
  } else {
      if (onlySLP ==2) {  # sLP pertaining to SE only
        return(SLP2) ; 
      } else {
        SLPetc <- list();
        SLPetc[[1]] <- SLP1;
        SLPetc[[2]] <- SLP2;
  
        datAr = bakD;  Ntrtot=dim(datAr)[1]; # restore in original form.
        # Next to combine both exp. data, beliefs, policies etc. 
        # Therefore has extra gp, pred, obs, SE, nofb and genPred cols :
        colN <- dim(hapPol)[2]+6;
        DatBelPol <- array(NA,c(Ntrtot+1,colN,Nptot));
        dimnames(DatBelPol)[[2]] <- c(colnames(datAr), colnames(hapPol), 'genPred')
        for (ptN in 1:Nptot) {
          offs = 1*is.na(datAr[1,'gp',ptN]);
          DatBelPol[(2-offs):(Ntrtot+1-offs),1:5,ptN] <- datAr[,,ptN];
          stTr = stimTr[,ptN];
          if (offs == 1){ 
            stTr[1] = TRUE; stTr=c(stTr,FALSE);
          } else {
            stTr=c(TRUE, stTr);  
          }
          DatBelPol[stTr,(5+1):(5+dim(hapPol)[2]),ptN] <- hapPol[1:sum(stTr),,ptN];
          for (trN in 1:Ntrtot) {
            if (!is.na(DatBelPol[trN+1,'accP',ptN])) {
               DatBelPol[trN+1,'genPred',ptN] <- rbinom(1,1,DatBelPol[trN+1,'accP',ptN]);
            }
          }
        }
        SLPetc[[3]] <- DatBelPol;
        # 4th element to have generated data only :
        SLPetc[[4]] <- array(NA,c(dim(DatBelPol)[1], 5, dim(DatBelPol)[3]));
        SLPetc[[4]][,,] <- DatBelPol[, c('gp','genPred','obs','genSE','nofb'),] ;
        colnames(SLPetc[[4]]) <- c('gp','pred','obs','SE','nofb'); # Just like real expt. data ...
        SLPetc[[5]] <- parMat;
        colnames(SLPetc[[5]]) <- c('SEb','acc0max','acc0min','eta', 'gam','wexp','wrpe','Tresp','Hresp','sig');
        names(SLPetc) <-c('predSLnP','SESLnP','DatBelPol','genD','ptPar');

    
        return(SLPetc);
      }
  }
  
} # end of SLPsocio0 - 'Happiness-like model' (Robb's)

# -- Fig SLPsocio0 to the whole dataset. ----------------------------
fitSLPsocio0 <- function (gpAcc=c(0.85,0.7,0.3,0.15), Pts=(22:61), predRat=1, pri0=NA,
                          baseDir=NA, workDir=NA, outpFile = "socio0fit_XXXX.RData",toPlot=1){
  # This is a messy function - it reads a lot of stuff from disk which
  # must be there for it to function ... 
  # predRat = 1 means Do try to fit participatn 'prediction ratings' too, 0 is don't.
  # gpAcc <-  FixAcc or NA; give 'known' acceptance rates to fit NON-learning model, NA for learning.
  
  
  if (is.na(baseDir)){
      switch(Sys.info()[['sysname']],
         Linux  = {baseDir <- "~/Dropbox/BASOR/BASOR_output/"; },
         Windows= {baseDir <- "C:/Users/mmoutou/Dropbox/BASOR/BASOR_output/";})
  }
  if (is.na(workDir)){ workDir = "GJW_LikeMe/" ;  }
   

  load(paste(baseDir,workDir,'datArrs61.RData',sep='')); # R friendly form of 61 pt data, contains datArW03
  remove(datAr61);         # tiny bit of uncluttering.
  require(ppcor);
  
  ml0fit <- list(); 
  
  # initial condition for fit and priors all in native space:
  tryP <- array(NA,c(1,10));
  dimnames(tryP)[[2]] = c('SEb','acc0max','acc0min','eta','gam','wexp',  'wrpe', 'Tresp','Hresp', 'sig');
  tryP[1,] <-           c(0.60,  0.9,       0.1,     0.05, 0.8,  0.02,    0.03,    0.12,   0.6,    0.09);  # derived 
                        # ... mostly from medians of no-learning fit.
  tryTrP = nat2trLP0(tryP);  # 'transformed parameter vector to try', core of initial conditions
  # had: pri0 <- NA;  # keep to NA for fit without priors.
  #  pri0 <- matrix(c(5.01,5,5.01,5,11,10,11,10,11,10,11,10),nrow=2);  # a simple prior forcing +ve non-crazy values.
  ml0res <- matrix(NA,nrow=dim(datArW03)[3],ncol=(dim(tryP)[2]+6));
  dimnames(ml0res)[[2]] <- c('ptNum',colnames(tryP),'predSLP','SESLD','RMSE','rPears','prPears');
  attemptN = 5;  # Try this many initial conditions in fit.
  
  for (ptN in 1:length(Pts)){ 
    
    D <- array(NA,c(dim(datArW03)[1:2],1));  # Create & clear the working array
    D[,,1] <- datArW03[,,Pts[ptN]];
    dimnames(D)[[2]] <- c('gp','pred','obs','SE','nofb');
    dimnames(D)[[3]] <- Pts[ptN];
    
    ml0fit[[ptN]] <- list();
    mPD <- Inf;
    
    for (attempt in 1:attemptN){  
      
      if (attempt == 1){
        iniTrPar = tryTrP;   # The 'reasonable' one. 
        # was based on actual estimates of Robb and Geert-Jan's : 
        # rrp = RRpar21b[ptN,];  rrp[vecTRUE(rrp < 1e-5)]=1e-5 ; 
        #rrp[1,c(2:4,9)]=c(0.9,0.1,0.1,0.75);       # 'quite reasonable'          
        # iniTrPar <- 0.95 * nat2trLP0(rrp);     # nat2trLP0(tryP[c(1,5:9)]);
      } else {
        iniTrPar <- runif( dim(rrp)[2], 0.5,2) * tryTrP; #   * nat2trLP0(tryP[c(1,5:9)])
      }
      if (!is.na(gpAcc[1]) && (length(iniTrPar)>7)){ iniTrPar = iniTrPar[,c(1,5:10)]; }
      
      # ---------------- The fitting attempt -------------------------------
      print(paste('Happinesque fit - ptN:',Pts[ptN],';  fit attempt:', attempt, ' with initial conditions:'));
      print(tr2natLP0(iniTrPar));
      try( fitAttempt <- nlm(msLP0tr, iniTrPar, D, pri0, gpAcc, PredRat, print.level=0, iterlim=500) );
      # --------------------------------------------------------------------
      
      # If the fit didn't fail altogether, check if it were better than best so far:
      if (vecTRUE(length(fitAttempt$estimate)>1)){
        if ( vecTRUE(fitAttempt$minimum < mPD) || !(vecTRUE(length(ml0fit[[ptN]][[1]])>1)) ){
          mPD <- fitAttempt$minimum;
          ml0fit[[ptN]][[1]] <- fitAttempt;
        }
      }
    }  # End exploration of initial conditions
    
    if (!is.na(gpAcc[1])){ 
      est7p <- (tr2natLP0(ml0fit[[ptN]][[1]]$estimate)) ; 
      est10p <- c(est7p[1],NA,NA,NA,est7p[2:7]);
    } else {
      est10p <- (tr2natLP0(ml0fit[[ptN]][[1]]$estimate)) ; 
      est7p <- est10p[,c(1,5:10)] ; 
    }
    ml0fit[[ptN]][[2]] <- SLPsocio0(est10p, D, 0, gpAcc );
    
    names(ml0fit[[ptN]]) <- c('NLM','SLP');
    
    # output array storage: Store the param. vector obtained, but also some fit stats:
    ml0res[ptN,1] = Pts[ptN];    # numerical ID ...
    ml0res[ptN,2:11] <- est10p;
    ml0res[ptN,12] = ml0fit[[ptN]][[2]][[1]];   ml0res[ptN,13]= ml0fit[[ptN]][[2]][[2]]; 
    # root mean square error :
    ml0res[ptN,'RMSE'] = sqrt(sum(na.omit(ml0fit[[ptN]][[2]][[3]][,'expSE',1] - ml0fit[[ptN]][[2]][[3]][,'SE',1])^2)); 
    # Correlations with real data: 
    pc <- pcor(na.omit(ml0fit[[ptN]][[2]][[3]][,c('SE','expSE'),1] )); 
    ml0res[ptN,'rPears']=pc$est[1,2];   ml0res[ptN,'prPears']=pc$p[1,2]; 
    
    
    # Plotting : 
    if (toPlot){   v = dispNLMsocioFit(D, ml0fit[[ptN]][[1]], NA, gpAcc); }
    
    save(ml0res,ml0fit,gpAcc,Pts,predRat,pri0,
         baseDir, workDir, outpFile,
         file=paste(baseDir,workDir,outpFile,sep='')); # save as we go along ...
    
  }
  
  # summing var explained
  vexp = 0;
  for (ptN in 1:length(ml0fit)){
    vexp = vexp + (ml0res[ptN,'rPears']^2)/length(ml0fit);
  }
  print(paste('Overall variance explained:', vexp));
  
  save(ml0res,ml0fit,vexp,
       gpAcc,Pts,predRat,pri0, baseDir, workDir, outpFile,
       file=paste(baseDir,workDir,outpFile,sep='')); # save as we go along ...
  
  return(ml0res);
  
} # end of fitSLPsocio0 ---------------------------------------------

# ------------------ 'Bean Counting model 1' (Michael's) -----------------
# Given Nptot pts., each with 
# - 'external' likeability belief a0>=1, b0>=1, 
#     N0 = a0+b0-2, who is 
# - exposed to a stream of Ntr (dis)likes (0)1,
# - coming from groups 1...M, usu. M=4, of equal size
# - total capacity Nmax, 
# - choice function (1+exp((1-2<p>)/T)^-1
# ... calc. the SumLnDensity (SLP) for a stream of
#     predictions about being liked and accompanying SE.

SLPsocio1 <- function( parMat, datAr,onlySLP=0, check=1){
# parMat has rows with the parameters for each pt.
# datAr has a page for each pt, and Ntr rows., gp pred  obs SE cols.
#
# par. row must be: c('aBase','nBase', 'a0min', 'n0', 'nMax','Tresp')

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
 colnames(parMat) <- c('aBase','nBase', 'a0min', 'n0', 'nMax','Tresp');
 
 Nptot <- dim(datAr)[3];
 Ntrtot <- dim(datAr)[1];
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
   # Initial beliefs about ratings by others (groups)
   # alpha0s spaced up from a0min geometrically so that aBase makes sense :
   d <- 1 - parMat[ptN,3]/parMat[ptN,4];
   r <- exp(2/3*log((1-parMat[ptN,1]/parMat[ptN,2])/d));
   abnPol[1,aInd,ptN] <- parMat[ptN,4]*(1-d*r^(0:3)); 
       # linear, when parMat gave a0max and a0min, was
       # ((M-1):0)*(parMat[ptN,3]-parMat[ptN,4])/(M-1) + parMat[ptN,4];
   abnPol[1,bInd,ptN] <- parMat[ptN,4]-abnPol[1,aInd,ptN];    
   abnPol[1,nInd,ptN] <- parMat[ptN,4];    

   aBase <- parMat[ptN,1];
   nMax <- parMat[ptN,5];   
   nBase <- parMat[ptN,2];
   nTask <- nMax - nBase;  # This much available for task to play with.
   adecay <- (nTask-3)/(nTask-2) ;  # to be used for 'too much data to remember'

   if (check){
     if (nTask-parMat[ptN,4] < 0) {
       print(paste('At ptN',ptN,' params:')); print(parMat[ptN,]);
       stop('Please make sure Nmax-nBase-n0 >= 0');
     }
   }
   
   # make sure 0 < SE <1 :
   datAr[vecTRUE(datAr[,4,ptN] <=0),4,ptN] <- eps  ;
   datAr[vecTRUE(datAr[,4,ptN] >=1),4,ptN] <- 1-eps;
   
   # loop over trials
   for (trN in 1:Ntrtot){
     
    # debug line:
    #if (vecTRUE(datAr[trN,4,ptN] > (1-2*eps))){
    #  print(trN);
    #}
     
    abnPol[trN+1,,ptN] <- abnPol[trN,,ptN];   # Initialise-most will remain same.
    
    gpI <- datAr[trN,1,ptN];  # which group rated this pt
    if (is.na(gpI)){  # if there was no valid group i.e. no 'rater' was presented
           # just keep propagated beliefs but don't attepmt anything of substance
      abnPol[trN+1,c(SEPDI,obsPI,accPI,genSEI,expSEI),ptN] <- NA;
      
    } else { # if there was valid group i.e. valid 'rater' was presented
    # Prob. of 'accept' response emitted (this is BEFORE rating seen),
    # if present. NB we will use beliefs after last trial, i.e. abnPol[trN,...
    ratingP <- 1/(1+exp((1-2*abnPol[trN,aInd[gpI],ptN]/
                             abnPol[trN,nInd[gpI],ptN] )/parMat[ptN,'Tresp']));
    abnPol[trN+1,accPI,ptN] <- ratingP;  # Store                          
    
    predAcc <- datAr[trN,2,ptN]; # rating that actual participant predicted.
    if (!is.na(predAcc)){    
      if (predAcc > 0.5){  # i.e. if it's 1 
        abnPol[trN+1,obsPI,ptN] <- ratingP;
      } else {
        abnPol[trN+1,obsPI,ptN] <- 1-ratingP;
      }
    } # End if valid observation seen
    
    nSoFar <- abnPol[trN,nInd[gpI],ptN];
    
    if ((nSoFar+1) <= nTask){
        abnPol[trN+1,nInd[gpI],ptN] <- abnPol[trN,nInd[gpI],ptN]+1;
        # Adjust a, b according to the observatio made:
        abnPol[trN+1,aInd[gpI],ptN] <- abnPol[trN,aInd[gpI],ptN]+datAr[trN,3,ptN];
        # Just for completeness and checking:
        abnPol[trN+1,bInd[gpI],ptN] <- abnPol[trN,bInd[gpI],ptN]+1-datAr[trN,3,ptN];
      
    } else { # if nSoFar has max'ed to nTask :
        abnPol[trN+1,nInd[gpI],ptN] <- abnPol[trN,nInd[gpI],ptN];
        # a only allowed to go to nTask-1, to allow b >= 1 :
        abnPol[trN+1,aInd[gpI],ptN] <- 1+ datAr[trN,3,ptN] +
                                       adecay*(abnPol[trN,aInd[gpI],ptN]-1) ;                
        # Again, just for completeness, b:
        abnPol[trN+1,bInd[gpI],ptN] <- nTask - abnPol[trN+1,aInd[gpI],ptN];    
    
    } # end if we have or haven't maxed out the trials to calc. over
   
    
    # Consider SE as a density over the unit interval, and calc. the
    # density at the new SE reported, if valid. IT HAS TO CORRESPOND TO THE
    # WAY SE IS GENERATED (e.g. for synthetic data ...)
    # We'll express the overall SE distribution as a mixture of Beta distros.
    if (is.na(datAr[trN,4,ptN])) {
      abnPol[trN+1,SEPDI,ptN] <- NA ;
    } else if ((SEgenMeth==0) || (SEgenMeth==1)) {
      # Actually for ==0 above isn't quite consistent ...
      abnPol[trN+1,SEPDI,ptN] <- mean(  dbeta(datAr[trN,4,ptN], 
                                              abnPol[trN+1,aInd,ptN]+parMat[ptN,1], 
                                              abnPol[trN+1,bInd,ptN]+parMat[ptN,2]-parMat[ptN,1]) );
    } else if (SEgenMeth==2) {
      a <- sum(abnPol[trN+1,aInd,ptN]) + parMat[ptN,1] ;
      b <- sum(abnPol[trN+1,bInd,ptN]) + parMat[ptN,2] - parMat[ptN,1]  ;     
      abnPol[trN+1,SEPDI,ptN] <- dbeta(datAr[trN,4,ptN] , a, b)
    }
    
    
    ##                      'reported', or generated, SE 
    #  - note selection var SEgenMeth
    if (SEgenMeth==1)  {  # should generated SE be random (1) by component
                          # expectation (0) or ...
      
      # first select which component to sample from:
      comp <- sample(1:M,1);
      # The SE beta distros also include 'private data' :
      a <- abnPol[trN+1,aInd[comp],ptN] + parMat[ptN,1] ;
      b <- abnPol[trN+1,bInd[comp],ptN] + parMat[ptN,2] - parMat[ptN,1] ; 
      # Now fll in expected and sampled SE !
      abnPol[trN+1,expSEI,ptN] <- a/(a+b);  
      abnPol[trN+1,genSEI,ptN] <- rbeta(1,a,b);  
      
    } else if (SEgenMeth==0) {  # 'reported' SE in genSE col. as overall expectation:
          Nfixed <- parMat[ptN,2];      afixed <- parMat[ptN,1];
          accSum <- 0;
          for (i in 1:M){
            accSum <- accSum +
                        (abnPol[trN+1,aInd[i],ptN]+afixed)/(abnPol[trN+1,nInd[i],ptN]+Nfixed);   
          }  
          abnPol[trN+1,genSEI,ptN] <- accSum/M; 
          abnPol[trN+1,expSEI,ptN] <- accSum/M;   # identical copy
    } else if (SEgenMeth==2) {  # 'reported' SE in genSE col. from an
                                #  overall beta ... 
      # calc. a and b again, as may not have been calculated above.
      a <- sum(abnPol[trN+1,aInd,ptN]) + parMat[ptN,1] ;
      b <- sum(abnPol[trN+1,bInd,ptN]) + parMat[ptN,2] - parMat[ptN,1] ;   
      abnPol[trN+1,expSEI,ptN] <- a/(a+b); 
      abnPol[trN+1,genSEI,ptN] <- rbeta(1,a,b);
      
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
    colnames(SLPetc[[5]]) <- c('aBase', 'nBase', 'a0min', 'n0',   'Nmax', 'Tresp');
    names(SLPetc) <-c('predSLnP','SESLnP','DatBelPol','genD','ptPar');
    
    return(SLPetc);
  }
    
} # end of SLPsocio1  - 'counting / popularity model' (Michael's)

# --------------- 'Likeability scale model' (Karl's) ----------------

## SE model based on the idea that the participant's rating relies
#  on how likeable they are on a scale that applies across rater groups,
# 


SLPsocio2 <- function( parMat, datAr, onlySLP=0, check=1){
  # parMat has rows with the parameters for each pt.
  # datAr has a page for each pt, and Ntr rows.
  #
  # par. row:             gam,  v0,  pi0,  Tresp,  u1     ...    uM
  #    e.g.   parM2 <-  c(2,   0.75,  6,   0.2 ,  1.2, 1.4, 1.6, 1.8  )
    
  # play with: 
  #      Ntr <- 200; d1 <- data.frame(matrix(rep(NA,Ntr*4),Ntr,4)); colnames(d1)<- c('gp','pred','obs','SE','nofb')
  #      d1[,1] <- sample(1:4,Ntr,TRUE); d1[,2] <- (round(rbeta(Ntr,7-d1[,1],2+d1[,1]))); d1[,3] <- (round(rbeta(Ntr,7-d1[,1],2+d1[,1])))
  #      d1[1,4] <- 0.75; for( i in 2:Ntr){d1[i,4] <- d1[i-1,4]^( 0.8^((d1[i,3]-d1[i,2])) )}
  #                   
  # par. row:             gam,  v0,  pi0,  Tresp,  u1     ...    uM
  #    e.g.   parMat <-  c(2,   0.75,  6,   0.2 ,  0.2, 0.4, 0.6, 0.8 )
  
  M <- 4; # Number of rater groups.
  
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
  colnames(parMat) <- c('gam','v0','pi0','Tresp',paste('u',1:M,sep=''));
  uOffs <- length(colnames(parMat)) - M; # to help index the  u1 ... uM
  
  Nptot <- dim(datAr)[3];
  Ntrtot <- dim(datAr)[1];
  # Array to hold expectations of approval for each group of raters,
  # pi=1/(1-exp(gam*(ui-v))) , the belief params vt, pit w. P(v)=N(v;vt,pit),
  # as well as the probability / prob. dens. for the responses emitted,
  # and generated acceptance prediction and SE. So each completed row
  # refers to the values at the END OF THAT TRIAL
  #
  # Note has one more row than trials as starts from 'priors'.
  scPol <- array(dim=c(Ntrtot+1,M+5,Nptot)); 
  dimnames(scPol)[[2]] <- c(paste('exp',1:M,sep=''), 'vt', 'pit',
                             'obsP','SEPD','accP');
  expInd <- 1:M    # Indices of group expectations in scPol 
  vtInd <- M+1;
  pitInd <- vtInd+1;       obsPI <- pitInd+1 ;
  SEPDI <- obsPI +1;       accPI <- SEPDI +1;
  
  for (ptN in 1:Nptot){
    # Initial beliefs about ratings by others (groups):
    gam <- parMat[ptN,'gam'];
    scPol[1,expInd,ptN] <- 1/(1+exp(gam*(parMat[ptN,c(paste('u',1:M,sep=''))]-
                                parMat[ptN,'v0'])));               
    scPol[1,c(vtInd,pitInd),ptN] <-  parMat[ptN,c('v0','pi0')]
    
    # loop over trials
    for (trN in 1:Ntrtot){
      scPol[trN+1,,ptN] <- scPol[trN,,ptN];   # Initialise-most will remain same.
      
      gpI <- datAr[trN,1,ptN];  # which group rated this pt
      
      # Prob. of 'accept' response emitted (this is BEFORE rating seen),
      # if present. NB we will use beliefs after last trial, i.e. scPol[trN,...
      ratingP <- 1/(1+exp((1-2*scPol[trN,expInd[gpI],ptN])/parMat[ptN,'Tresp']));
      scPol[trN+1,accPI,ptN] <- ratingP;  # Store                          
      
      predAcc <- datAr[trN,2,ptN]; # rating that actual participant predicted.
      if (!is.na(predAcc)){    
        if (predAcc > 0.5){  # i.e. if it's 1 
          scPol[trN+1,obsPI,ptN] <- ratingP;
        } else {
          scPol[trN+1,obsPI,ptN] <- 1-ratingP;
        }
      } # End if valid observation seen


      # Calc. updated values for v ... :
      scPol[trN+1,vtInd,ptN] <- scPol[trN,vtInd,ptN] +
                                 gam*(datAr[trN,'obs',ptN] -
                                          scPol[trN,expInd[gpI],ptN])/
                                              scPol[trN,pitInd,ptN]    ;
      # ... and pi:
      scPol[trN+1,pitInd,ptN] <- scPol[trN,pitInd,ptN] +
          gam*gam*scPol[trN,expInd[gpI],ptN]*(1-scPol[trN,expInd[gpI],ptN]);
      
      # Determine density at the actual response:
      sd <- 1/sqrt(scPol[trN+1,pitInd,ptN])
      scPol[trN+1,SEPDI,ptN] <- dnorm( datAr[trN, 'SE',ptN]   ,
                                       scPol[trN+1,vtInd,ptN] , sd );
      
      # update expK for all groups acc. to new vt:
      scPol[trN+1,expInd,ptN] <- 1/(1+exp(gam*(
          parMat[ptN,(uOffs+1):(uOffs+M)] - scPol[trN+1,vtInd,ptN] ) ) ); 
      
    } # end of loop over trials

    
    
  } # end loop over pts
  
  # Create objects for output
  SLP1 <- sum(log(na.omit(as.vector(scPol[,obsPI,]))));
  SLP2 <- sum(log(na.omit(as.vector(scPol[,SEPDI,]))));
  
  if (onlySLP){
    return( SLP1+SLP2 );
  } else {
    SLPetc <- list();
    SLPetc[[1]] <- SLP1;
    SLPetc[[2]] <- SLP2;
    # Next to combine both exp. data, beliefs, policies etc.
    # Therefore has extra gp, pred, obs, SE and genSE, genPred cols :
    colN <- dim(scPol)[2]+6;
    DatBelPol <- array(NA,c(Ntrtot+1,colN,Nptot));
    dimnames(DatBelPol)[[2]] <- c(colnames(datAr), colnames(scPol), 'genPred','genSE')
    DatBelPol[2:(Ntrtot+1),1:5,] <- datAr;
    DatBelPol[,(5+1):(5+dim(scPol)[2]),] <- scPol;
    for (ptN in 1:Nptot) {
      for (trN in 1:Ntrtot) {
          DatBelPol[trN+1,'genPred',ptN] <- rbinom(1,1,DatBelPol[trN+1,'accP',ptN]);
          sd <- 1/sqrt(DatBelPol[trN+1,'pit',ptN]);  # bec. prec = 1/(sd*sd)
          DatBelPol[trN+1,'genSE',ptN] <- rnorm(1,DatBelPol[trN+1,'vt',ptN],sd);
      }      
    }
    SLPetc[[3]] <- DatBelPol;
    # 4th element to have generated data only :
    SLPetc[[4]] <- NA*datAr;  # shortest scripting to preserve dimentionality ...
    SLPetc[[4]][,,] <- DatBelPol[2:(Ntrtot+1), c('gp','genPred','obs','genSE','nofb'),] ;
    colnames(SLPetc[[4]]) <- c('gp','pred','obs','SE','nofb'); # Just like real expt. data ...

    SLPetc[[5]] <- parMat;
    names(SLPetc) <-c('predSLnP','SESLnP','belPol','genD','ptPar');
    
    return(SLPetc);
  }
  
} # end of SLPsocio0 - 'Scale likeability model' (Karl's)

##  Wrappers to simplify calling fitting function nlm #########################

# Wrapper for happinesque function -
# - using  msLP0tr(parM0, datAr)
# parM0 only to have atanh(2*SEb-1), log(gam,  wexp,  wrpe, Tresp,  sig)
# SLPsocio0( parMat, datAr, onlySLP=0, fixAcc=NA, check=1)
# REM               SEb, acc0max, acc0min,  eta,  gam,  wexp,  wrpe, Tresp,  sig
#      parMat <- c( 0.8,  0.8,    0.6,      0.1,  0.8,   0.3,   0.4,  0.2,   0.1)
#      if fixAcc (fixed acceptance proportions) are given, then reset eta to zero,
#      acc0min and max to NA and use fixAcc (for all ptN) in hapPol[1,expInd,ptN].
#      usu. leave fixAcc at default c(0.85,0.70,0.30,0.15)

msLP0tr <- function(trParM, datAr, gamPri=NA, fixAcc=c(0.85,0.70,0.30,0.15), predRat=0, check=1){
  # trParM only to have atanh(2*SEb-1), log(gam,  wexp,  wrpe, Tresp,  sig)
  if (check){
    if (is.null(dim(trParM))){
      trParM <- matrix(trParM,nrow=1,byrow=TRUE)
    }
  }
  parM <- matrix(NA,nrow=dim(trParM)[1],ncol=3+dim(trParM)[2]);   # long form ...
  
  # fill in long form :
  if (dim(trParM)[2] == 7){  
    parM[,c(1,5:10)]  <- tr2natLP0(trParM);  # keep NAs for acc0max, acc0min, eta
  } else {
    if (dim(trParM)[2] == 10) { 
      parM  <- tr2natLP0(trParM);
    } else {
        print(trParM);
        stop('tr2natLP0 not ready for this transformed parameter matrix, dim(trParM) ');
      }
  }


  # Cacl. the log prior for MAP purposes etc, all calc'd in short form:
  mSLPrior <- 0;
  if (length(gamPri)>1){  # legit prior must have 12 elements or so!
    for (ptN in 1:dim(trParM)[1]) {
      # in the line below na.omit bec. usually we don't have acc0max, acc0min, eta
      mSLPrior <- mSLPrior - sum(dgammaMS(na.omit(parM[ptN,]),   
                                          gamPri[1,],gamPri[2,], log=TRUE)); 
    }
  } 

  if (mSLPrior == Inf){  # If we are in an a priori prohibited parameter region
    # do not attempt to calculated the likelihood - it will be nonsense anyway.
    return(Inf); 
  } else {
    if (predRat==1) {  # i.e. if we are to consider pts predictions of ratings
       return ( mSLPrior - SLPsocio0( parM, datAr, onlySLP=1, fixAcc, check=1) );
    } else {  # this is the default, for predRat=0 : only return sLP pertaining to SE, not ratings.
      return ( mSLPrior - SLPsocio0( parM, datAr, onlySLP=2, fixAcc, check=1) );
    }
  }

  # was:  mSLP <- - SLPsocio0( parM, datAr, onlySLP=1, fixAcc, check=1)
  #  return(mSLP);
    
} # end of msLP0tr

# - - - - - - transform from native to fitting space for happinesque - - - - -
nat2trLP0 <- function(parM,check=1){
  if (check){
    if (is.null(dim(parM))){
      parM <- matrix(parM,nrow=1,byrow=TRUE)
    }
  }  
  if (dim(parM)[2] == 10){  # This means acc0max, acc0min, eta given.
      #  c('SEb','acc0max','acc0min','eta','gam','wexp','wrpe','Tresp','Hresp','sig')
      # stop('tr2natLP0 not ready for non-ready-made general group acceptance probabilities');
      trParM <- matrix(NA,nrow=dim(parM)[1],ncol=dim(parM)[2]); 
      trParM[,c(1:4,9)] <- as.matrix(atanh(2*parM[,c(1:4,9)]-1));
      trParM[,c(5:8,10)] <- as.matrix(log(parM[,c(5:8,10)])) ;
      #trParM[,9] <- as.matrix(atanh(2*parM[,9]-1));
  } else {
      if (dim(parM)[2] == 7) {
         #  c('SEb', 'gam', 'wexp', 'wrpe', 'Tresp','Hresp','sig')
         trParM <- matrix(NA,nrow=dim(parM)[1],ncol=dim(parM)[2]); 
         trParM[,c(1,6)] <- as.matrix(atanh(2*parM[,c(1,6)]-1));
         trParM[,c(2:5,7)] <- as.matrix(log(parM[,c(2:5,7)])) ;
      } else {
         print(parM);
         stop('tr2natLP0 not ready for this param. vector length provided'); 
      }
  }
  return(trParM);
}
# - - - - - - transform from fitting space to native for happinesque - - - - -
tr2natLP0 <- function(trParM,check=1){
  if (check){
    if (is.null(dim(trParM))){
      trParM <- matrix(trParM,nrow=1,byrow=TRUE)
    }
  }
  if (dim(trParM)[2] == 10 ){  # This means acc0max, acc0min, eta given.
    parM <- matrix(NA,nrow=dim(trParM)[1],ncol=dim(trParM)[2]);
    colnames(parM) <- c('SEb','acc0max', 'acc0min', 'eta', 'gam','wexp','wrpe','Tresp','Hresp','sig');
    parM[,c(1:4,9)] <- as.matrix(0.5*(tanh(trParM[,c(1:4,9)])+1));
    parM[,c(5:8,10)] <- as.matrix(exp(trParM[,c(5:8,10)])) ;
    
  } else {
    if (dim(trParM)[2] == 7) {
      parM <- matrix(NA,nrow=dim(trParM)[1],ncol=dim(trParM)[2]);
      colnames(parM) <- c('SEb','gam','wexp','wrpe','Tresp','Hresp','sig');
      parM[,c(1,6)] <- as.matrix(0.5*(tanh(trParM[,c(1,6)])+1));
      parM[,c(2:5,7)] <- as.matrix(exp(trParM[,c(2:5,7)])) ;
    } else {
      stop('tr2natLP0 not ready for this length of trParM');
    }
    
  }

  return(parM);
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Wrapper for 'counting' function -
# - using  msLP1tr(trParM1, datAr)
# trParM1 to have c(ln(nBase-2), ln(a0max-a0min), ln(a0min-1), ln(n0-a0max), ln(Nmax-n0-nBase), lnTresp)
# so it derives aBase and bBase as per  aBase/nBase = (a0max+a0min)/(2*n0)
# SLPsocio1( parMat, datAr, onlySLP=0, check=1)   # 
#           Based on 'beta beliefs'. datAr as usual,
#           parMat:  aBase, bBase, a0max, a0min, n0,   Nmax, Tresp
#             

msLP1tr <- function(trParM, datAr, gamPri=NA, check=0){
  # trParM: transf. directly by tr2natLP1  
  #   c('tr(SEb,SEmin)','tr(aB OR bB)','ln(a0min-1)',
  #                               'ln(n0-a0min-1)','tr(Nmax etc)','ln(Tresp)')
  # gamPri has the means (row1) and sd's (row2) for gamma priors on ParM IN NATIVE
  # SPACE !!!
  
  if (is.null(dim(trParM))){   # turn trParM into matrix if it's a vector
    trParM <- matrix(trParM,nrow=1,byrow=TRUE)      }
  
  if (check){
      if ((dim(datAr)[2]<4) || (dim(trParM)[2]<6)){
          stop('arguments trParM or datAr appear to have wrong dimensions');  }
  }
  
  
  parM <- matrix(NA,nrow=dim(trParM)[1],ncol=dim(trParM)[2]+1); 
  
  # fill in:  aBase  nBase    a0min   n0       nMax     Tresp
  parM <- tr2natLP1(trParM); 
  
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
    return ( mSLPrior - SLPsocio1( parM, datAr, onlySLP=1, check) );
  }
  
} # end of msLP1tr 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Wrapper for Fristonian scale function -
#        

msLP2tr <- function(trParM, datAr, p0MS=NA, check=0){
  # trParM: transf. directly by tr2natLP2
  #  trParM is as follows;  ln(gam), v0, ln(pi0), ln(Tresp), uMin, ln(du).
  # gamPri has the means (row1) and sd's (row2) for  priors on ParM IN NATIVE
  # SPACE, i.e.
  #  gam (gammaMS), v0 (norm), pi0 (gammaMS), Tresp (gammaMS), uMin (norm), du (gammaMS)
  
  if (is.null(dim(trParM))){   # turn trParM into matrix if it's a vector
    trParM <- matrix(trParM,nrow=1,byrow=TRUE)      }
  
  if (check){
      if ((dim(datAr)[2]<4) || (dim(trParM)[2]<6)){
          stop('arguments trParM or datAr appear to have wrong dimensions');  }
  }
  
  
  parM <- matrix(NA,nrow=dim(trParM)[1],ncol=dim(trParM)[2]+2); 
  
  # Transform to the short form in native space:
  parM[,1:6] <- tr2natLP2(trParM);

  # Cacl. the log prior for MAP purposes etc:
  if (length(p0MS)>1){  # legit prior must have 12 elements or so!
      mSLPrior <-  sum( - dgammaMS(parM[,1], p0MS[1,1], p0MS[2,1], log=TRUE)
                        - dnorm(   parM[,2], p0MS[1,2], p0MS[2,2], log=TRUE)
                        - dgammaMS(parM[,3], p0MS[1,3], p0MS[2,3], log=TRUE)
                        - dgammaMS(parM[,4], p0MS[1,4], p0MS[2,4], log=TRUE)
                        - dnorm(   parM[,5], p0MS[1,5], p0MS[2,5], log=TRUE)
                        - dgammaMS(parM[,6], p0MS[1,6], p0MS[2,6], log=TRUE) ) ;
      
  } else {
      mSLPrior <- 0;
  }

  # only now get the long form:
  parM[,6:8] <- parM[,5]+ parM[,6] %*% t(1:3);
  
   
  # debug line:
  #print(paste('Prior density:',mSLPrior))
  #print(cbind(parM,trParM));
  
  if (mSLPrior == Inf){  # If we are in an appriori prohibited parameter region
    # do not attempt to calculated the likelihood - it will be nonsense anyway.
    return(Inf); 
  } else {
    return ( mSLPrior - SLPsocio2( parM, datAr, onlySLP=1, check) );
  }
  
} # end of msLP2tr 


# -----------------------------------------------------------------------------

# Param transf. for SLPsocio1, using 6-element input
# as per nat2trLP1 above.
#   SLPsocio1( parMat, datAr, onlySLP=0, check=1) : Based on 'beta beliefs'
#           parMat e.g.:  aBase, bBase, a0max, a0min, n0,   Nmax, Tresp
#                           20.0,  5.0,  7.0,   4.0,  10.0,  33.0   0.2
#   In order for this to always make sense, transf. to:
#      aBase -> tanh prop. of n0 >=1 

nat2trLP1 <- function(p,check=1){  
  # p here is of form  c('aBase','nBase', 'a0min', 'n0', 'nMax','Tresp') :
  # so  trp to be of form:

  
  eps   <- exp(-25);   # So that for R 1+eps > 1
  minLn <- -1000;   # so that for R exp(minLn) == 0, exp(-minLn) == +Inf
  
  # Basic check - of argument format
  if (check > 0){ if (is.null(dim(p))){   # convert vec to mat if need be
    p <- matrix(p,nrow=1,byrow=TRUE) }   }    
  ptTot <- dim(p)[1]; 
  # Detailed check
  if (check > 1){
    for (ptN in 1:ptTot) {
      if ((p[ptN,3] - 1) < -2*eps) { 
        print(paste('ln(a0min-1)=',p[ptN,3]));
        stop('p[ptN,3] error' ); 
      }
      if ((p[ptN,4]-p[ptN,3]) < (2-2*eps)) {
        stop('p[ptN,4]-p[ptN,3] ln(n0-a0min-2) error' ); 
      }
      if ((p[ptN,5]-p[ptN,2]-p[ptN,4]) < -2*eps){
        stop('p[ptN,5]-p[ptN,2]-p[ptN,4] ln(Nmax-nBase-n0) error')
      }
    }
  }
  
  trp <- matrix(NA,nrow=ptTot,ncol=dim(p)[2]); 
   
  y <- p[,3]-1;        y[y<eps] <- eps; 
  trp[,3] <- log(y);            # ln(a0min-1)
  y <- p[,4]-p[,3]-2;  y[y<eps] <- eps; 
  trp[,4] <- log(y);            # ln(n0-a0min-2)
  # auxiliary: SEb (baseline mean SE = aBase / nBase):
  SEb <- p[,1]/p[,2];
  SEmin <- p[,3]/p[,4];
  trp[,1] <- atanh(2*(SEb-SEmin)/(1-SEmin) - 1);  # tr(SEb,SEmin) 
  # bound the under / overflows:
  trp[trp[,1] < minLn,1] <- minLn;    trp[trp[,1] > -minLn,1] <- -minLn;
  
  # Now for nBase : 
  for (ptN in 1:ptTot){
    # first calc. the argument of the ln that we aim to take:
    if (SEb[ptN] <= 0.5){ # in this case trp[ptN,2] contains traBase
      z <- p[ptN,1]-1;
    } else {  # in this case trp[ptN,2] contains trbBase
      z <- p[ptN,1]*(1/SEb[ptN]-1) - 1; 
    } 
    if (z > 0){ 
      trp[ptN,2] <- log(z);
    } else {
      trp[ptN,2] <- minLn; 
    }
  }
   
  y <- p[,5]-p[,2]-p[,4];   y[y<eps] <- eps; 
  trp[,5] <- log(y); # ln(Nmax-nBase-n0-eps)
  
  #Tresp -> store ln Tresp
  trp[,6] <- log(p[,6]);
  if (check){ colnames(trp)<- c('tr(SEb,SEmin)','tr(aB OR bB)','ln(a0min-1)','ln(n0-a0min-1)','tr(Nmax etc)','ln(Tresp)')}
  return(trp);

    
}  # end of nat2trLP1 


# -----------------------------------------------------------------------------

nat2trLP1b <- function(parM,check=1){   # OLD VERSION - POOR PARAMETRIZATION ...
# parM here is of form  c(nBase, a0max, a0min, n0, nMax, Tresp) :
# so  trParM to be of form:
#   ln(nBase-2), ln(a0max-a0min), ln(a0min-1), ln(n0-a0max), ln(Nmax-n0-nBase-eps), lnTresp
    
  eps <- 1e-10;
  
  if (check){ if (is.null(dim(parM))){   # convert vec to mat if need be
      parM <- matrix(parM,nrow=1,byrow=TRUE) }   }  
  trParM <- matrix(NA,nrow=dim(parM)[1],ncol=dim(parM)[2]); 

  # nBase should be >= 2 :
  trParM[,1] <- log(parM[,1] - 2);
  # in place of a0min ,  store ln(a0min-1)                                
  trParM[,3] <- log(parM[,3] - 1);
  # a0max ->  store ln(a0max-a0min) :
  trParM[,2] <- log(parM[,2]- parM[,3]);
  # n0 ->  store ln(n0-a0max)
  trParM[,4] <- log(parM[,4]- parM[,2]);
  # Nmax -> ln(Nmax-n0-nBase)
  trParM[,5] <- log(parM[,5]-parM[,4]-parM[,1] - eps);
    
  #Tresp -> store ln Tresp
  trParM[,6] <- log(parM[,6]);
  
  return(trParM); 
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

tr2natLP1 <- function(trp,check=1){
#  trp is as follows; note only 6 elements:
#   c('tr(SEb,SEmin)','tr(aB OR bB)','ln(a0min-1)',
#                               'ln(n0-a0min-1)','tr(Nmax etc)','ln(Tresp)')
#  Returns: c('aBase','nBase', 'a0min', 'n0', 'nMax','Tresp')
  
  eps <- 1e-10;  #  eps is a tiny constant guaranteeing Nmax > n0+nBase as is needed.
  
  if (check){ if (is.null(dim(trp))){   # convert vec to mat if need be
      trp <- matrix(trp,nrow=1,byrow=TRUE) }   }
  ptTot <- dim(trp)[1]; 
  p <- matrix(NA,nrow=ptTot,ncol=dim(trp)[2]); 

  p[,3] <- exp(trp[,3]) + 1;      # a0min
  p[,4] <- p[,3]+exp(trp[,4])+2;  # n0 : restrict to above 3 bec. want
  # nMax - nBase = Ntask > 3 so the delta learning rule works as per
  # aDecay = (Ntask-3)/(Ntask-2) > 0 . 
  # Intermediaries:
  SEmin <- p[,3]/p[,4];           # the min. mean SE
  SEb <- SEmin + (1-SEmin)*0.5*(tanh(trp[,1])+1) ; # the baseline mean SE
  # debug print(p);   print(trp)
  # interpret trp[,2] differently depending on SEb ...
  # this may be vectorized as per bigSEb <- (SEb > 0.5) etc ...
  for (ptN in 1:ptTot){
      if (SEb[ptN] > 0.5) {  # i.e. bBase < aBase
          bBase <- exp(trp[ptN,2]) + 1;
          p[ptN,1] <- SEb[ptN]* bBase/ (1-SEb[ptN]);  # aBase
      } else {               # i.e. aBase <= bBase
          p[ptN,1] <- exp(trp[ptN,2]) + 1;
      }
  }
  p[,2] <- p[,1]/SEb;                          # nBase  = aBase/SEbase
  p[,5] <- exp(trp[,5]) + p[,2] + p[,4] + eps; # nMax > nBase+n0
  p[,6] <- exp(trp[,6]); # Tresp
  if (check){ colnames(p) <- c('aBase','nBase', 'a0min', 'n0', 'nMax','Tresp') };
  return(p); 
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

tr2natLP1z <- function(trParM,check=1){  # OLDEST VERSION - POOR PARAMETRIZATION ...
#  trParM is as follows; note only 6 elements:
#   ln(nBase-2), ln(a0max-a0min), ln(a0min-1), ln(n0-a0max), ...
#    ...  ln(Nmax-n0-nBase - eps),     lnTresp
#  eps is a tiny constant guaranteeing Nmax > n0+nBase as is needed.
#  returns         nBase>=2,    a0max>=0, a0min>=0, n0>=2,Nmax>=2, Tresp>0
#  e.g.:  parM1 <- c(15.0+5.0,      4.0,      3.0,      5.0,  30.0,     0.2)
#  because  aBase/nBase = (a0max+a0min)/(2*n0)
  
  eps <- 1e-10;  #  eps is a tiny constant guaranteeing Nmax > n0+nBase as is needed.
  
  if (check){ if (is.null(dim(trParM))){   # convert vec to mat if need be
      trParM <- matrix(trParM,nrow=1,byrow=TRUE) }   }  
  parM <- matrix(NA,nrow=dim(trParM)[1],ncol=dim(trParM)[2]); 

  # a's and b' should be >= 1, so:
  nBase <- exp(trParM[,1])+2;
  # a0min should be >= 1: 
  parM[,3] <- exp(trParM[,3])+1;
  # a0max should be > a0min:
  parM[,2] <- parM[,3] + exp(trParM[,2]);
  # n0 similarly > a0max:
  parM[,4] <- parM[,2] + exp(trParM[,4]);
  # Nmax should be >= nBase + n0 (and >= 4; in SLPsocio1 it INCLUDES n0) :
  parM[,5] <- nBase+parM[,4]+ exp(trParM[,5]) + eps;
  # Tresp is just > 0:
  parM[,6] <- exp(trParM[,6]);
  # finally, record nBase:
  parM[,1] <- nBase;
  
  return(parM); 
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

nat2trLP2 <- function(p,check=1){  
  # p here is of form  c('gam','v0','pi0','Tresp', uMin, du
  # so  trp to be of form:
  # ln(gam), v0, ln(pi0), ln(Tresp), uMin, ln(du).
  # The SLPsocio2 will need to be fed M (usu. 4) u values, usu. uMin + (1...M)*du

  eps   <- exp(-25);   # So that for R 1+eps > 1
  minLn <- -1000;   # so that for R exp(minLn) == 0, exp(-minLn) == +Inf
  
  # Basic check - of argument format
  if (check > 0){ if (is.null(dim(p))){   # convert vec to mat if need be
    p <- matrix(p,nrow=1,byrow=TRUE) }   }    
  ptTot <- dim(p)[1]; 
  # Detailed check
  if (check > 1){
    for (ptN in 1:ptTot) {
      if (p[ptN,1] < -2*eps) { 
        print(paste('gam=',p[ptN,1]));
        stop('p[ptN,1] error' ); 
      }
      if (p[ptN,3] < -2*eps) {
        stop('p[ptN,3] pi0 error' ); 
      }
      if (p[ptN,4] < -2*eps){
        stop('p[ptN,4] Tresp error')
      }
      if (p[ptN,6] < -2*eps){
        stop('p[ptN,6] du error')
      }
    }
  }
  
  trp <- matrix(NA,nrow=ptTot,ncol=dim(p)[2]); 
   
  y <- p[,1];    y[y<eps] <- eps;   trp[,1] <- log(y);  # ln(gam)
  y <- p[,3];    y[y<eps] <- eps;   trp[,3] <- log(y);  # ln(pi0)
  y <- p[,4];    y[y<eps] <- eps;   trp[,4] <- log(y);  # ln(Tresp)
  y <- p[,6];    y[y<eps] <- eps;   trp[,6] <- log(y);  # ln(Tresp)
  trp[,c(2,5)] <- p[,c(2,5)];       # v0 and uMin.

  return(trp);
    
} # end nat2trLP2 


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

tr2natLP2 <- function(trp,check=1){
#  trp is as follows;  ln(gam), v0, ln(pi0), ln(Tresp), uMin, ln(du).
    
#  Returns: gam v0 pi0 uMin du
  
  eps <- 1e-10;  #  eps is a tiny constant to prevent other bits of the
      # whole thing attempting ln(0)
  
  if (check > 0){ if (is.null(dim(trp))){   # convert vec to mat if need be
      trp <- matrix(trp,nrow=1,byrow=TRUE) }   }
  ptTot <- dim(trp)[1]; 
  p <- matrix(NA,nrow=ptTot,ncol=dim(trp)[2]); 

  p[,c(1,3,4,6)] <- exp(trp[,c(1,3,4,6)]) ; 
  p[p[,1] < eps,1]  <- eps;
  p[p[,3] < eps,3]  <- eps;
  p[p[,4] < eps,4]  <- eps;
  p[p[,6] < eps,6]  <- eps;
  p[,c(2,5)] <- trp[,c(2,5)];

  return(p);
           
} # end tr2natLP2

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# ------------------ 'Bean Counting model B' (Michael's) -----------------
# Given Nptot pts., each with 
# - Likeability response fn. w. se sensitivity a0>=0, and se shift b0>=0, 
#   i.e. SE = 1/(1+((1-x)/(x*b))^a)
# - exposed to a stream of Ntr (dis)likes (0)1,
# - coming from groups 1...M, usu. M=4, of equal size
# - total capacity Nmax, 
# - choice function (1+exp((1-2(<p> + c))/T)^-1 including
#   an self-flattering Bias c ...
# ... calc. the SumLnDensity (SLP) for a stream of
#     predictions about being liked and accompanying SE.


# auxiliary fns to cater for different use of rating VAS rating scale
# i.e. different maps from SE to pAcceptance, based on the unit sigmoid.
# p here is the acceptance probability, output is the SE, 
# a is sensitivity, b is shift / offset (but see alternative parametrization below)
accP2SE <- function(p, a, b){
   return( 1/(1+((1-p)/(p*b))^a) );
} # end accP2SE
# the inverse of pacc2se
SE2accP <- function(s, a, b) {
  return( 1/(1+((1/s-1)^(1/a))*b )  );
} # end SE2accP 
# slope of acceptance wrt SE, needed for remapping the density
# as per P(se) = P(accP(se)) * daccP/dse
# Optionally, if already derived, provide corresp. accP (from SE2accP)
slopeSE2accP <- function(s, a, b, accP=NA) {
  if (is.na(sum(accP))){ accP = SE2accP(s, a, b); }  
  dAdS = (b/a)*((accP/s)^2)*((1/s-1)^(1/a-1));
  return(dAdS);
} # end slopeSE2accP

# Map of acceptance probability to SE, aka response function, 
# parametrization in terms of the SE for which SE=p, termed se0,
# and the slope of the map a at that point: 
pacc2se <- function(p, se0, a){
  if (a==0){ return( rep(se0,length(p)));  } # limit of zero slope 
  else { return( 1/(1+((1/se0-1)^(1-a))*(1/p-1)^a) ); }
} # end accP2SE
se2pacc <- function(s, se0, a) {
  return( 1/(1+( (se0/(1-se0))^(1-a)*(1/s-1))^(1/a)) );
} # end SE2accP 
# slope of acceptance wrt SE, needed for remapping the density
# as per P(se) = P(accP(se)) * daccP/dse
# Optionally, if already derived, provide corresp. accP (from se2pacc)
# Here parametrised in terms of se0 defined above:
slopese2pacc <- function(s, se0, a, accP=NA) {
  b = (se0/(1-se0))^(1/a-1);
  if (is.na(sum(accP))){ accP = SE2accP(s, a, b); }  
  dAdS = (b/a)*((accP/s)^2)*((1/s-1)^(1/a-1));
  return(dAdS);
} # end slopeSE2accP

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Some stuff for display : 
dispNLMsocioFit <- function(Dat, Fit=NA, natPar=NA, gpAcc=NA){
# plot and also display key stats resulting from an nlm fit 
# or on the basis of a give native space param vector natPar.
# of the form Fit <- nlm(msLP0tr, p0tr, D, NA, gpAcc,PredRat, print.level=0, iterlim=500)
# EITHER provide a native space param vector natPar OR a fit as above.
  
  require(ppcor);
  if ((!is.na(Fit)) && (!is.na(natPar))){
    stop('Please provide EITHER Fit OR natural param vector');
  } else {
    if (is.na(natPar[1])){
      if (model == '1lr'){
        npar <- tr2natLP0(Fit$estimate);    # here, already in long form.
      } else if (model == '2lr'){
        npar <- tr2natLP2lr(Fit$estimate);  # here, already in long form.
      } else if (model == 'CA'){
        npar <- tr2natLPCA(Fit$estimate);  # here, already in long form.
      } else {
        stop('Please specify which model is being used (1lr or 2lr or CA)')
      }
      if (length(Fit$estimate)<9){
        npar = c(npar[1],NA,NA,NA,npar[2:7]); 
        if (is.na(gpAcc[1])){
          stop('If the fit was without learning, we need the fixed/known group acceptances provided ...')
        }
      }      
    } else { npar <- natPar; }
  }
  if (is.na(natPar[1])){ preStr = 'Fitted ';} else {preStr = 'Case ';};  
  
  if (model == '1lr'){
  d <- SLPsocio0(npar,Dat,0,gpAcc);       # original model with one learning rate
  } else if (model == '2lr') {
  d <- SLPsocio2lr(npar,Dat,0,gpAcc);     # model including etapos/etaneg (comment one line or the other)
  } else if (model == 'CA')  {
  d <- SLPsocioCA(npar,Dat,0,gpAcc);      # competence-acceptance model
  } else {
    stop('Please specify which model is being used (1lr or 2lr or CA)');
  }
  
  cr = pcor(na.omit(d[[3]][,c('SE','expSE'),1])); 
  crstr = paste('   correl:  r=',round(cr$est[2,1],2), ',  p=', round(cr$p[2,1],4),sep='');
  iSE = !(is.na(d[[3]][,'SE',1]));
  plot(d[[3]][iSE,'expSE',1],t='l',lwd=2,ylim=c(0,1.25),main=paste(preStr,dimnames(Dat)[[3]],crstr),col='red3');  
  #lines(socio17[[3]][,'genSE',1],t='l',col='pink3');
  lines(d[[3]][iSE,'expSE',1]+as.numeric(1.96*d$ptPar[1,'sig']),t='l',col='pink3');
  lines(d[[3]][iSE,'expSE',1]-as.numeric(1.96*d$ptPar[1,'sig']),t='l',col='pink3');
  lines(d[[3]][iSE,'exp1',1]/2+0.5,t='l',col='purple'); 
  lines(d[[3]][iSE,'exp2',1]/2+0.5,t='l',col='blue'); 
  lines(d[[3]][,'exp3',1]/2+0.5,t='l',col='red'); 
  lines(d[[3]][,'exp4',1]/2+0.5,t='l',col='orange2');
  lines(d[[3]][iSE,'SE',1],t='l',col='blue4',lwd=2);
 
  # Now for command line output : 
  if (is.na(natPar[1])){ preStr = 'Fitted ';} else {preStr = '       ';};  
  mse = sum(na.omit(d[[3]][,'expSE',1] - d[[3]][,'SE',1])^2); 
  cat(preStr,'param:\n');
  print(d$ptPar,4);
  cat(preStr,'pt  MSE:\n',paste('        ',mse));
  cat('\n',preStr,'pt:   predSLnP                 SESLnP\n',paste('             ',c(d[[1]],d[[2]])),'\n\n');
  
  return(mse); 
  
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Wrapper for happinesque function - revised by Geert-Jan to omit the hresp parameter
# - using  msLP_noHresp(parM0, datAr)
# parM0 only to have atanh(2*SEb-1), log(gam,  wexp,  wrpe, Tresp,  sig)
# SLPsocio0( parMat, datAr, onlySLP=0, fixAcc=NA, check=1)
# REM               SEb, acc0max, acc0min,  eta,  gam,  wexp,  wrpe, Tresp,  sig
#      parMat <- c( 0.8,  0.8,    0.6,      0.1,  0.8,   0.3,   0.4,  0.2,   0.1)
#      if fixAcc (fixed acceptance proportions) are given, then reset eta to zero,
#      acc0min and max to NA and use fixAcc (for all ptN) in hapPol[1,expInd,ptN].
#      usu. leave fixAcc at default c(0.85,0.70,0.30,0.15)

msLP_noHresp <- function(trParM, datAr, gamPri=NA, fixAcc=c(0.85,0.70,0.30,0.15), predRat=0, check=1){
  # trParM only to have atanh(2*SEb-1), log(gam,  wexp,  wrpe, Tresp,  sig)
  # GJW: I think trParM is starting parameters
  if (check){
    if (is.null(dim(trParM))){
      trParM <- matrix(trParM,nrow=1,byrow=TRUE)
    }
  }
  parM <- matrix(NA,nrow=dim(trParM)[1],ncol=3+dim(trParM)[2]);   # long form ...
  
  # fill in long form :
  if (dim(trParM)[2] == 10){  
    parM  <- tr2natLP0(trParM);  # keep NAs for acc0max, acc0min, eta
    parM[9]  = 0;
    
  } else {
    if (dim(trParM)[2] == 9) { # #I think you suggested doing this, but I would like to know how it differs from the option above (the option above prevents all sorts of trouble with transforming back and forth)
      trParM[c(1:8,10)] <- trParM
      trParM[9] = 0;
      parM  <- tr2natLP0(trParM);
      
    } else {
      print(trParM);
      stop('tr2natLP0 not ready for this transformed parameter matrix, dim(trParM) ');
    }
  }
  
  
  # Cacl. the log prior for MAP purposes etc, all calc'd in short form:
  mSLPrior <- 0;
  if (length(gamPri)>1){  # legit prior must have 12 elements or so!
    for (ptN in 1:dim(trParM)[1]) {
      # in the line below na.omit bec. usually we don't have acc0max, acc0min, eta
      mSLPrior <- mSLPrior - sum(dgammaMS(na.omit(parM[ptN,]),   
                                          gamPri[1,],gamPri[2,], log=TRUE)); 
    }
  } 
  
  if (mSLPrior == Inf){  # If we are in an a priori prohibited parameter region
    # do not attempt to calculated the likelihood - it will be nonsense anyway.
    return(Inf); 
  } else {
    if (predRat==1) {  # i.e. if we are to consider pts predictions of ratings
      return ( mSLPrior - SLPsocio0( parM, datAr, onlySLP=1, fixAcc, check=1) );
    } else {  # this is the default, for predRat=0 : only return sLP pertaining to SE, not ratings.
      return ( mSLPrior - SLPsocio0( parM, datAr, onlySLP=2, fixAcc, check=1) );
    }
  }
  
  # was:  mSLP <- - SLPsocio0( parM, datAr, onlySLP=1, fixAcc, check=1)
  #  return(mSLP);
  
} # end of msLP_noHresp

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Wrapper for happinesque function - revised by Geert-Jan to omit the hresp parameter
# - using  msLP_no_wexp(parM0, datAr)
# parM0 only to have atanh(2*SEb-1), log(gam,  wexp,  wrpe, Tresp,  sig)
# SLPsocio0( parMat, datAr, onlySLP=0, fixAcc=NA, check=1)
# REM               SEb, acc0max, acc0min,  eta,  gam,  wexp,  wrpe, Tresp,  sig
#      parMat <- c( 0.8,  0.8,    0.6,      0.1,  0.8,   0.3,   0.4,  0.2,   0.1)
#      if fixAcc (fixed acceptance proportions) are given, then reset eta to zero,
#      acc0min and max to NA and use fixAcc (for all ptN) in hapPol[1,expInd,ptN].
#      usu. leave fixAcc at default c(0.85,0.70,0.30,0.15)

msLP_no_wexp <- function(trParM, datAr, gamPri=NA, fixAcc=c(0.85,0.70,0.30,0.15), predRat=0, check=1){
  # trParM only to have atanh(2*SEb-1), log(gam,  wexp,  wrpe, Tresp,  sig)
  if (check){
    if (is.null(dim(trParM))){
      trParM <- matrix(trParM,nrow=1,byrow=TRUE)
    }
  }
  parM <- matrix(NA,nrow=dim(trParM)[1],ncol=3+dim(trParM)[2]);   # long form ...
  
  # fill in long form :
  if (dim(trParM)[2] == 9) { #I think you suggested doing this, but I would like to know how it differs from the option above (the option above prevents all sorts of trouble with transforming back and forth)
    trParM[c(1:5,7:10)] <- trParM
    trParM[6] = -11.51 # Transform of 1e-5; 
    parM  <- tr2natLP0(trParM);
    
  } else {
    print(trParM);
    stop('tr2natLP0 not ready for this transformed parameter matrix, dim(trParM) ');
  }
  
  
  # Cacl. the log prior for MAP purposes etc, all calc'd in short form:
  mSLPrior <- 0;
  if (length(gamPri)>1){  # legit prior must have 12 elements or so!
    for (ptN in 1:dim(trParM)[1]) {
      # in the line below na.omit bec. usually we don't have acc0max, acc0min, eta
      mSLPrior <- mSLPrior - sum(dgammaMS(na.omit(parM[ptN,]),   
                                          gamPri[1,],gamPri[2,], log=TRUE)); 
    }
  } 
  
  if (mSLPrior == Inf){  # If we are in an a priori prohibited parameter region
    # do not attempt to calculated the likelihood - it will be nonsense anyway.
    return(Inf); 
  } else {
    if (predRat==1) {  # i.e. if we are to consider pts predictions of ratings
      return ( mSLPrior - SLPsocio0( parM, datAr, onlySLP=1, fixAcc, check=1) );
    } else {  # this is the default, for predRat=0 : only return sLP pertaining to SE, not ratings.
      return ( mSLPrior - SLPsocio0( parM, datAr, onlySLP=2, fixAcc, check=1) );
    }
  }
  
  # was:  mSLP <- - SLPsocio0( parM, datAr, onlySLP=1, fixAcc, check=1)
  #  return(mSLP);
  
} # end of msLP_no_wexp

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Wrapper for happinesque function - revised by Geert-Jan to omit the hresp parameter
# - using  msLP_sfb(parM0, datAr)
# parM0 only to have atanh(2*SEb-1), log(gam,  wexp,  wrpe, Tresp,  sig)
# SLPsocio0( parMat, datAr, onlySLP=0, fixAcc=NA, check=1)
# REM               SEb, acc0max, acc0min,  eta,  gam,  wexp,  wrpe, Tresp,  sig
#      parMat <- c( 0.8,  0.8,    0.6,      0.1,  0.8,   0.3,   0.4,  0.2,   0.1)
#      if fixAcc (fixed acceptance proportions) are given, then reset eta to zero,
#      acc0min and max to NA and use fixAcc (for all ptN) in hapPol[1,expInd,ptN].
#      usu. leave fixAcc at default c(0.85,0.70,0.30,0.15)

msLP_sfb <- function(trParM, datAr, gamPri=NA, fixAcc=c(0.85,0.70,0.30,0.15), predRat=0, check=1){
  # trParM only to have atanh(2*SEb-1), log(gam,  wexp,  wrpe, Tresp,  sig)
  if (check){
    if (is.null(dim(trParM))){
      trParM <- matrix(trParM,nrow=1,byrow=TRUE)
    }
  }
  parM <- matrix(NA,nrow=dim(trParM)[1],ncol=4+dim(trParM)[2]);   # long form ...
  
  # fill in long form :
  if (dim(trParM)[2] == 7){  
    parM[,c(1,5:10)]  <- tr2natLP0(trParM);
    
  } else {
    if (dim(trParM)[2] == 6) { 
      trParM[c(1:2,4:7)] <- trParM
      trParM[3] = -11.51 # Transform of 1e-10; 
      parM[,c(1,5:10)]  <- tr2natLP0(trParM);
      
    } else {
      print(trParM);
      stop('tr2natLP0 not ready for this transformed parameter matrix, dim(trParM) ');
    }
  }
  
  
  # Cacl. the log prior for MAP purposes etc, all calc'd in short form:
  mSLPrior <- 0;
  if (length(gamPri)>1){  # legit prior must have 12 elements or so!
    for (ptN in 1:dim(trParM)[1]) {
      # in the line below na.omit bec. usually we don't have acc0max, acc0min, eta
      mSLPrior <- mSLPrior - sum(dgammaMS(na.omit(parM[ptN,]),   
                                          gamPri[1,],gamPri[2,], log=TRUE)); 
    }
  } 
  
  if (mSLPrior == Inf){  # If we are in an a priori prohibited parameter region
    # do not attempt to calculated the likelihood - it will be nonsense anyway.
    return(Inf); 
  } else {
    if (predRat==1) {  # i.e. if we are to consider pts predictions of ratings
      return ( mSLPrior - SLPsocio0( parM, datAr, onlySLP=1, fixAcc, check=1) );
    } else {  # this is the default, for predRat=0 : only return sLP pertaining to SE, not ratings.
      return ( mSLPrior - SLPsocio0( parM, datAr, onlySLP=2, fixAcc, check=1) );
    }
  }
  
  # was:  mSLP <- - SLPsocio0( parM, datAr, onlySLP=1, fixAcc, check=1)
  #  return(mSLP);
  
} # end of msLP_sfb

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#                                         end of file




