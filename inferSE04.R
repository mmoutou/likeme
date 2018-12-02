# Key functions for inferring self-esteem by evidence accummulation and mapping
# beliefs to responses using a sigmoid function
# New model - as of discussion on 3rd September 2018
#
# Michael Moutoussis & Alexis (An Yee) Low, 2018

# ---- First find out from where this source code and so set paths -----
# Set the base and code directories consistently for different
# users of the main code, LikeMe.R etc.
sewd <- getwd()
# find out where we are
# Depending on what the path contains, decide who is the user. Student PLS EDIT YOUR ENTRY:
if (grepl('Alexis', sewd)) {
  whoami <- 'Student'
}
if (grepl('/home/hopper', sewd)) {
  whoami <- 'WillLinux'
}
if (grepl('C:/Users/mmoutou', sewd)) {
  whoami <- 'SpectreMM'
}
if (grepl('michael', sewd)) {
  whoami <- 'LinuxMM'
}
if (grepl('geert-jan', sewd)) {
  whoami <- 'GeertJanMac'
}
if (grepl('C_mmoutou', sewd)) {
  whoami <- 'SpectreVM'
}

# Adjust the base directdory accordingly.  Student PLS EDIT YOUR ENTRY :
switch(
  whoami,
  Student  = {
    baseDir <-
      "X:/OneDrive - University College London/Summer Project - Alexis An Yee Low/"
    
  },
  WillLinux = {
    baseDir <- "/home/hopper/Dropbox/SelfEvalMEV"
  },
  SpectreMM = {
    baseDir <-
      "C:/Users/mmoutou/OneDrive - University College London/SharePoint/Low, An Yee/Summer Project - Alexis An Yee Low/"
  },
  SpectreVM = {
    baseDir <-
      "/media/michael/C_mmoutou/OneDrive/SharePoint/Low, An Yee/Summer Project - Alexis An Yee Low/"
  },
  LinuxMM = {
    baseDir <- "/home/michael/gitwork/LikeMe/"
  },
  GeertJanMac = {
    baseDir <- "/Users/geert-janwill/Dropbox/GJW_LikeMe/"
    
  }
)

codeDir <- paste(baseDir, "likeme-Socio3/", sep = '')
# --------------------------------------------------------------------------

# LikeMe.R has most of the functions that for this project :
source(paste(codeDir, 'LikeMe.R', sep = ''))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SLPsocio4 <- function(parMat,
                      datAr,
                      onlySLP = 0,
                      check = 1) {
  # parMat has rows with the parameters for each pt.
  # datAr has a page for each pt, and Ntr rows., gp pred  obs SE cols.
  #
  # par. row must be: c('n0', 'a0min', 'a0max', 'Tpred', 'Bpred ',
  # 'decayCoeffGroups', 'decayCoeffSelf', 'weightSelf', 'sensi','sesh')
  # e.g.   parMat =   c(6,     1,     4,     0.2,      0.1,    0.8,     0.8,    5,    1,    1.5)
  #        sensitivity of pAcc->SE, threshold of pAcc->SE etc.
  
  M <- 4
  # Number of rater groups.
  #SEgenMeth <-  2
  # should generated SE be random (1) from component (group) distro,
  # expectation (0), overall beta (2) ... #A: combining all 4 groups into 1 beta distribution.
  eps <- 1e-10
  # a very small number to catch ln(0) etc ...
  
  if (check) {
    datDim <- dim(datAr)
    
    if (length(datDim) < 3) {
      # if 2D object was provided, convert to 3D. #A: matrix to array I think
      hd <- colnames(datAr)
      
      datAr <- array(as.matrix(datAr), c(datDim, 1))
      
      colnames(datAr) <-
        hd
      #A: hd is header -this is in order to keep the same header after turned into matrix
    }
    if (is.null(dim(parMat))) {
      parMat <- array(parMat, c(1, length(parMat)))
      
    }
  }
  
  colnames(parMat) <-
    c(
      'n0',
      'a0min',
      'a0max',
      'Tpred',
      'Bpred',
      'decayCoeffGroups',
      'decayCoeffSelf',
      'weightSelf',
      'sensi',
      'sesh'
    )
  
  Nptot <-
    dim(datAr)[3]
  #A: dim(datAr) is in the form of rows, columns, pages.
  Ntrtot <-
    dim(datAr)[1]
  # this will sadly have to be fiddled with later, usually to
  # Cater somehow for the fact that scanned task contains interruptions.
  bakD <- datAr
  # backup copy to restore later ...
  stimTr <-
    array(NA, c(Ntrtot, Nptot))
  # will store positions of stimulus-endowed trials. #A: empty array with #trials rows #pts columns
  
  # Array to hold alpha, beta, n-so-far for each group of raters,
  # as well as the probability / prob. dens. for the responses emitted,
  # and generated acceptance prediction and SE. expSE is the 'the SE' point estimate.
  # Note has one more row than trials as starts from 'priors'.
  abnPol <- array(dim=c(Ntrtot+1,3*M+8,Nptot));
  
  dimnames(abnPol)[[2]] <-
    c(paste(c('a', 'b', 'n'), repAdjVec(1:M, 3), sep = ''),'obsP','SEPD','predAccP','genSE','expSE','SEa','SEb','SEn')
  aInd <- (0:(M - 1)) * 3 + 1
  # Indices of a in abnG
  bInd <- aInd + 1
  
  nInd <- bInd + 1
  
  obsPI <- 3 * M + 1
  SEPDI  <- obsPI + 1
  
  predAccPI <- SEPDI + 1
  genSEI <- predAccPI + 1
  expSEI <- genSEI + 1
  SEaI <- expSEI + 1
  SEbI <- SEaI + 1
  SEnI <- SEbI + 1
  
  
  for (ptN in 1:Nptot) {
    # Cater somehow for the fact that scanned task contains interruptions
    # including at the very start. Awkward - to start with, just exclude:
    stimTr[, ptN] <-
      !is.na(datAr[, 'gp', ptN])
    #A: if there is a group indicated, a trial happened, so put 'true'. result - column of TRUEs
    Ntrtot <-
      sum(stimTr[, ptN])
    # update to exclude non-stimulus trials. Was dim(datAr)[1] above. #A: find the actual # of trials that happened
    datAr[1:Ntrtot, , ptN] <-
      datAr[stimTr[, ptN], , ptN]
    #A: use the data from stimTR[,ptN] (true, false) to select what to continue to include in datAr
    if (Ntrtot < dim(datAr)[1]) {
      #A: first thing ins dim(datAr) is the number of rows. remember #rows ia a dimension. so this means if number is lower after exclusion
      datAr[(Ntrtot + 1):dim(datAr)[1], , ptN] <-
        NA
      #A: then assign NA to rest of the rows.
    }
    
    # Initial beliefs about ratings by others (groups)
    abnPol[1, aInd, ptN] <-
      ((M - 1):0) * (parMat[ptN, 3] - parMat[ptN, 2]) / (M-1) + parMat[ptN, 2]
    
    abnPol[1, nInd, ptN] <- parMat[ptN, 1]
    
    abnPol[1, bInd, ptN] <-
      abnPol[1, nInd, ptN] - abnPol[1, aInd, ptN]
    
    
    # parameters for mapping acceptance py to SE (don't use lower case a, b !)
    A <- parMat[ptN, 'sensi']
    B <- parMat[ptN, 'sesh']
    
    #more parameters
    decayCoeffGroups <- parMat[ptN, 'decayCoeffGroups']
    decayCoeffSelf <- parMat[ptN, 'decayCoeffSelf']
    weightSelf <-  parMat[ptN, 'weightSelf']
    
    #initialising SE abn
    abnPol[1, SEaI, ptN] <- mean(abnPol[1, aInd, ptN])
    abnPol[1, SEbI, ptN] <- mean(abnPol[1, bInd, ptN])
    abnPol[1, SEnI, ptN] <- mean(abnPol[1, nInd, ptN])
    
    PE = 0
    
    # The shift or offset sesh has to be consistent with baseline SE and other beliefs.
    # so that if expectations above the approval rates etc. turned out to be true,
    # then self-evaluation would, remain stable. So, if the above were true, the
    # average acceptance rate would be:
    #nBal <- parMat[ptN,'nBal'];
    #aBal <- parMat[ptN,'accP0']*nBal;    bBal <- nBal - aBal;
    n0   <- parMat[ptN,1]
    accP <- (parMat[ptN,2]+parMat[ptN,3])/2/(n0)
                                        
    #A: this is the actual experimental initial acceptance probability in pt's head generated from all these parms!!!
    # And this would correspond to an 'equilibrium SE' of:
    abnPol[1, 'expSE', ptN] <-
      accP2SE(accP, A, B)
    #A: using a function that maps acceptance probability to SE.
    
    # Reminder - n0 is the denominator for a0, but it is included in the data that
    # will be modified, so the notional data it denotes is subset of the max that will
    # be included in the Nmax :
    ## if (check){
    ##  if (nMax-parMat[ptN,5] < 0) {
    ##    print(paste('At ptN',ptN,' params:')); print(parMat[ptN,]);
    ##    stop('Please make sure Nmax-n0 >= 0');
    ##  }
    ##}
    
    # make sure 0 < SE <1 :
    datAr[vecTRUE(datAr[, 4, ptN] <= 0), 4, ptN] <- eps
    
    datAr[vecTRUE(datAr[, 4, ptN] >= 1), 4, ptN] <- 1 - eps
    
    
    # Now for acceptance prediction response function parameters:
    Tpred = parMat[ptN, 'Tpred']
    # this and Bpred below scaled to be
    Bpred = parMat[ptN, 'Bpred']
    # tuned to probability-like calcs ...
    
    
    for (trN in 1:Ntrtot) {
      abnPol[trN + 1, , ptN] <-
        abnPol[trN, , ptN]
      # Initialise-most will remain same.
      
      gpI <-
        datAr[trN, 1, ptN]
      # which group rated this pt
      
      #A: allow all abns to decay, encountered or not
      abnPol[trN + 1, aInd, ptN] <-
        (1 - decayCoeffGroups)*abnPol[trN, aInd, ptN] + decayCoeffGroups
      abnPol[trN + 1, bInd, ptN] <-
        (1 - decayCoeffGroups)*abnPol[trN, bInd, ptN] + decayCoeffGroups
      abnPol[trN + 1, nInd, ptN] <-
        abnPol[trN + 1, aInd, ptN] + abnPol[trN + 1, bInd, ptN]
      
      if (is.na(gpI)) {
        # if there was no valid group i.e. no 'rater' was presented
        # just keep propagated beliefs but don't attepmt anything of substance
        abnPol[trN + 1, c(SEPDI, obsPI, preAccPI, genSEI, expSEI), ptN] <-
          NA
        
        
      } else {
        # if there was valid group i.e. valid 'rater' was presented
        # Prob. of 'accept' response emitted (this is BEFORE rating seen),
        # if present. NB we will use beliefs after last trial, i.e. abnPol[trN,...(see notes if confused)
        ratingP <-
          1 / (1 + exp((1 - 2 * (
            abnPol[trN, aInd[gpI], ptN] / abnPol[trN, nInd[gpI], ptN] +
              Bpred
          )) / Tpred))
        
        abnPol[trN + 1, predAccPI, ptN] <-
          ratingP
        # Store
        
        predAcc <-
          datAr[trN, 2, ptN]
        # rating that actual participant predicted. #A: binary, 1 or -1
        
        
        
        
        if (!is.na(predAcc)) {
          if (predAcc > 0.5) {
            # i.e. if it's 1
            abnPol[trN + 1, obsPI, ptN] <- ratingP
            
          } else { #A: either 1 or 0 (0 if genD)
            abnPol[trN + 1, obsPI, ptN] <- 1 - ratingP
            
          }
        } # End if valid prediction predAcc
        
        nSoFar <- abnPol[trN, nInd[gpI], ptN]
        
        nofb = datAr[trN, 'nofb', ptN]
        # 0 if feedback given, 1 otherwise, so if no #A: 1 is true 0 is false
        # feedback given don't augment evidence index
        if (!nofb) {
          # If not feedback was given, we leave all the a,b,n alone, which
          # are already in place. !nofb means that feedback was given, so:
          apprfb = (datAr[trN, 3, ptN] + 1) / 2 
          # approval or not, i.e. convert from -1 1 to 0 1
          
          #A: update abns based on feedback
          abnPol[trN + 1, aInd[gpI], ptN] <-
            abnPol[trN + 1, aInd[gpI], ptN] + apprfb
          abnPol[trN + 1, bInd[gpI], ptN] <-
            abnPol[trN + 1, bInd[gpI], ptN] + 1 - apprfb
          abnPol[trN + 1, nInd[gpI], ptN] <-
            abnPol[trN + 1, aInd[gpI], ptN] + abnPol[trN + 1, bInd[gpI], ptN]
          
          #A: prediction error = PE
          if (apprfb == 1) {
            PE <-
              abnPol[trN + 1, bInd[gpI], ptN] / abnPol[trN + 1, nInd[gpI], ptN]
          }
          
          else {
            PE <-
              -abnPol[trN + 1, aInd[gpI], ptN] / abnPol[trN + 1, nInd[gpI], ptN]
          }
          
       
        } # end if valid feedback given
        else {
          PE = 0
        }  
        
        
        # Consider SE as a map from prob. of acceptance to a scale over c(0,1)
        # & calc. p density at the new SE reported, if valid. IT HAS TO CORRESPOND TO THE
        # WAY SE IS GENERATED (e.g. for synthetic data ...)
        # Generated SE may be: (1) random from the component (group) distro at hand; or
        #                      (2) random from an 'overall' distro, where we express the
        #                      overall SE distribution as derived from a mixture of Beta distros.
        #
        #                      ( or possibly from some central tendency with independent noise)
        #
        
        #Updating SE 
        
        abnPol[trN + 1, SEaI, ptN] <-
          decayCoeffSelf* (abnPol[trN + 1, SEaI, ptN] - 1) + 1 + weightSelf * max(PE, 0) #A: note change in decayCoeffSelf!!! to 1-
        abnPol[trN + 1, SEbI, ptN] <-
          decayCoeffSelf* (abnPol[trN + 1, SEbI, ptN] - 1) + 1 - weightSelf * min(PE, 0)
        abnPol[trN + 1, SEnI, ptN] <-
          abnPol[trN + 1, SEaI, ptN] + abnPol[trN + 1, SEbI, ptN]
        
        
        a <- abnPol[trN + 1, SEaI, ptN]
        b <- abnPol[trN + 1, SEbI, ptN]
        
        abnPol[trN + 1, expSEI, ptN] <- accP2SE(a / (a + b), A, B)
        
        # for debug:  abnPol[trN+1,genSEI,ptN] <- abnPol[trN+1,expSEI,ptN];
        abnPol[trN + 1, genSEI, ptN] <- accP2SE(rbeta(1, a, b), A, B)
        
        
        # End generation of SE values, expected and generated 'to report'.
        
        #  Now for the probability density at the actually measured SE
        #  in the experiment, if valid, using a and b calculated above:
        
        SEdat = datAr[trN, 4, ptN]
        
        if (is.na(SEdat)) {
          abnPol[trN + 1, SEPDI, ptN] <- NA
          
        } else {
          #  Expressed SE in terms of an acceptance probability :
          experAccP <- SE2accP(SEdat, A, B)
          
          #  Acceptance belief density at that point acc. to
          #  self esteem beta distribution:
          accPdens <- dbeta(experAccP , a, b)
          
          abnPol[trN + 1, SEPDI, ptN] <-
            accPdens * slopeSE2accP(SEdat, A, B, experAccP)
          #scaling
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
    DatBelPol[2:(Ntrtot+1),1:5,] <- datAr[1:Ntrtot,,];
    DatBelPol[,(5+1):(5+dim(abnPol)[2]),] <- abnPol[1:dim(DatBelPol)[1],,];
    for (ptN in 1:Nptot) {
      for (trN in 1:Ntrtot) {
        DatBelPol[trN+1,'genPred',ptN] <- rbinom(1,1,DatBelPol[trN+1,'predAccP',ptN]);
      }
    }
    SLPetc[[3]] <- DatBelPol;
    # 4th element to have generated data only :
    SLPetc[[4]] <- NA*datAr;  # shortest scripting to preserve dimentionality ...
    SLPetc[[4]][1:Ntrtot,,] <- DatBelPol[2:(Ntrtot+1), c('gp','genPred','obs','genSE','nofb'),] ;
    colnames(SLPetc[[4]]) <- c('gp','pred','obs','SE','nofb'); # Just like real expt. data ...
    SLPetc[[5]] <- parMat;
    colnames(SLPetc[[5]]) <- c('n0', 'a0min', 'a0max', 'Tpred', 'Bpred ','decayCoeffGroups', 'decayCoeffSelf', 'weightSelf', 'sensi','sesh');
    names(SLPetc) <-c('predSLnP','SESLnP','DatBelPol','genD','ptPar');
    
    return(SLPetc);
  }
  
} # end of SLPsocio4

# Param transf. for SLPsocio4, using 10-element input.
#par. row must be: c('n0', 'a0min', 'a0max', 'Tpred', 'Bpred ',
# 'decayCoeffGroups', 'decayCoeffSelf', 'weightSelf', 'sensi','sesh')
# e.g.   parMat =   c(6,     1,     4,     0.2,      0.1,    0.5,     0.5,    5,    1,    1)
# to ln(n0-a0max-1), ln(a0min-1), ln(a0max-a0min), ln(Tpred), Bpred, atanh(2decayCoeffGroups-1)
# atanh(2decayCoeffSelf-1), ln(weightSelf), ln(sensi), ln(sesh)
# atanh(2x-1) restricts values to be between 0 and 1, ln restricts values to positive. 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

tr2natLP4 <- function(trp,check=1){  # from transformed, i.e. -inf to inf, to native space
  #  trp to be as follows;
  #      c(ln(n0-a0max-1), ln(a0min), ln(a0max-a0min), ln(Tpred), Bpred, atanh(2decayCoeffGroups-1)
  # atanh(2decayCoeffSelf-1), ln(weightSelf), ln(sensi), ln(sesh))
  #  Returns:  c('n0', 'a0min', 'a0max', 'Tpred', 'Bpred ',
  # 'decayCoeffGroups', 'decayCoeffSelf', 'weightSelf', 'sensi','sesh')
  
  eps <- 1e-10;  #  eps is a tiny constant that can be used to guarantee
  #  that rounding errors, underflows etc. don't ruin strict inequalities.
  
  if (check){ if (is.null(dim(trp))){   # convert vec to mat if need be
    trp <- matrix(trp,nrow=1,byrow=TRUE) }   }
  ptTot <- dim(trp)[1]; 
  p <- matrix(NA,nrow=ptTot,ncol=dim(trp)[2]); 

  #n0 (1), a0min(2), a0max (3)
  p[,2] <- exp(trp[,2]);           # a0min
  p[,3] <- p[,2] + exp(trp[,3]);  # a0max
  p[,1] <- 1 + p[,3] + exp(trp[,1]);  # n0
  
  #the rest: 
  p[,c(4,8,9,10)] <- exp(trp[,c(4,8,9,10)]);  # Tpred, weightSelf, sensi, sesh
  p[,5] <- trp[,5];
  p[,6] <- 0.5*(1 + tanh(trp[,6])) ; # decayCoeffGroups
  p[,7] <- 0.5*(1 + tanh(trp[,7])) ; # decayCoeffSelf
  
  
  if (check){ colnames(p) <- c('n0', 'a0min', 'a0max', 'Tpred', 'Bpred ', 'decayCoeffGroups', 'decayCoeffSelf', 'weightSelf', 'sensi','sesh') };
  return(p); 
} 

nat2trLP4 <- function(p,check=1){  # From native to transformed.
  #  Returns trp as follows;
  #      c(ln(n0-a0max-1), ln(a0min), ln(a0max-a0min), ln(Tpred), Bpred, atanh(2decayCoeffGroups-1)
  # atanh(2decayCoeffSelf-1), ln(weightSelf), ln(sensi), ln(sesh))
  
  
  eps   <- exp(-25);   # So that for R 1+eps > 1
  minLn <- -1000;   # so that for R exp(minLn) == 0, exp(-minLn) == +Inf
  
  # Basic check - of argument format
  if (check > 0){ if (is.null(dim(p))){   # convert vec to mat if need be
    p <- matrix(p,nrow=1,byrow=TRUE) }   }    
  ptTot <- dim(p)[1]; 
  # Detailed check
  if (check > 1){
    for (ptN in 1:ptTot) {
      if (any(p[ptN,c(2,4,8,9,10)] < -2*eps)) { #A: why was it sum?
        print(paste('parMat 2,4,8,9,10 =',p[ptN,c(2,4,8,9,10)]));
        stop('--> ln of -ve    error' ); 
      }
      if ((p[ptN,1]-p[ptN,3]-1) < -2*eps) { 
        stop('ln(n0-a0max-1) error' );
      }
      
      if ((p[ptN,3]-p[ptN,2]) < -2*eps) {
        stop('ln(a0max-a0min) error' ); 
      }
      
    }
  }
  
  trp <- matrix(NA,nrow=ptTot,ncol=dim(p)[2]); 
  
  #coordinating transformations of n0 (1), a0min (2) and a0max (3)
   
  y <- p[,1]-p[,3]-1;  y[y<eps] <- eps; 
  trp[,1] <- log(y);            # ln(n0-a0min-1) 
  y <- p[,2];        y[y<eps] <- eps; 
  trp[,2] <- log(y);            # ln(a0min-1)
  y <- p[,3]-p[,2]  ;  y[y<eps] <- eps; 
  trp[,3] <- log(y);            # ln(a0max-a0min)
  
  # the others : ln(Tpred) (4), Bpred (5), atanh(2decayCoeffGroups-1) (6)
  # atanh(2decayCoeffSelf-1) (7), ln(weightSelf) (8), ln(sensi) (9), ln(sesh) (10)
  
  trp[,5]  <- p[,5]; #Bpred
  trp[,6]  <- atanh(2*p[,6]-1); #atanh(2decayCoeffGroups-1)
  trp[,7]  <- atanh(2*p[,7]-1); #atanh(2decayCoeffSelf-1) 
  y <- p[,c(4,8,9,10)]; y[y<eps] <- eps; trp[,c(4,8,9,10)] <- log(y); #ln(Tpred), ln(weightSelf), ln(sensi), ln(sesh)
           
  # rough bounding of under / overflows:
  trp[trp < minLn] <- minLn;    trp[trp > -minLn] <- -minLn;
  
  
  if (check){ colnames(trp)<- c('ln(n0-a0max-1)', 'ln(a0min)', 'ln(a0max-a0min)', 'ln(Tpred)', 'Bpred', 'atanh(2decayCoeffGroups-1)', 'atanh(2decayCoeffSelf-1)', 'ln(weightSelf)', 'ln(sensi)', 'ln(sesh)') }
  return(trp);
  
}  # end of nat2trLP3

msLP4tr <- function(trParM, datAr, Pri=NA, check=0){
  # trParM: transf. directly by tr2natLP4 
  
  #A: This bit is from msLP3tr - will edit later
  #   c('tr(SEb,SEmin)','tr(aB OR bB)','ln(a0min-1)',
  #                               'ln(n0-a0min-1)','tr(Nmax etc)','ln(Tresp)')
  # Pri has the means (row1) and sd's (row2) for priors on ParM IN NATIVE SPACE !!! REM:
  #         c('accP0', 'sensi', 'sesh', 'a0min', 'n0', 'nMax','Tpred', 'Bpred','nBal')
  #            beta     gamma    gamma   gamma   gamma  gamma   gamma   norm    gamma
  
  if (is.null(dim(trParM))){   # turn trParM into matrix if it's a vector
    trParM <- matrix(trParM,nrow=1,byrow=TRUE)      }
  
  if (check){
    if ((dim(datAr)[2]<4) || (dim(trParM)[2]<10)){
      stop('arguments trParM or datAr appear to have wrong dimensions');  }
  }
  parM <- tr2natLP4(trParM)
  
  # Cacl. the log prior for MAP purposes etc: #A: this is from SLPsocio3 - to edit later
  mSLPrior <- 0;
  if (length(Pri)>1){  # legit prior must have 20 elements
    for (ptN in 1:dim(trParM)[1]) {
      #            'n0', 'a0min', 'a0max', 'Tpred', 'Bpred ', 'decayCoeffGroups', 'decayCoeffSelf', 'weightSelf', 'sensi’, 'sesh'
      #            gamma  gamma    gamma    gamma    norm      beta                  beta         gamma      gamma     gamma
      # First the non-gamma-prior param:
      mSLPrior <- 
      -dbetaMS(parM[ptN,6], Pri[1,6],Pri[2,6])
      -dbetaMS(parM[ptN,7], Pri[1,7],Pri[2,7])
      -dnorm(  parM[ptN,5], Pri[1,5],Pri[2,5],log=TRUE) #Bpred. Note signs (both -ve). Please also note that SD for 6,7 prior must be between 0 and 0.25 due to nature of it being converted into a beta distribution.
      # now the gamma params, first the ones with min. zero:
      mSLPrior <- mSLPrior - sum(dgammaMS(parM[ptN,c(2:4,8:10)], Pri[1,c(2:4,8:10)],Pri[2,c(2:4,8:10)], log=TRUE)); 
      # n0 has min 1.0:
      mSLPrior <- mSLPrior - sum(dgammaMS(parM[ptN,1]-1.0, Pri[1,1],Pri[2,1], log=TRUE));
    }
  } 
  
  # debug line:
  #print(paste('Prior density:',mSLPrior))
  #print(cbind(parM,trParM));
  
  if (mSLPrior == Inf){  # If we are in an a priori prohibited parameter region
    # do not attempt to calculated the likelihood - it will be nonsense anyway.
    return(Inf); 
  } else {
    return ( mSLPrior - SLPsocio4( parM, datAr, onlySLP=1, check) ); #A: this is what is minimized - the difference between priors and actual (as gone through SLPsocio1) #To change to only SESLnP or only predSLnP, use eg. SLPsocio3(parM, datAr, onlySLP=0, check)$SESLnP
  }
  
} 

#start of nlm function
#if you would like it to have only one argument, set Par0 = flatpriors and remove Par0 as an argument

load("loadfornlm.RData") #this contains bestsofar, tryPmatrix, datArW03, and flatpriors

beliefModelFit <- function(pts,Par0) {
  ml1fit <- list();
  ml1res <- matrix(NA,nrow=dim(datArW03)[3],ncol=15);
  dimnames(ml1res)[[2]] <- c('n0', 'a0min', 'a0max', 'Tpred', 'Bpred ', 'decayCoeffGroups', 'decayCoeffSelf', 'weightSelf', 'sensi', 'sesh', 'predSLP', 'SESLD','SEcor','predProb','BIC');
  
  
  for (ptN in pts){ #1:dim(datArW03)[3] ){
    
    
    D <- array(NA,c(dim(datArW03)[1:2],1));  # Create & clear the working array
    D[,,1] <- datArW03[,,ptN];
    dimnames(D)[[2]] <- c('gp','pred','obs','SE','nofb');
    dimnames(D)[[3]] <- ptN;
    ml1fit[[ptN]] <- list();
    mPD <- Inf;
    
    tryPmatrixwbest<-matrix(NA,nrow=129,ncol=10)
    tryPmatrixwbest[1:128,] <- tryPmatrix
    tryPmatrixwbest[129,] <-bestsofar[ptN,]
    allsets <- matrix(NA,nrow=129,ncol=12)
    
    for (set in 1:129) { #for (set in c(1,25,50,75,100,125)) { for testing
      tryP = tryPmatrixwbest[set,]
      iniLen=length(tryP);
      
      ## this part included if further randomised attempts are wanted - remove the double #s and add attempts as an argument
      ##for (attempt in 1:attempts){  # 2-10 for testing; try (10*iniLen) for real { #put attempts back into function as an argument if you want
      ##if (attempt == 1){
      ##    iniTrPar <- nat2trLP4(tryP);
      ##} else {
      ##    iniTrPar <- nat2trLP4(tryP) * runif(10,0.75,1.25);
      ##}
      ## # atI <- attempt %% iniLen; if (atI == 0){atI <- iniLen;};
      ## #if (attempt > 11) {
      ## #    iniTrPar <- as.numeric(iniTrParM[atI,] * runif(10,0.8,1.2));
      ## #}
      
      print(paste('ptN:',ptN,';  fit attempt:', attempt));  print(paste('Init. Cond:', paste(round(tr2natLP4(iniTrPar),3),collapse=',')));
      try( fitAttempt <<- nlm(msLP4tr, iniTrPar, D, Par0, print.level=2, iterlim=500)
           
      ); # Par0, print.level=2, iterlim=500) 
      
      
      
      if (vecTRUE(length(fitAttempt$estimate)>1)){
        if ( vecTRUE(fitAttempt$minimum < mPD) || !(vecTRUE(length(ml1fit[[ptN]][[1]])>1)) ){
          mPD <- fitAttempt$minimum;
          ml1fit[[ptN]][[1]] <- fitAttempt;
        }
        #now to save estp and summed log likelihoods for all trials
        estpOfAttempt <- (tr2natLP4(fitAttempt$estimate)) ;
        allsets[set,1:10] <- estpOfAttempt
        allsets[set,11]   <- SLPsocio4(estpOfAttempt, D)$SESLnP
        allsets[set,12]   <- SLPsocio4(estpOfAttempt, D)$predSLnP
        ml1fit[[ptN]][[3]] <- allsets 
      }
      
      
      ## } 
    } # End exploration of initial conditions
    est10p <- (tr2natLP4(ml1fit[[ptN]][[1]]$estimate)) ;
    ml1fit[[ptN]][[2]] <- SLPsocio4(est10p, D);
    names(ml1fit[[ptN]]) <- c('NLM','SLP','alltrials')
    # output array storage
    ml1res[ptN,1:10] <- tr2natLP4(ml1fit[[ptN]][[1]]$estimate);
    ml1res[ptN,11]   <- ml1fit[[ptN]][[2]][[1]];
    ml1res[ptN,12]   <- ml1fit[[ptN]][[2]][[2]];
    ml1res[ptN,13]   <- round(cor(na.omit(data.frame(D[,'SE',1], expSE)))[1,2],2) #SE correlation, 2sf
    ml1res[ptN,14]   <- exp(ml1res[ptN,11]/192) #percentage of right predictions 
    #now the individual BIC
    LnforBIC = ml1fit[[ptN]][[2]]$predSLnP + ml1fit[[ptN]][[2]]$SESLnP 
    nforBIC = length(na.omit(D[,'pred',1]))+ length(na.omit(D[,'SE',1])) 
    kforBIC = 10 #number of params
    ml1res[ptN,15]   <- log(nforBIC)*kforBIC - 2*LnforBIC #BIC formula
    
    #now to save images - here they are saved in the baseDir
    mypath <- file.path(baseDir,paste("myplot_", ptN, ".png", sep = "")) 
    png(file=mypath, width = 912, height = 742, units = "px")
    
    # Prepare for graphs with real & randomly generated data for visual inspection:
    v <- D[,'SE',1]; v <- 1+v; v<- v/v;  # create vector of 1's and NA's, where 1 is when an SE was present
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
         main=paste('MAP fit, counting model (SLPsocio4):   pt',ptN,';  cor=',c,'\n[green: measured;   blue:fitted,  pink: generated from fit]'),
         xlab='trial number',
         ylab='Self Evaluation');
    
    lines(d2plot[,'expSE'],t='l',col='blue',lwd=3);
    lines(d2plot[,'genSE'],t='l',col='pink3');
    
    dev.off()
    
    #print(ml1res[1:ptN,])
    save.image(paste(baseDir,"socio1fitTEST.RData"))
    ml1fit <<- ml1fit #so that these objects have global assignment
    ml1res <<- ml1res #otherwise, only exist within the function
  }
}
beliefModelFit(1:21,flatpriors)
save(ml1fit, ml1res, file = "nlmoutputs.RData") #outside because they only need to be 
write.csv(ml1res, file = "ml1res.csv") #saved after all data run
#end of nlm function
