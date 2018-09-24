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
  # par. row must be: c('accP0', 'a0min', 'a0max', 'Tpred', 'Bpred ',
  # 'decayCoeffGroups', 'decayCoeffSelf', 'weightSelf', 'sensi','sesh')
  # e.g.   parMat =   c(0.67,     1,     6,     1,      1,    0.5,     0.5,    5,    1,    1)
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
      'accP0',
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
      ((M - 1):0) * (parMat[ptN, 3] - parMat[ptN, 2]) / (M - 1) + parMat[ptN, 2]
    
    abnPol[1, nInd, ptN] <-
      (parMat[ptN, 2] + (parMat[ptN, 3] - parMat[ptN, 2]) / 2) / parMat[ptN, 1]
    
    #because accP0 = mean of alphas / n, n = mean of alphas / accP0
    
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
    
    # The shift or offset sesh has to be consistent with baseline SE and other beliefs.
    # so that if expectations above the approval rates etc. turned out to be true,
    # then self-evaluation would, remain stable. So, if the above were true, the
    # average acceptance rate would be:
    #nBal <- parMat[ptN,'nBal'];
    #aBal <- parMat[ptN,'accP0']*nBal;    bBal <- nBal - aBal;
    n0   <- #A: CHECK THIS
      (parMat[ptN, 2] + (parMat[ptN, 3] - parMat[ptN, 2]) / 2) / parMat[ptN, 1] #A: see above
    accP <- sum(abnPol[1, aInd, ptN]) / (n0 * 4)
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
      abnPol[trN + 1, bInd, ptN] <-
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
            
          } else {
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
        
        
        
        # Consider SE as a map from prob. of acceptance to a scale over c(0,1)
        # & calc. p density at the new SE reported, if valid. IT HAS TO CORRESPOND TO THE
        # WAY SE IS GENERATED (e.g. for synthetic data ...)
        # Generated SE may be: (1) random from the component (group) distro at hand; or
        #                      (2) random from an 'overall' distro, where we express the
        #                      overall SE distribution as derived from a mixture of Beta distros.
        #
        #                      ( or possibly from some central tendency with independent noise)
        #
        
        #A: last close bracket before function closes
        
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
    colnames(SLPetc[[5]]) <- c('accP0', 'a0min', 'a0max', 'Tpred', 'Bpred ','decayCoeffGroups', 'decayCoeffSelf', 'weightSelf', 'sensi','sesh');
    names(SLPetc) <-c('predSLnP','SESLnP','DatBelPol','genD','ptPar');
    
    return(SLPetc);
  }
  
} # end of SLPsocio4