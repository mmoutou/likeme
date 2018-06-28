# General utilities & math functions ...
#
# source('~/Dropbox/FIL_aux/R_scripts/gen_ut.R')

# -------------------------------------
# sqrtVar     quick est. of sigma; apply to df columns: apply(dataFrame,2,sqrtVar)
# repRowMat()
# vecTRUE()
# pGibbs()
# logit()
# invlogit()
# ussig()              # Unit square sigmoid, x^z/(x^z + (1-x)^z) (Chris Matthys)

# dgammaMS()
# rgammaMS()
# dlnormMS()
# rlnormMS()

# betamean()
# betamode()
# betavar()
# dbetaMS(x, Mean, SD, ncp=0, log=FALSE)
# dbetasc()
# rbetasc()
# tbetvar(v,tau)       # for a beta distro with a,b > 1, sharpen or
                       # broaden acc. to a temerature tau. tau=1 leaves
                       # v unchanged, tau=0 turns v=0 too.
# talphabeta(a,b,v)    # take a beta distro with a, b and find new a,b
                       # so the distro has same mode but variance v.
# alphabetatau(a,b,T)  # Keep mode same but sharpen acc. to tbetvar. 

# noisyBino()          # noisyBino(pSucc, U, binN ) this is the whole mdf for
                       # applying extra uncertainty U to binomial distro of binN-1
                       # draws w. success param pSucc as per
                       # MDF = dbinom(0:n,n,pSuccs); MDF = MDF .^ 1/U ... etc.
                       # Note it will also produce concave, bimodal distros e.g.
                       # barplot(noisyBino(0.525,-0.5,4)) or barplot(noisyBino(0.525,-5,14))
# sharpBino()          # just like above but with shaprness param Sh = 1/U AND ALSO
                       # returns log probs if argument ln=TRUE
# catBetish()          # categorical mdf for categories 1 ... catN based on
                       # discretization of beta distro with the params aish,bish a la R
# slcatBetish()        #  slcatBetish(d, aish,bish, catN) : sum log likelihood of 
                       #  data vector d under aish, bish, catN specifying
                       #  distribution catBetish
# rmdf()               # draw k random samples 1...n based on an mdf encoded in a
                       # prob. vector p1,p2,...,pn (sum pi = 1)
# mdfDkl()             # mdfDkl(q,p,check) find Dkl between rows of q and rows
                       # of p, as per sum(q * (log(q)-log(p)).
# repAdjVec()          # repeat c(a,b,c) as c(a,a,a,...,b,b,b,...,c,c,c,...)
# grid1D()             # to tabulate fns. negligible beyond given range. 
                       #       See also seq(from = 5, to = 100, by = 5)
# List2Dgrid()         # 2D grid in terms of a dataframe of all pairs of coords
# perm12(k)            # matrix w. all the k long binary strings 1..1,1..2 to 2..2
# grid2DfromList()
# error.bar
# det2DcolMat()        # determinants of 2x2 vectorized matrices given as rows.
# inv2DcolMat()        # inverse matrices for set of 2x2 vectorized mats
# mm2mult()            # vectorized fn for mult lots of 2x2 matrices pairwise
# mv2mult()            # postmult 2D mat by vec
# KeplerInt1()         # Kepler / Simpson rule
# fitparabmax(xy)      # return the precision, max, etc. around the max (or min)
                       # of the 1-d map x->y
# aggregateCSV         # fn to run in the background to assemble csv file outputs
                       # into one, and stop when N of them done.
# CSV utilities
# importCSV            # importCSV(fName) Simple wrapper to return dataframe
                       # imported from a nice CSV file.
# exportCSV            # exportCSV(fName, Dat) Simple wrapper to write
                       #   dataframe to a nice CSV file.  Note that vectors are
                       #   written as a single-row CSV - make into
                       #   a 1-col matrix if need be.
# cuberoot(x)          # Return all  the complex cube roots of a number
# polyno(x,coeffs)     # a*x^n + b*x^(n-1) ..., coeffs = c(a,b,...)
# cubsol(coeffs)       # solves analytically the cubic equation and
                       # returns a list whose first element is the real roots and the
                       # second element the complex roots.


try(library(ppcor))    # Just because I use it all the time !!

#
tol1 <- 1e-10;  # additive tolerance so that 1 - (1-tol1) appox== tol 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# quick estimate of sigma from sample - excluding NAs
# Can apply to df columns as per  apply(dataFrame,2,sqrtVar)
sqrtVar <- function(X) { return( sqrt(var(na.omit(X)))); }

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
importCSV <- function(fName) {
  # Simple wrapper to return dataframe imported from a nice CSV file.

  imported <- read.table (file=fName, header=TRUE, sep=',', quote='"\'', dec='.', fill=FALSE, comment.char="#",  na.strings = "NA", nrows = -1, skip = 0, check.names = TRUE, strip.white = TRUE, blank.lines.skip = TRUE)

  return(imported)
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
exportCSV <- function(fName, Dat, verbo=FALSE) {
  # Simple wrapper to write dataframe to a nice CSV file.
  # Note that vectors are written as a single-row CSV - make into
  # a 1-col matrix if need be.

  # First reformat vectors and matrices as dataframes.
  if(is.vector(Dat)){
      rowN <- 1; colN <- length(Dat);
      dat <- data.frame(matrix(rep(NA,colN),1));
      colnames(dat) <- labels(Dat);
      dat[1,] <- Dat;
  }
  if (is.matrix(Dat)) {
      dat <- data.frame(Dat)
      colnames(dat) <- dimnames(Dat)[[2]];
      rownames(dat) <- dimnames(Dat)[[1]];
  }

  write.table ( x =  Dat, file = fName , append =  FALSE  ,quote = TRUE ,  sep = ',' , eol = "\n" , na = "NA" ,  dec = '.' , row.names =  FALSE ,  col.names =  TRUE , qmethod=  'escape' );
## Print result if need be
 if (verbo){  try(print(paste(fName, 'written to disk'),quote=FALSE)); }
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
repRowMat <- function(x, renN) {
#
 if (!(is.vector(x))){
    stop('non - vector 1st argument in repRowMat')
  }
 return(t(matrix(rep(x,renN),nrow=length(x)))); 
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
vecTRUE <- function( x ){
# return vector of same length as input with 'TRUE' only where
# x is clearly != 0, and 'FALSE' otherwise - even for NAs.

vTRUE <- (!(x == 0)); 
vTRUE[is.na(vTRUE)] <- FALSE;
return(vTRUE);
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pGibbs <- function(q,T=1.0,ind=NA) {
# Gibbs softmax probabilities for vectors q and temperatures T.
# Expect q to be a matrix with columns the different actions, all
# subject to the same T.
# If ind is NA, return the whole pGibbs matrix; 
# otherwise for cols. in ind.

# old comment - for future devel:
# Should use the result to create a number of decisions - in 1-D case
# policy <- matrix( pGibbs(c(-1,0,1)),1)
# choices <- as.vector( rMultinom( policy, 100));

# test with
# T <- c(0.1,0.3,1,2); q <- t(matrix(rep(c(0.1,0.5,0.6),4),3)); ind <- c(1,1,2,3);
    
if (is.vector(q)){  q <- matrix(q,1); }; # convert to 1-row matrix
if (!(is.vector(T))){ stop('T must be a vector in pGibbs'); };
if (!(length(T)==dim(q)[1])){ stop('q and T incompatible');  };

T <- matrix(rep(T,dim(q)[2]),length(T)) ; # convert to matrix for ease
                                          # of vect. op.
unNorm <- exp(q/T);
denoms <- rowSums(unNorm);
denoms <- matrix(rep(denoms,dim(q)[2]),dim(q)[1]); # again into matrix
pGibbs <- unNorm / denoms;

if (is.na(ind[1])) {
    return( pGibbs );
}
else {
    if (!(length(ind) == dim(q)[1])){stop('q and ind incompatible');}
    else{
      ind <- matrix(rep(ind,dim(q)[2]),length(ind));
      # Next row a bit ridiculous - can't I just select the round(ind) elements??
      return(  rowSums(  pGibbs*(col(ind) == round(ind)))   );
    }
}
    
}  # end of fn. pGibbs

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
logit <- function(x, a=1) {
#  logit of x bet. 0 and a. x can be multidim.
    
if (!isTRUE( sum( a > x) == length(as.matrix(x,rown=1)))) {
    stop('All elements of x need to be < a');
}
if (!isTRUE( sum( x > 0) == length(as.matrix(x,rown=1)))) {
    stop('All elements of x need to be > 0');
}

res <- log( x / (a-x));
return(res);

}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
invlogit <- function(x, a=1) {
  #  inverse of logit of x bet. 0 and a
  return( a/(1+exp(-x)));
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ussig <- function(x, z) {
#  unit square sigmoid f(x) = x^z/(x^z + (1-x)^z)
    
if (!isTRUE( sum( 1 >= x) == length(x))) {
    stop('Unit square sigmoid: all elements of x need to be <= 1');
}
if (!isTRUE( sum( x >= 0) == length(x))) {
    stop('Unit square sigmoid: all elements of x need to be >= 0');
}
 
return(  1/(1+(((1-x)/x)^z)) );

}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# fns to discretize
# discretize x to the nearest integer from 1 to N
discint <- function(x, N){
  # discretize x to the nearest integer from 1 to N
  discx = round(x);
  discx[discx < 1] = 1;
  discx[discx > N] = N;
  return(discx);
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
discmdf <- function(x, mdf){
# A convenient function to use as a prior on the real line so
# that when said line is transformed by discint above, we have
# a prior mdf (as per argument of this fn) over this transformed set.
  N = length(mdf);
  dsx <- discint(x, N);
  xdf <- rep(0, length(x));
  # just look up the elements of x that do not correspond
  # to the end bins of mdf :
  xdf[dsx > 1 & dsx < N] = mdf[ dsx[dsx > 1 & dsx < N] ] ; 
  # Now at the low end:
  klo = -1-mdf[2]/mdf[1];
  xdf[dsx==1] = mdf[2]*(2.5-x[dsx==1])^klo ;
  khi = -1-mdf[N-1]/mdf[N];
  xdf[dsx==N] = mdf[N-1]*(-N+1.5+x[dsx==N] )^khi;
  
  return(xdf); 
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# fns. to use mean and sd to call gamma distro R functions, instead of rate and scale
#dgamma(x, shape, rate = 1, scale = 1/rate, log = FALSE) 
#pgamma(q, shape, rate = 1, scale = 1/rate, lower.tail = TRUE,  log.p = FALSE)
#qgamma(p, shape, rate = 1, scale = 1/rate, lower.tail = TRUE, log.p = FALSE)
#rgamma(n, shape, rate = 1, scale = 1/rate)
dgammaMS <- function(x, Mean, SD, log= FALSE) {
   Scale <- SD*SD / Mean;
   Shape <- Mean / Scale;
   res <- dgamma(x, shape=Shape, scale=Scale, log=log)
   return( res );
}
# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
rgammaMS <- function(n, Mean, SD) {
   Scale <- SD*SD / Mean;
   Shape <- Mean / Scale;
   
   return( rgamma(n, shape=Shape, scale=Scale));
}
# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
# lognormal distribution of x with mean > 0 and sd > 0 in terms of these, mean
# and sd, rather than the mean and sd of log(x)
dlnormMS <- function(x, Mean, SD, log=FALSE){

    c <- log(1+ (SD/Mean)^2);
    meanLog <- log(Mean) - c/2;
    sdLog   <- sqrt(c);

    return( dlnorm( x, meanlog=meanLog, sdlog=sdLog, log=log) );

}
# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
rlnormMS <- function(n, Mean, SD){

    c <- log(1+ (SD/Mean)^2);
    meanLog <- log(Mean) - c/2;
    sdLog   <- sqrt(c);

    return( rlnorm(n,  meanlog=meanLog, sdlog=sdLog) );

}
# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
betamean <- function(a,b){
  return(a/(a+b));
}
# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
betamode <- function(a,b){
  if ((a <=1) || (b<=1)){
    if (a>=b) { m=1; } else {m=0;}
  } else {
    m = (a-1)/(a+b-2)
  }
  return(m);
}
# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
betavar <- function(a,b){
  return(a*b/((a+b+1)*(a+b)^2));
}
# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
dbetaMS <- function(x, Mean, SD, ncp=0, log=FALSE){
# Ha ha this started as an excuse to make note of the inversion formulae :-)
    a <- (1-Mean)*(Mean*Mean)/(SD*SD) - Mean;
    b <- a*(1/Mean - 1) ;
    return( dbeta(x, a, b, ncp, log) );
}

# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
# beta distribution with scaled x values, so that instead of bet. 0 and 1,
# the random var obtains values between lo and hi. Can have lo > hi, in
# which case they are mirrored.
dbetasc <- function(x, shape1, shape2, lo=0, hi=1, ncp=0, log=FALSE){

 xtr <- (x-lo)/(hi-lo); # will work even if hi<lo  
 if (log==FALSE) {
     return( dbeta( xtr, shape1, shape2, ncp, log)/abs(hi-lo) );
 }
 else {
     return( dbeta( xtr, shape1, shape2, ncp, log) - log(abs(hi-lo)) );
 }
}
# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
rbetasc <- function(n, shape1, shape2, lo=0, hi=1, ncp=0){
    auxv <- rbeta(n, shape1, shape2, ncp);
    return(  lo+ auxv*(hi-lo) );
}

# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
noisyBino <- function(pSucc, U, binN ){
   # this is the whole mdf for applying extra uncertainty U to binomial 
   # distro of binN-1 draws w. success param pSucc as per
   #  MDF = binopdf(0:(binN-1),binN-1,pSuccs); MDF = MDF ^ 1/U ... etc.
 n = binN-1;
 MDF = dbinom(0:n,n,pSucc);
 MDF = MDF ^ (1/U);
 return( MDF / sum(MDF));
}
# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
sharpBino <- function(pSucc, Sh, binN, ln=FALSE ){
  # this is the whole mdf for applying extra Sharpness Sh to binomial 
  # distro of binN-1 draws w. success param pSucc as per
  #  MDF = binopdf(0:(binN-1),binN-1,pSuccs); MDF = MDF ^ Sh ... etc.
  # returns log probs if need be.
  n = binN-1;
  MDF = dbinom(0:n,n,pSucc);
  MDF = MDF ^ Sh;
  if (!ln) {
    return( MDF / sum(MDF));
  } else {
    log(MDF) - log(sum(MDF))
  }
}
# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
slSharpBino <- function(d, pSucc, Sh, binN){
  # sum log likelihood of data vector d under other arguments specifying
  # distribution sharpBino. The elements of d to take vals from 1 to binN .
  
  fc  = as.matrix(table(d));         # this counts categories as factor levels ...
  ifc = as.numeric(rownames(fc));    # so back into numbers here !
  fvec = rep(0,binN);
  fvec[ifc] = fc[,1];
  
  lp = sharpBino(pSucc, Sh, binN, ln=TRUE);
  return(fvec %*% lp); 
}
# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
mslSharpBiff <- function(trPar,d, binN){
  # as per slSharpBino, but pSucc = 1/(1+exp(trPar[1])) and Sh=trPar[2],
  # and sign reversed hence m rather than msl, f_or f_itting with nlm and the like.
  # sum log likelihood of data vector d under other arguments specifying
  # distribution sharpBino. The elements of d to take vals from 1 to binN .
  
  pSucc = 1/(1+exp(trPar[1]));       Sh=trPar[2];   # params back to native space.
  
  fc  = as.matrix(table(d));         # this counts categories as factor levels ...
  ifc = as.numeric(rownames(fc));    # so back into numbers here !
  fvec = rep(0,binN);
  fvec[ifc] = fc[,1];
  
  lp = -sharpBino(pSucc, Sh, binN, ln=TRUE);  # note minus sign.
  return(fvec %*% lp); 
}
# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
#fitSharpBino(d, binN, init=c(0.5,1)){
   #
#}
# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
catBetish <- function(aish,bish, catN, ln=FALSE){
    # discretization of the beta distro so that the unit interval is 
    # divided into catN bins, the middle of each is used to derive the relative
    # height of the mdf and the whole thing is normalized. As if the middle of
    # bin 1 is \propto probability of drawing category 1, etc. to category catN .
    # Try catBetish(0.5,0.4,4) for funsies. 1,1,N is flat . 
    # return the logs of the mdf if need be.
    MDF = dbeta( ((1:catN)/catN - 0.5/catN), aish, bish);
    if (!ln) {
      return( MDF / sum(MDF) );
    } else {
      return( log(MDF) - log(sum(MDF)));
    }
}
# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
slcatBetish <- function(d, aish,bish, catN){
    # sum log likelihood of data vector d under aish, bish, catN specifying
    # distribution catBetish
  
    fc  = as.matrix(table(d));         # this counts categories as factor levels ...
    ifc = as.numeric(rownames(fc));    # so back into numbers here !
    fvec = rep(0,catN);
    fvec[ifc] = fc[,1];
    
    lp = catBetish(aish, bish, catN, ln=TRUE);
    return(fvec %*% lp); 
}
# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
mdfDkl <- function(q, p, check=0) {

if (check != 0){
  dimq <- dim(q);
  dimp <- dim(p);
  
  if ( !(is.null(dimq)) && vecTRUE(length(dimq) != 2 ) ){
      stop('q must be p vector or matrix with rows of p-vectors in mdfDKL'); }
  if ( !(is.null(dimp)) && vecTRUE(length(dimp) != 2 ) ){
      stop('p must be p-vector or matrix with rows of p-vectors in mdfDKL'); }
  if (is.null(dimq)){  # i.e. we have 2 row vectors
      if (length(p) != length(q)){
          stop('p and q must have same length in  mdfDKL'); }
      if (abs(sum(p)-1) + abs(sum(q) - 1) > 1e-5) {
          stop('p and q must each add up to 1 in  mdfDKL'); }
  } else {
      if ( sum(abs( dim(p) - dim(q) )) ){
          stop('p and q must have same dimensions in  mdfDKL'); }
      if ( sum(abs(rowSums(p)-1) + abs(rowSums(q) - 1)) > 1e-5 ) {
           stop('rowSums of p and q must be 1s in  mdfDKL'); }
  }
}

if (is.null(dim(q))){ 
 return ( sum( q * (log(q) - log(p)) )) ;
} else {
 return ( rowSums(  q * (log(q) - log(p)) ));
}

}
# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
# Produce k random samples from the arbirary probability vector mdf
# test with e.g. hist(rmdf(10000,dbinom(0:5,5,0.8)) )
rmdf <- function(k, mdf,check=FALSE){
    cdf <- cumsum(mdf);
    n <- length(mdf);
    if (check){
      Tol=1e-8;  # for rough checking ...
      if (abs(cdf[n] - 1) > Tol)
      { stop('mdf provided does not add up to 1 within 1e-16'); };
    }
    cdf[n] <- 1.0;  # force it to 1, for good measure ...
    x <- runif(k);
    CDF <- repRowMat(cdf,k);
    return( 1+n - rowSums(CDF >= x));
}

# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
# Repeat each element adjacent to itself N times so that c(1,2,3) becomes e.g.
# c(1,1,2,2,3,3)
repAdjVec <- function( inVec, repN) {

 return(  as.vector((rep(1,repN)) %*% t(inVec)) );
 
}

# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
# A 1-D regular grid for a fn. that can be neglected +/- Nsinf*sigma from its
# mean, where each sigma is represented by nsig points. NB THIS SORT OF FN
# MUST BE GIVEN double ARGS, NOT single-member dataframes etc.
grid1D <- function(mu,sig,Nsinf=4,nsig=15) {
    return(((-Nsinf*nsig):(Nsinf*nsig))*sig/nsig + mu) ;
}
# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
List2Dgrid <- function( xGrid, yGrid) {
# Given a 2 dimensional grid defined by the one-dimensional vectors xGrid and
# yGrid, return a listing of all the xy pairs as a dataframe

X <- as.vector(rep(1,length(yGrid)) %*% t(xGrid));
Y <- as.vector(t((rep(1,length(xGrid)) %*% t(yGrid))))
XY <- data.frame(X,Y);
colnames(XY) <- c('X','Y');
return(XY);
}
# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
perm12 <- function( k ){
 # matrix w. all the k long binary strings 1..1,1..2 to 2..2
  per = matrix(0,nrow=2^k,ncol=k); 
  for (col in 1:k){ 
    per[,col] = rep(as.vector(repRowMat(1:2,2^(k-col))) , 2^(col-1))
  }
  return(per)
}
# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
grid2DfromList <- function( XYlist, dims){
# return the 1-D grids that gave rise to a list of pairs of numbers ...
# Only does very basic checking.
    
if (!(dim(XYlist)[1] %% dims[1]) == 0){
  stop('The length of XY list is not a multiple of dims[1]');  
}
if (!(dim(XYlist)[1] %% dims[2]) == 0){
  stop('The length of XY list is not a multiple of dims[2]');  
}

yGrid <- XYlist[1:dims[2],2] ;  # 2nd column is the Y's
xInds <- (0:(dims[1]-1))*dims[2] + 1;
xGrid <- XYlist[xInds,1];

return( list(xGrid=xGrid,yGrid=yGrid) ); 
    
}

# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
# Simple error bars: (see further below)
# error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
#    if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
#    stop("vectors must be same length")
#    arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
#}
# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
#
det2DcolMat <- function( colMat ) {
# colMat is a (set of) 2 d matrix(ces) vectorized per column,
#    so each row of colMat is an c(a11,a12,a21,a22)
#    Return rows of NAs for rows where the determinant is negative.

    if (is.vector(colMat)){
      if (length(colMat) == 4) {
        return( colMat[1]*colMat[4] - colMat[2]*colMat[3] );
      }
      else {
        stop('det2DcolMat is only for sets of 2D matrices vectorized by column'); 
      }
    }
    if (is.matrix(colMat)){
        if (length(colMat[1,]) == 4){
           return ( colMat[,1]*colMat[,4] - colMat[,2]*colMat[,3] );
        }
    }
    else {
        stop('det2DcolMat is only for sets of 2D matrices vectorized by column');
    }
}
# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
#
inv2DcolMat <- function( colMat ) {
# For inputs as per det2DcolMat, ret. the inverse matrices (or NAs if
# det <= 0 )

Dets <- det2DcolMat( colMat); 
if (is.vector(colMat)) {
   inv2 <- rep(NA,4);
   if (Dets > 0){
       inv2 <- (colMat[c(4,2,3,1)]*c(1,-1,-1,1))/Dets ;
   }
}
else {
  if  (is.matrix(colMat)){
    inv2 <- array(rep(NA,length(colMat)),dim(colMat));
    posInd <- vecTRUE( Dets > 0);
    posN <- sum(posInd);
    invDetVs <- (1/Dets[posInd]) %*% t(c(1,-1,-1,1));
    inv2[posInd,] <- colMat[posInd,c(4,2,3,1)] * invDetVs;
  }    
  else {
     stop('inv2DcolMat is only for sets of 2D matrices vectorized by column');
  }
}   
return(inv2);

}
# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
#
mm2mult <-function(A,B) {
# multiply 2 2D matrices vectorized by col.
# Rows of A and B represent different matrices (so fn. is vectorized).

C <- A;  # just make space ...
if (is.vector(A)){
    C[1] <- A[1]*B[1]+ A[3]*B[2];
    C[2] <- A[2]*B[1]+ A[4]*B[2];
    C[3] <- A[1]*B[3]+ A[3]*B[4];
    C[4] <- A[2]*B[3]+ A[4]*B[4];
}
else {
    C[,1] <- A[,1]*B[,1]+ A[,3]*B[,2];
    C[,2] <- A[,2]*B[,1]+ A[,4]*B[,2];
    C[,3] <- A[,1]*B[,3]+ A[,3]*B[,4];
    C[,4] <- A[,2]*B[,3]+ A[,4]*B[,4];
}

return(C);

}
# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
#
mv2mult <- function(A,b) {
# postmultiply a 2D matrix vectorized by col. by a 2D vector.
# Rows of A and b represent different matrices (so fn. is vectorized).

v <- b;  # just make space ...
if (is.vector(A)){
    v[1] <- A[1]*b[1]+ A[3]*b[2];
    v[2] <- A[2]*b[1]+ A[4]*b[2];
}
else {
    v[,1] <- A[,1]*b[,1]+ A[,3]*b[,2];
    v[,2] <- A[,2]*b[,1]+ A[,4]*b[,2];
}

return(v);

}

# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
#
# Use this after a 'plot' or 'barplot' call ...
#
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
    if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
    arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}
# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
#
KeplerInt1 <- function(Y,h,N,Check=0){
  if (Check != 0) {
    if ( (1+2*round((N-1)/2)) != N ) {
      stop('N is even')
    }
  }
  # if Y is a simple vector, turn it into a 1-row array:
  if( is.null(dim(Y))){  
    Y <- array(Y,c(1,length(Y)));  
  }
  
  Ie <- (1:((N-1)/2))*2;     # for 4 x
  Io <- (1:((N-3)/2))*2+1;   # for 2 x
  Ye <- array(Y[,Ie],c(dim(Y)[1],length(Ie)));
  Yo <- array(Y[,Io],c(dim(Y)[1],length(I)));

  
  return( (h/3)*(Y[,1]+Y[,N]+4*rowSums(Ye)+2*rowSums(Yo)) )
}
# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
# fitparabmax(xy)      # return the precision, max, etc. around the max (or min)
# of the 1-d map x->y. 
# Test with xy = matrix(c(1:10,8-3.14*((1:10)-3.25)^2),10,2); xy
fitparabmax <- function(xy,K=5,mx=TRUE){
  if (mx){ ymInd <- which.max(xy[,2]) } else {ymInd <- min(xy[,2])}
  ym = xy[ymInd,2]; xm=xy[ymInd,1];
  N = dim(xy)[1];
  # Now find DELIBERATELY SMALL grid around maximum for the quadratic fit
  # First for CS :
  rlo <- rhi <-  ymInd;
  if (ymInd < K){               # too close to bottom
    rlo <- 1; shortfall <- K-ymInd;
  } else {
    rlo <- ymInd - 4; shortfall <- 0;
  }
  if (ymInd > N-4){  # too close to top (shortfall above irrelevant)
    rhi <- N;
    rlo <- rlo + ymInd - N ; 
  } else {  
    rhi <- ymInd + 4;
    if (rhi < N - shortfall){
      rhi <- rhi + shortfall;
    }  # this if is mostly decorative - shortfall 
    # will be non-zero only if ymInd is very near the bottom anyway.
  }
  # use the grid to fit a parabola very simply by lin. regrn. :
  Y <- xy[rlo:rhi,2];
  X  <- xy[rlo:rhi,1];                Xsq <- X*X; 
  YXXsq <- lm(Y ~ (X + Xsq))
  abc <- YXXsq$coefficients;
  
  ###   KEY OUTPUTS - the estimated CI etc ###
  # use the fitted coefficients to find fitted max, sd, and
  # coordinate at max:
  res <- c(NA,4);
  res[3] <- -abc[2]/(2*abc[3]);       # coordinate at max
  res[2] <- abc[1] - abc[3]*res[3]^2; # max. LL - interpolated via CS
  res[1] <-  -2*abc[3];               # precision of the Laplace approx gaussian
  res[4] <-  1/sqrt(res[1]);
  names(res) <- c('prec','ym','xm','xysd');
  return(res);
  
}

# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
aggregateCSV <- function(nameFrag,fileNum,Dt=600) {
  # run this in the background, e.g. in a screen session, to check every Dt
  # sec whether fileNum files with obeying
  found <- 0;
  while (found < fileNum){
      fileNames <- dir( patt = paste('.*',nameFrag,'.*csv$',sep=''));
      found <- length(filenames);
      
  }
    
} # end aggregateCSV


# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
polyno <- function(x, coeffs){
  nterms = length(coeffs)
  y = 0;
  for (ord in 1:nterms){
    y = y + coeffs[ord]*x^(nterms-ord)}
  return(y)
}
# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
# Return all  the complex cube roots of a number
cuberoot <- function(x){
  return( as.complex(x)^(1/3)*exp(c(0,2,4)*1i*pi/3) );
}
# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
# cubsol solves analytically the cubic equation and
# returns a list whose first element is the real roots and the
# second element the complex roots.
# test with :
#a = -1; b=-10; c=0; d=50; x=0.01*(-1000:1500); plot(x,a*x^3+b*x^2+c*x+d,t='l'); abline(h=0)
# coefs = c(a,b,c,d)
cubsol <- function(coeffs) {
  if (!(length(coeffs) == 4)){
    stop('Please provide cubsol with a 4-vector of coefficients')
  }
  a = coeffs[1]; b=coeffs[2]; c=coeffs[3]; d=coeffs[4];
  rts = list();  
  
  p <- -b/3/a
  q <- p^3 + (b*c-3*a*(d))/(6*a^2)
  r <- c/3/a
  
  s0 = q^2+(r-p^2)^3; 
  xtemp = as.complex(rep(0,9));    
  if (s0 >= 0){ nReRts=1; } else {nReRts=3; }
  # Now find all the roots in complex space:
  s0 = as.complex(s0);
  s1 = cuberoot(q+s0^0.5)
  s2 = cuberoot(q-s0^0.5);
  xtemp[1:3] <- s1+ s2 +p;  # I think this is meant to always contain
                            # the sure real soln.
  # Second and third solution;
  iSqr3 = sqrt(3)*1i; 
  xtemp[4:6] = p - 0.5*(s1+s2 + iSqr3*(s1-s2));
  xtemp[7:9] = p - 0.5*(s1+s2 - iSqr3*(s1-s2));
  ind1 = which.min(abs(a*xtemp[1:3]^3 + b*xtemp[1:3]^2 +c*xtemp[1:3] +d))
  ind2 = 3+which.min(abs(a*xtemp[4:6]^3 + b*xtemp[4:6]^2 +c*xtemp[4:6] +d))
  ind3 = 6+which.min(abs(a*xtemp[7:9]^3 + b*xtemp[7:9]^2 +c*xtemp[7:9] +d))
  
  if (nReRts == 1){  
    rts[[1]] = c(Re(xtemp[ind1])); 
    rts[[2]] = xtemp[c(ind2,ind3)]
  } else {  # three real roots
    rts[[1]] = Re(xtemp[c(ind1,ind2,ind3)]); 
    rts[[2]] = numeric();
  }
  return(rts)

} # end of function cubsol

# -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
# Function to sharpen or broaden beta beliefs - transform the
# variance of a beta distro to a new one according to a 
# temperature-like param tau 
tbetavar <- function(v,tau){
  Vm = 1/12
  if (v >= Vm){
    stop('tbetavar is meant to work for a,b > 1 so v must be <1/12 ')
  }
  return(v*Vm*tau/(Vm+(tau-1)*v));
}
# - - - - - - - - - - - -  - - - - - - - - - - -
# Find new alpha and beta for a distro that used
# to have a and b, but now has the same mode but a new
# variance v
talphabeta <- function(a,b, v){
  if ((a <= 1) || (b <= 1)){
    stop('in talphabeta, old a and b need to be > 1')
  }
  pm = (a-1)/(a+b-2);  # the mode that we will preserve
  # auxiliaries ...
  c1 = 2*pm-1; 
  c2 = 1-pm;
  c3 = pm + c1; 
  # We will set up a cubic equation to solve, with coefficients:
  coefs = rep(NA,4);
  coefs[1] = v;
  coefs[2] = 2*v*c1  + v*c3 - pm*pm*c2;
  coefs[3] = v*c1*c1 + 2*v*c1*c3 - pm*pm*c1;
  coefs[4] = v*c3*c1*c1; 
  
  newab = data.frame(NA*a,NA*b); 
  colnames(newab) <- c('a','b'); 
  aCuSol = cubsol(coefs);  # Solve the cubic.
  newab$a = aCuSol[[1]][1]; 
  newab$b = (c2*newab$a + c1)/pm ; 
  
  return(newab)
}
# - - - - - - - - - - - -  - - - - - - - - - - -
# Find new alpha and beta for a distro that used
# to have a and b, but now has the same mode but 
# is modified by a temperature tau as per tbetavar 

alphabetatau <- function(a,b,tau){
  v = tbetavar(betavar(a,b),tau);
  return(talphabeta(a,b,v)); 
}
# - - - - - - - - - - - -  - - - - - - - - - - -
