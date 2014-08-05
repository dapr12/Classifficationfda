#'Performs functional discrimination of a sample of curves when a categorical response is observed (supervised classification). 
#'A local bandwidth (i.e. local number of neighbours) is selected by a cross-validation procedure.
#' @param Classes vector containing the categorical responses giving the group number for each 
# curve in the matrix CURVES (if nbclass is the number of groups, "Classes" contains numbers 1,2,...,nbclass)
#' @param "CURVES" matrix containing the curves dataset (row by row) used for the estimating stage
#' @param "PRED" matrix containing new curves stored row by row used for computing predictions
#' @param ..." arguments needed for the call of the function computing the semi-metric between curves
#' @param "kernel" the kernel function used for computing of the kernel estimator"quadratic (default)
#' @param "semimetric" character string allowing to choose the function computing the semimetric;  you can select "mplsr"
#' @return A component list with: 
#'\itemize{
#' \item{"Estimated.classnumber""}{vector containing estimated class membership for each curve of "CURVES}
#' \item{"Predicted.classnumber"}{if PRED different from CURVES, this vector contains predicted class membership for each curve of PRED} 
#' \item{"Bandwidths"}{vector containing the local data-driven bandwidths for each curve in the matrix "CURVES"}
#' \item{"Misclas"}{misclassification rate computed from estimated values and observed values}
#'}
#' @references  Ramsay, James O., and Silverman, Bernard W. (2006), Functional Data Analysis, 2nd ed., Springer, New York.
#' @examples \dontrun{
#' tr <- sample(1:100, 50)
#' AllGwdprotein<-cbind(Gwdprotein$Group1,Gwdprotein$Group2)
#' train<- AllGwdprotein[,tr]
#' test<- AllGwdprotein[,-tr]
#' cl <- factor(c(rep("G1",50), rep("G2",50)))
#' Classlearn <- sort(rep(1:2,25)) 
#' Classifmplsr <- funopadi.knn.lcv(Classlearn, t(train), t(test), 1,kernel = "quadratic",semimetric="mplsr")
#' }
#' @import 'MASS'
#' @export
Sigma<-function( x, y ){
  
  Sigma<-matrix(rep(0,length(x)*length(y)),nrow=length(x))
  
  for(i in 1:nrow(Sigma)){
    
    for (j in 1:ncol(Sigma)){ 
      
      Sigma[i,j]<-exp(-1/2*(abs(x[i]-y[j]))^2)
    }
    
  }
  
  return(Sigma)
}

simulatefda<-function ( nsamples, ndrawn, rangeval , mean , sigma )
  {
  
  samples<- nsamples
  
  nval<-4 ## Need to change
  
  sn<- sigma
  
  mn<- mean
  
  valmax<-rangeval[2]
  
  valmin<-rangeval[1]
    
  x.star<-seq(valmin,valmax,l=ndrawn)
  
  y<-rnorm(nval,mn,0)
  
  f <-data.frame(x.star,y)
  
  k.xx <- Sigma(f$x,f$x)
  
  k.xxs <- Sigma(f$x,x.star)
  
  k.xsx <- Sigma(x.star,f$x)
  
  k.xsxs <- Sigma(x.star,x.star)
  
  f.bar.star <-k.xsx%*%solve(k.xx+sn^2*diag(1,ncol(k.xx)))%*%f$y
  
  cov.f.star <-k.xsxs-k.xsx%*%solve(k.xx+sn^2*diag(1,ncol(k.xx)))%*%k.xxs
  
  values<-matrix(rep(0,length(x.star)*samples),ncol=samples)
  
  for (i in 1:samples){ 
        
    values[,i]<-mvrnorm(1,f.bar.star,cov.f.star)
    
  }
  
  list( rangetime= x.star, simulation = values  ) 
  
}


