#' Bsplines- Computes the functional spatial median of the functional data 
#' @param fdaobj An object of the FDA Class.
#' #' @param fdaobj An object of the FDA Class.
#' #' @param fdaobj An object of the FDA Class.
#' #' @param fdaobj An object of the FDA Class.
#' @return A component list with: 
#'\itemize{
#'\item{"argvals "}{}
#'\item{"fdSmooth"}{}
#'\item{"df"}{}
#'\item{"SSE"}{}
#'\item{"PenMat"}{}
#'\item{"GCV"}{}
#'}
#' @references  Ramsay, James O., and Silverman, Bernard W. (2006), Functional Data Analysis, 2nd ed., Springer, New York.
#'              Ramsay, James O., and Silverman, Bernard W. (2002), Applied Functional Data Analysis, Springer,New York.
#' @examples \dontrun{
#' norder<-6
#' lambda <- 0.01
#' Lf<-4
#' fdaobj<-fdaobjMale
#' BSplinesGrowth <-bsplines( fdaobj, norder, lambda, Lf )
#' matplot(BSplinesGrowth$fdSmooth$coefs, type="l",main="Smooth Data using B-Splines",xlab="Age (years)", ylab="Heigth (cm)")
#' }
#' @import 'fda'
#' @export
bsplines <- function ( fdaobj, norder, lambda, Lf )
{
  
  if (!(inherits(fdaobj, "FdaClass"))) 
    stop("Argument FD  not a functional data object.")
  
  argvals<- fdaobj$argvals
    
  rangevals<- fdaobj$rangeval
  
  data<-as.matrix((fdaobj$data))
  
  norder<- norder
  
  nbasis <- length(argvals) + norder - 2
  
  lambda<- lambda
  
  basis = create.bspline.basis(rangevals, nbasis, norder, argvals)
  
  fdPar  = fdPar(basis, Lf, lambda)
  
  fdSmooth = smooth.basis(argvals, data, fdPar)
  
  result<- list( argvals = fdaobj$argvals,  fdSmooth= fdSmooth$fd, df = fdSmooth$df,
                 SSE= fdSmooth$SSE, PenMat= fdSmooth$penmat, GCV= fdSmooth$gcv) 
  
  return(result)
  
}


