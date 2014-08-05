#' densityScores
#' @param pcaobj PcaClass Object
#' @param nscores Number of Scores 
#' @return A vector with the estimate density values  
#' @references  Delaigle, A. and Hall, P. (2010). Defining probability density for a distribution of random functions. Ann. Statist., 38, 1171-1193.
#' @examples \dontrun{
#' fdvector<-fdensity( fdaobjMale, pcaobj, bandwith=800)
#' plot(fdvector, type="p")
#' }
#' @import 'ks'
#' @export
densityScores<-function ( pcaobj, nscores){
  
  if (!(inherits(pcaobj, "PcaClass"))) 
    stop("Argument FD  not a functional data object.")
  
  Scores<- pcaobj$Scores
  
  if( nscores == 1)
  {
    fhat<-kde(pcaobj$Scores[,1], positive=TRUE)
    plot(fhat)
  }
  
  if( nscores == 2)
  {
  
    fhat<- kde(pcaobj$Scores[,1:2], positive=TRUE)
    plot(fhat)
  
  }

}



