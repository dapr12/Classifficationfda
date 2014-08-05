#' pcafd - Functional Principal components analysis aims to display types of variation across a sample of
#' functions. Principal components analysis is an exploratory data analysis that tends to be an early
#' part of many projects.
#' @param fdaobject A functional Data Object 
#' @param nharm the number of harmonics or principal components to compute
#' @return A list containg the following components: 
#'\itemize{
#'\item{Harmonics}{ A functional data object for the harmonics or eigenfunctions.}
#'\item{values}{ The complete set of eigenvalues.}
#'\item{scores}{ A matrix of scores on the principal components or harmonics.}
#'\item{varprop}{A vector giving the proportion of variance explained by each eigenfunction.}
#'}
#' @references  Ramsay, James O., and Silverman, Bernard W. (2002), Applied Functional Data Analysis, Springer, New York.
#' @examples \dontrun{
#' temppcaobj <- pcafd( fdaobjMale, nharm=3)
#' temppcaobj <- pcafd( fdaobjMale, nharm=2, plotting=TRUE)
#' }
#' @import 'fda'
#' @export
pcafd<- function ( fdaobj, nharm, plotting=FALSE) 
{
  
  if (!(inherits(fdaobj, "FdaClass"))) 
    stop("Argument FD  not a functional data object.")
  
  data<- fdaobj$data
  argvals<- fdaobj$argvals
  rangevals<- fdaobj$rangeval
  Data2FD <- Data2fd( argvals, data)
  PCA <- pca.fd(Data2FD, nharm, centerfns =FALSE)
  Harmnonics<-PCA$harmonics$coefs
  Scores<- PCA$scores
  values<- PCA$values
  varprop<- PCA$varprop
  if(plotting== TRUE)
  { plot(PCA)}
  pcaFD<-list(Harmonics = Harmnonics, 
              Scores= Scores, Values= values, VarProp=varprop )
  
  class(pcaFD) <- "PcaClass"
  return(pcaFD)
  
  
}

  
  