#' CVSelect - Select the Cross-Validation Bandwith described in (Foster, and ) for the Median of the PSE funcion based on Functional Data
#' @param bandwith 
#' @param x Location of the discretization points. THis discretization points must be uniform and missing values are not accepted.  
#' @param y Typically a matrix or data frame which contains a set of curves stored in rows. Missing values are not accepte. 
#' @param degree Degree of the local Polynomial to be used. If Degree is missing takes by default degree = 1.
#' @return A bandwith that minimizes the Median of the Median PSE. 
#' @references  Peter Foster PhD Thesis. University of Manchester 
#' @examples \dontrun{
#' Mat<- fdaobjMale$data
#' h<- cv.select(c(0,10), 1:31,t(Mat),1)
#' }
#' @export
hselect <- function(x, y, degree, interval = NULL, ...) 
{     
  if (is.null(interval)) {
    rangex   <- diff(range(x))
    meshx    <- rangex / (length(x) - 1)
    interval <- c( ifelse(degree < 2, meshx / 2, meshx), rangex / 2)
  }
  
  optimize(medianPSE, interval, x = x, y = y, degree = degree, ...)$minimum 
}
