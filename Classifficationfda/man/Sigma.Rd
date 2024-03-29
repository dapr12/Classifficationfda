% Generated by roxygen2 (4.0.1.99): do not edit by hand
\name{Sigma}
\alias{Sigma}
\title{Performs functional discrimination of a sample of curves when a categorical response is observed (supervised classification).
A local bandwidth (i.e. local number of neighbours) is selected by a cross-validation procedure.}
\usage{
Sigma(x, y)
}
\arguments{
\item{Classes}{vector containing the categorical responses giving the group number for each}

\item{"CURVES"}{matrix containing the curves dataset (row by row) used for the estimating stage}

\item{"PRED"}{matrix containing new curves stored row by row used for computing predictions}

\item{..."}{arguments needed for the call of the function computing the semi-metric between curves}

\item{"kernel"}{the kernel function used for computing of the kernel estimator"quadratic (default)}

\item{"semimetric"}{character string allowing to choose the function computing the semimetric;  you can select "mplsr"}
}
\value{
A component list with:
\itemize{
\item{"Estimated.classnumber""}{vector containing estimated class membership for each curve of "CURVES}
\item{"Predicted.classnumber"}{if PRED different from CURVES, this vector contains predicted class membership for each curve of PRED}
\item{"Bandwidths"}{vector containing the local data-driven bandwidths for each curve in the matrix "CURVES"}
\item{"Misclas"}{misclassification rate computed from estimated values and observed values}
}
}
\description{
Performs functional discrimination of a sample of curves when a categorical response is observed (supervised classification).
A local bandwidth (i.e. local number of neighbours) is selected by a cross-validation procedure.
}
\examples{
\dontrun{
tr <- sample(1:100, 50)
AllGwdprotein<-cbind(Gwdprotein$Group1,Gwdprotein$Group2)
train<- AllGwdprotein[,tr]
test<- AllGwdprotein[,-tr]
cl <- factor(c(rep("G1",50), rep("G2",50)))
Classlearn <- sort(rep(1:2,25))
Classifmplsr <- funopadi.knn.lcv(Classlearn, t(train), t(test), 1,kernel = "quadratic",semimetric="mplsr")
}
}
\references{
Ramsay, James O., and Silverman, Bernard W. (2006), Functional Data Analysis, 2nd ed., Springer, New York.
}

