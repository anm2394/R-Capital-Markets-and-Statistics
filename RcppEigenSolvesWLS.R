#import Libraries
library("inline")
library("RcppEigen")
library("pracma")
library("microbenchmark")

#defining the vectors and matrices
XX=matrix(seq(from=99,to=105,by=2),ncol=2)
yy=matrix(c(15,20),ncol=1)
ww=c(0.4,0.6)
#Writing the C++/Eigen Script function
CSolvesWLSCpp<-'
using Eigen::MatrixXd;
typedef Eigen::Map<Eigen::VectorXd>   MapVectd;
typedef Eigen::Map<Eigen::MatrixXd>   MapMatd;
const MapVectd  WW(as<MapVectd>(ww));
const MapMatd    X(as<MapMatd>(XX));
const MatrixXd    W(WW.matrix().asDiagonal());
const MapMatd    y(as<MapMatd>(yy));
return List::create(Named("(XTWX)^-1.(XTWy)") = (((X.transpose() * W)* X).inverse())*((X.transpose()*W)*y));
'
#wrapping the script in a R function
CSolvesWLS<-cxxfunction(signature(XX="matrix",ww="vector",yy="matrix"),CSolvesWLSCpp,"RcppEigen")

#Output for CSolvesWLS:
CSolvesWLS(XX,ww,yy)
microbenchmark(CSolvesWLS(XX,ww,yy),times=1000L)

#Writing the R Function:
RSolvesWLS=function(XX,ww,yy){
  w_mat<-matrix(diag(ww),ncol=length(ww))
  Xt=t(XX)
  return(inv(Xt%*%w_mat%*%XX)%*%(Xt%*%w_mat%*%yy))
 }
#Output for RSolvesWLS:
RSolvesWLS(XX,ww,yy)
microbenchmark(RSolvesWLS(XX,ww,yy),times=1000L)
