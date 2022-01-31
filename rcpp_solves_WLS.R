library(Rcpp)
library(RcppEigen)
library(pracma)
X<-matrix(1:4,ncol=2)
y<-matrix(c(1,2),ncol=1)
w<-c(0.5,0.5)
#Creating function for cpp to solve the multiplication
CSolvesWLS=function(X,y,w){
  
  w_mat<-matrix(diag(w),ncol=length(w))
Xt=t(X)
w_mat<-matrix(diag(w),ncol=length(w))
cppFunction('NumericMatrix mmult(const NumericMatrix& m1, const NumericMatrix& m2){
if (m1.ncol() != m2.nrow()) stop ("Incompatible matrix dimensions");
NumericMatrix out(m1.nrow(),m2.ncol());
NumericVector rm1, cm2;
for (size_t i = 0; i < m1.nrow(); ++i) {
    rm1 = m1(i,_);
    for (size_t j = 0; j < m2.ncol(); ++j) {
      cm2 = m2(_,j);
      out(i,j) = std::inner_product(rm1.begin(), rm1.end(), cm2.begin(), 0.);              
    }
  }
return out;
}')

Xt_w=mmult(Xt,w_mat)
Xt_w_X=mmult(Xt_w,X)
Xt_w_y=mmult(Xt_w,y)
mmult(inv(Xt_w_X),mmult(Xt_w,y))
}
CSolvesWLS(X,y,w)
#Solving through R's built in
RSolvesWLS=function(X,y,w){
w_mat<-matrix(diag(w),ncol=length(w))
Xt=t(X)
b=t(X)%*%w_mat%*%X
c=t(X)%*%w_mat%*%y
beta1=inv(b)%*%c
return(beta1)
}
RSolvesWLS(X,y,w)
microbenchmark(RSolvesWLS(X,y,w))
#timecomparisons
microbenchmark(CSolvesWLS(X,y,w))
microbenchmark(RSolvesWLS(X,y,w))
#timecomparisons for functions
w_mat<-matrix(diag(w),ncol=length(w))
Xt=t(X)
microbenchmark(mmult(Xt,w_mat))
microbenchmark(Xt%*%w_mat)
cppFunction('NumericMatrix mmult(const NumericMatrix& m1, const NumericMatrix& m2){
if (m1.ncol() != m2.nrow()) stop ("Incompatible matrix dimensions");
NumericMatrix out(m1.nrow(),m2.ncol());
NumericVector rm1, cm2;
for (size_t i = 0; i < m1.nrow(); ++i) {
    rm1 = m1(i,_);
    for (size_t j = 0; j < m2.ncol(); ++j) {
      cm2 = m2(_,j);
      out(i,j) = std::inner_product(rm1.begin(), rm1.end(), cm2.begin(), 0.);              
    }
  }
return out;
}')