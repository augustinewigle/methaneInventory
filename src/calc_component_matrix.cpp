#include <Rcpp.h>
using namespace Rcpp;

// ' Create variance matrix for components in cpp
// '
// [[Rcpp::export]]
 NumericMatrix calc_component_matrix(NumericVector p,
                                     NumericVector Ytrue,
                                     double phi,
                                     int n_pass) {

   NumericMatrix return_mat(n_pass,n_pass);

   for(int i = 0; i < (n_pass); ++i) {

     for(int j = 0; j < (n_pass); ++j) {

       if(i == j) {

         return_mat(i,j) = pow(phi/p[i], 2)*(1-p[i]/phi)*pow(Ytrue[i],2);

       } else {

         return_mat(i,j) = phi*(1-1/phi)*Ytrue[i]*Ytrue[j]*phi/(p[i]*p[j]);

       }

     }

   }

   return(return_mat);

 }
