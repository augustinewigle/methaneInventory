#include <Rcpp.h>
using namespace Rcpp;

// ' Create variance matrix for strata in cpp
// '
// [[Rcpp::export]]
NumericMatrix calc_strat_matrix(CharacterVector facility_ids,
                                NumericVector psu_means,
                                double nh,
                                double Nh,
                                int dimension) {

  NumericMatrix return_mat(dimension, dimension);
  double temp;
  double nhNh = nh/Nh;

  for(int i = 0; i < (dimension); ++i) {

    for(int j = 0; j < (dimension); ++j) {

       if(facility_ids[i] == facility_ids[j]) {

            temp = nhNh;

          } else {

            temp = (nh-1)*nh/(Nh-1)/Nh;

          }

       return_mat(i,j) = (temp-pow(nhNh, 2))/temp*psu_means[i]*psu_means[j]*pow(1/nhNh, 2);

    }

  }

  return(return_mat);

}
