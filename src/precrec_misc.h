#ifndef PRECREC_MISC_H_
#define PRECREC_MISC_H_

#include <Rcpp.h>
#include <cfloat>       // DBL_MIN, DBL_MAX
#include <vector>       // std::vector
#include <string>       // std::string

//
// Shuffle int vector
//
void shuffle_intvec(std::vector<int>::iterator first,
                    std::vector<int>::iterator last,
                    int (*gen)(const int));

#endif /* PRECREC_MISC_H_ */

