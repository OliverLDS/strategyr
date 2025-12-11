#ifndef UTILSMATH_H
#define UTILSMATH_H

#include <cmath>

// Utility: sigmoid function
inline double sigmoid(double z) {
  return 1.0 / (1.0 + std::exp(-z));
}


// Utility: clamp function
inline double clamp(double x, double lo, double hi) {
  return (x < lo) ? lo : (x > hi ? hi : x);
}

#endif