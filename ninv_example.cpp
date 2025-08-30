#include <cassert>
#include <cinttypes>
#include <chrono>
#include <iostream>
#include <boost/multiprecision/gmp.hpp>

// To jest deklaracja testowanej funkcji.
extern "C" void ninv(uint64_t *Y, uint64_t const *X, unsigned n);

using std::cout;
using std::cerr;
using std::stol;
using std::chrono::system_clock;
using std::chrono::duration_cast;
using std::chrono::milliseconds;
using uintn_t = boost::multiprecision::mpz_int;

namespace {
  // Funkcje konwertujące są jawnie podane, aby pokazać,
  // jakiego kodowania liczb wymaga funkcja ninv.

  template<typename T>
  void convert2bin(T in, uint64_t *out, size_t n) {
    for (size_t i = 0; i < n; ++i) {
      out[i] = (uint64_t)(in & UINT64_MAX);
      in >>= 64;
    }
  }

  template<typename T>
  void convert2boost(T &out, uint64_t const *in, size_t n) {
    out = 0;
    for (size_t i = n; i-- > 0;) {
      out <<= 64;
      out += in[i];
    }
  }
}

int main(int argc, char *args[]) {
  if (argc != 3) {
    cerr << "Usage:\n"
         << args[0] << " n X\n"
         << "Examples:\n"
         << args[0] << " 64 2\n"
         << args[0] << " 128 1000000\n"
         << args[0] << " 256 0x12ab\n"
         << args[0] << " 640 -1\n";
    return 1;
  }

  long k = stol(args[1]);
  unsigned n = k;
  uintn_t X(args[2]), X_after, Y, N = uintn_t(1) << n;

  if (X < 0)
    X += N;

  assert(k % 64 == 0 && k >= 64 && k <= 256000);
  assert(X > 1 && X < N);

  cout << "n = " << n << "\n" << "x = " << X << "\n";

  uint64_t *x = new uint64_t[n/64], *y = new uint64_t[n/64];

  convert2bin(X, x, n/64);

  auto begin = system_clock::now();

  ninv(y, x, n);

  auto end = system_clock::now();

  convert2boost(Y, y, n/64);
  convert2boost(X_after, x, n/64);

  delete[] x;
  delete[] y;

  cout << "y = " << Y << "\n"
       << duration_cast<milliseconds>(end - begin).count() << " ms\n";

  assert(X == X_after);
  assert(Y > 0 && X * Y <= N && N < X * (Y + 1));
}
