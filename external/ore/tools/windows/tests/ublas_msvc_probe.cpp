// Regression for boostorg/ublas#77: including sparse vectors must preserve
// the MSVC iterator macro before another standard header uses its value.
#include <vector>
#include <boost/numeric/ublas/vector_sparse.hpp>
#include <span>

static_assert(_ITERATOR_DEBUG_LEVEL == 0);
static_assert(std::span<int>::extent == std::dynamic_extent);
