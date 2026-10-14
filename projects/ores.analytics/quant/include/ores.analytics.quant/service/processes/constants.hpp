/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_CONSTANTS_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_CONSTANTS_HPP

#include <cmath>
#include <limits>

namespace ores::analytics::quant::service {

/**
 * @brief Mean-reversion speed below which a process takes its small-kappa
 * limit instead of dividing by kappa.
 *
 * Mirrors QuantLib's OrnsteinUhlenbeckProcess::variance() branch point: a
 * kappa that is positive but too small to divide by safely still needs the
 * algebraic limit, not only the kappa == 0 case.
 *
 * The process files each defined their own copy in an anonymous namespace. A
 * unity build concatenates those files into one translation unit, which merges
 * the unnamed namespaces and rejects the repeated name, so the definition
 * lives here instead. Further constants the process layer shares belong here
 * as well.
 */
// std::sqrt is not constexpr in this toolchain, so the constant cannot be
// constexpr; inline keeps it a single public definition across the library.
inline const double small_kappa_threshold = std::sqrt(std::numeric_limits<double>::epsilon());

}

#endif
