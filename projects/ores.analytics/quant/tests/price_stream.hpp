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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_ANALYTICS_QUANT_TESTS_PRICE_STREAM_HPP
#define ORES_ANALYTICS_QUANT_TESTS_PRICE_STREAM_HPP

#include "ores.analytics.quant/math/rng_stream.hpp"
#include <cstdint>

/**
 * @brief A price process's stream for a test, keyed by an explicit seed.
 *
 * Two tests with one seed draw the same path, and two seeds draw two.
 */
inline ores::analytics::quant::math::rng_stream price_stream(std::uint64_t seed) {
    using namespace ores::analytics::quant::math;
    return rng_stream(
        stream_key{.run_seed = seed, .series = series_key("test"), .purpose = draw_purpose::price});
}

#endif
