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
#ifndef ORES_COMPUTE_API_MESSAGING_CLAIM_PACING_HPP
#define ORES_COMPUTE_API_MESSAGING_CLAIM_PACING_HPP

#include <algorithm>
#include <chrono>

namespace ores::compute::messaging {

/**
 * @brief The wait before a node asks for work again after a claim that found none.
 *
 * Every idle node polls, and a notice also wakes them all at once. A fixed interval
 * would keep the nodes in step and send their requests together, so the wait is spread
 * by up to a quarter either side of the base. The notice makes the common case fast and
 * the poll is the fallback, so the base can be several seconds.
 *
 * @param base The poll interval.
 * @param unit A random number from 0 up to but not including 1, from the caller, so
 *        the spread can be tested without a random source.
 */
inline std::chrono::milliseconds poll_delay(std::chrono::milliseconds base, double unit) {
    const double factor = 0.75 + 0.5 * std::clamp(unit, 0.0, 1.0);
    return std::chrono::milliseconds(
        static_cast<long long>(static_cast<double>(base.count()) * factor));
}

/**
 * @brief The wait before a node tries again after a claim request that failed.
 *
 * The wait doubles with each failure in a row, from one second to a limit of thirty,
 * and is spread like the poll. A service that is down or slow is then asked less and
 * less often, and the nodes do not retry in step when it returns.
 *
 * @param failures The number of failures in a row, at least one.
 * @param unit A random number from 0 up to but not including 1.
 */
inline std::chrono::milliseconds error_delay(int failures, double unit) {
    constexpr long long cap_ms = 30'000;
    const int shift = std::clamp(failures, 1, 6) - 1;
    const long long base_ms = std::min(cap_ms, 1'000LL << shift);
    const double factor = 0.75 + 0.5 * std::clamp(unit, 0.0, 1.0);
    return std::chrono::milliseconds(
        std::min(cap_ms, static_cast<long long>(static_cast<double>(base_ms) * factor)));
}

} // namespace ores::compute::messaging

#endif
