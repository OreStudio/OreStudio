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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_STALENESS_POLICY_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_STALENESS_POLICY_HPP

#include "ores.analytics.quant/domain/rate_status.hpp"
#include <chrono>

namespace ores::analytics::quant::domain {

/// Caller-supplied tolerance for how old a contributing driver may be
/// before a derived rate is considered stale, and how much older still
/// before it's considered disconnected outright. The engine never fetches
/// this from anywhere -- it is a plain parameter, per this library's
/// no-refdata-coupling rule.
///
/// @c disconnected_after defaults to "never" (duration::max()) so a
/// single-threshold caller (existing tests, risk_recentering) keeps its
/// original fresh/stale-only behaviour without change.
struct staleness_policy {
    std::chrono::system_clock::duration stale_after;
    std::chrono::system_clock::duration disconnected_after =
        std::chrono::system_clock::duration::max();

    [[nodiscard]] rate_status evaluate(std::chrono::system_clock::duration age) const {
        if (age <= stale_after)
            return rate_status::fresh;
        if (age <= disconnected_after)
            return rate_status::stale;
        return rate_status::disconnected;
    }
};

} // namespace ores::analytics::quant::domain

#endif
