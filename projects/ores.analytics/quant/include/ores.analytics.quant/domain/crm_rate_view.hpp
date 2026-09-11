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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_CRM_RATE_VIEW_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_CRM_RATE_VIEW_HPP

#include "ores.analytics.quant/domain/rate_status.hpp"
#include <chrono>
#include <optional>
#include <string>

namespace ores::analytics::quant::domain {

/// A rate resolved for display: the requested (base, quote) pair, with the
/// reciprocal and %-change-vs-previous-observation logic already applied.
/// Callers (NATS handlers, HTTP, shell, Wt) map this straight onto their
/// own wire type; nothing downstream re-derives the reciprocal or deltas.
struct crm_rate_view {
    std::string base_code;
    std::string quote_code;
    /// Only meaningful when @c status != rate_status::unavailable. For a
    /// reciprocal view this is already 1/rate -- callers never re-reciprocate.
    double rate = 0.0;
    rate_status status = rate_status::unavailable;
    std::chrono::system_clock::time_point as_of;
    /// True when no direct quote existed for (base_code, quote_code) and
    /// this value was computed as 1/rate from the reverse pair.
    bool reciprocal = false;
    /// %-change vs. the last value served for this exact (base, quote)
    /// pair, as displayed (post-reciprocal). std::nullopt when there is no
    /// prior observation to compare against, or the current/previous
    /// value isn't a valid rate to diff.
    std::optional<double> delta_pct;
};

} // namespace ores::analytics::quant::domain

#endif
