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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_DERIVED_RATE_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_DERIVED_RATE_HPP

#include "ores.analytics.quant/domain/rate_status.hpp"
#include <chrono>
#include <string>

namespace ores::analytics::quant::domain {

/// A single rate served by @c rate_engine::rate / @c rate_engine::rates --
/// direct or triangulated, the caller cannot tell the difference from this
/// type alone (deliberately: the whole point of the CRM is that callers
/// don't need to know).
struct derived_rate {
    std::string base_code;
    std::string quote_code;
    /// Only meaningful when @c status != rate_status::unavailable.
    double rate;
    rate_status status;
    /// The oldest contributing driver tick's timestamp on the path between
    /// base and quote -- a chain is only as fresh as its stalest link.
    std::chrono::system_clock::time_point as_of;
};

} // namespace ores::analytics::quant::domain

#endif
