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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_RATE_RECIPROCATOR_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_RATE_RECIPROCATOR_HPP

#include "ores.analytics.quant/domain/crm_rate_view.hpp"
#include "ores.analytics.quant/domain/derived_rate.hpp"
#include "ores.analytics.quant/export.hpp"
#include <map>
#include <string>
#include <utility>
#include <vector>

namespace ores::analytics::quant::service {

/**
 * @brief Resolves a requested (base, quote) pair against a batch of
 * already-computed @c derived_rate results, falling back to the reverse
 * pair's reciprocal (1/rate) when no direct entry exists for the requested
 * pair -- e.g. a CRM configured with EUR/CAD but not CAD/EUR.
 *
 * Pure/stateless: no dependency on @c rate_engine, NATS, or any refdata
 * convention type. A caller (e.g. ores.marketdata's crm_handler) builds a
 * lookup once per batch via @c make_lookup, then calls @c resolve per
 * requested cell.
 */
class ORES_ANALYTICS_QUANT_EXPORT rate_reciprocator {
public:
    using rate_lookup = std::map<std::pair<std::string, std::string>, domain::derived_rate>;

    /// Builds a (base_code, quote_code) -> derived_rate lookup from a
    /// batch, e.g. the result of @c rate_engine::rates(). Later entries
    /// for the same pair overwrite earlier ones.
    [[nodiscard]] static rate_lookup make_lookup(const std::vector<domain::derived_rate>& rates);

    /// Resolves @p base_code / @p quote_code against @p lookup:
    ///  - a direct entry present in @p lookup is returned unchanged
    ///    (reciprocal = false), regardless of its status;
    ///  - absent a direct entry, if @p allow_reciprocal and the reverse pair
    ///    is present, returns its reciprocal (rate = 1/rate when the reverse
    ///    rate is finite and non-zero; otherwise the reverse's own
    ///    status/as_of are kept and rate is left at 0.0 to avoid a
    ///    division by zero);
    ///  - otherwise returns an unavailable view with a default-constructed
    ///    as_of.
    [[nodiscard]] static domain::crm_rate_view resolve(const std::string& base_code,
                                                       const std::string& quote_code,
                                                       const rate_lookup& lookup,
                                                       bool allow_reciprocal);
};

} // namespace ores::analytics::quant::service

#endif
