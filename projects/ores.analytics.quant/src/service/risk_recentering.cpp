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
#include "ores.analytics.quant/service/risk_recentering.hpp"
#include "ores.analytics.quant/domain/ccy_pair_input.hpp"
#include "ores.analytics.quant/domain/currency_id.hpp"
#include "ores.analytics.quant/domain/driver_quote.hpp"
#include "ores.analytics.quant/domain/rate_status.hpp"
#include "ores.analytics.quant/service/topology_builder.hpp"
#include <stdexcept>

namespace ores::analytics::quant::service {

using domain::ccy_pair_input;
using domain::currency_id;
using domain::driver_quote;
using domain::rate_status;

rate_engine recenter(const rate_engine& source,
                     const std::string& aggregation_code,
                     domain::staleness_policy policy,
                     std::chrono::system_clock::time_point now) {
    const auto& source_topology = source.topology();
    if (!source_topology.currency_id_for(aggregation_code)) {
        throw std::invalid_argument("recenter: unknown aggregation currency " + aggregation_code);
    }

    std::vector<std::string> currency_codes;
    for (std::size_t v = 0; v < source_topology.vertex_count(); ++v) {
        const currency_id id(static_cast<std::uint16_t>(v));
        auto code = source_topology.code_of(id);
        if (code != aggregation_code)
            currency_codes.push_back(std::move(code));
    }

    std::vector<ccy_pair_input> star_edges;
    star_edges.reserve(currency_codes.size());
    for (const auto& code : currency_codes)
        star_edges.push_back({aggregation_code, code, true});

    auto star_topology = topology_builder::build(star_edges, aggregation_code, currency_codes);
    rate_engine recentred(std::move(star_topology), policy);

    for (const auto& code : currency_codes) {
        const auto current = source.rate(aggregation_code, code, now);
        if (current.status == rate_status::unavailable)
            continue;
        recentred.update(driver_quote{aggregation_code, code, current.rate, current.as_of});
    }

    return recentred;
}

} // namespace ores::analytics::quant::service
