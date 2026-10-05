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
#include "ores.marketdata.service/app/tick_plan.hpp"
#include "ores.marketdata.api/domain/tick_subjects.hpp"
#include "ores.marketdata.core/datum/ore_key_codec.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.platform/numeric/floating_point.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::marketdata::service::app {

std::expected<tick_plan, tick_drop> plan_tick(const messaging::market_tick& tick,
                                              const std::vector<domain::feed_binding>& bindings) {
    if (bindings.empty())
        return std::unexpected(tick_drop::unbound_source);

    auto tick_datum = datum::oresmd_uri_codec::read(tick.oresmd_uri);
    if (!tick_datum)
        return std::unexpected(tick_drop::unnameable_datum);
    const auto ore_key = datum::ore_key_codec::write(*tick_datum);
    if (!ore_key)
        return std::unexpected(tick_drop::unnameable_datum);

    const auto value = ores::platform::numeric::parse_double(tick.value);
    if (!value)
        return std::unexpected(tick_drop::not_a_number);

    tick_plan plan{std::move(*tick_datum), *value, {}};
    plan.targets.reserve(bindings.size());
    for (const auto& b : bindings)
        plan.targets.push_back({b,
                                domain::market_tick_subject(b.tenant_id.to_string(),
                                                            boost::uuids::to_string(b.party_id),
                                                            *ore_key)});
    return plan;
}

}
