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
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.platform/numeric/floating_point.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::marketdata::service::app {

std::expected<std::shared_ptr<const named_datum>, tick_drop> name_datum(std::string_view uri) {
    auto datum = datum::oresmd_uri_codec::read(uri);
    if (!datum)
        return std::unexpected(tick_drop::unnameable_datum);
    // The codec reads only a datum's one spelling, so the URI it writes is the tick's own and
    // the subject is the datum's alone. A datum needs no ORE key.
    auto canonical = datum::oresmd_uri_codec::write(*datum);
    if (!canonical)
        return std::unexpected(tick_drop::unnameable_datum);
    auto series = datum::oresmd_uri_codec::write(datum::series_of(*datum));
    if (!series)
        return std::unexpected(tick_drop::unnameable_datum);
    return std::make_shared<const named_datum>(
        named_datum{std::move(*datum), std::move(*canonical), std::move(*series)});
}

std::shared_ptr<const std::vector<tick_target>>
make_targets(const std::vector<domain::feed_binding>& bindings, const named_datum& named) {
    auto targets = std::make_shared<std::vector<tick_target>>();
    targets->reserve(bindings.size());
    for (const auto& b : bindings) {
        auto tenant = b.tenant_id.to_string();
        auto party = boost::uuids::to_string(b.party_id);
        auto subject = domain::market_tick_subject(tenant, party, named.uri);
        targets->push_back({b, std::move(subject), std::move(tenant), std::move(party)});
    }
    return targets;
}

std::expected<tick_plan, tick_drop>
plan_tick(const messaging::market_tick& tick,
          const std::vector<domain::feed_binding>& bindings,
          std::shared_ptr<const named_datum> named,
          std::shared_ptr<const std::vector<tick_target>> targets) {
    if (bindings.empty())
        return std::unexpected(tick_drop::unbound_source);

    if (!named) {
        auto read = name_datum(tick.oresmd_uri);
        if (!read)
            return std::unexpected(read.error());
        named = std::move(*read);
    }

    const auto value = ores::platform::numeric::parse_double(tick.value);
    if (!value)
        return std::unexpected(tick_drop::not_a_number);

    if (!targets)
        targets = make_targets(bindings, *named);
    return tick_plan{std::move(named), *value, std::move(targets)};
}

}
