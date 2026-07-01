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
#include "ores.marketdata.core/repository/market_fixing_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.marketdata.api/domain/market_fixing_json_io.hpp" // IWYU pragma: keep.
#include "ores.platform/time/datetime.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <format>
#include <sstream>

namespace ores::marketdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::market_fixing market_fixing_mapper::map(const market_fixing_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::market_fixing r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);

    r.series_id = boost::lexical_cast<boost::uuids::uuid>(v.series_id);

    {
        int yy{}, mm{}, dd{};
        char s1{}, s2{};
        std::istringstream ss(v.fixing_date);
        ss >> yy >> s1 >> mm >> s2 >> dd;
        r.fixing_date = std::chrono::year{yy} / std::chrono::month{static_cast<unsigned>(mm)} /
                        std::chrono::day{static_cast<unsigned>(dd)};
    }

    r.value = v.value;
    r.source = v.source.value_or("");
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

market_fixing_entity market_fixing_mapper::map(const domain::market_fixing& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    market_fixing_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);

    r.series_id = boost::uuids::to_string(v.series_id);

    r.fixing_date = std::format("{:%Y-%m-%d}", v.fixing_date);

    r.value = v.value;
    r.source = v.source.empty() ? std::nullopt : std::optional(v.source);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::market_fixing>
market_fixing_mapper::map(const std::vector<market_fixing_entity>& v) {
    return map_vector<market_fixing_entity, domain::market_fixing>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<market_fixing_entity>
market_fixing_mapper::map(const std::vector<domain::market_fixing>& v) {
    return map_vector<domain::market_fixing, market_fixing_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
