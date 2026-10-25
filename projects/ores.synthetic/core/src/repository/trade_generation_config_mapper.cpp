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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_mapper.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.synthetic.core/repository/trade_generation_config_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.synthetic.api/domain/trade_generation_config.hpp"
#include "ores.synthetic.api/domain/trade_generation_config_json_io.hpp" // IWYU pragma: keep.
#include "ores.synthetic.core/repository/trade_generation_config_entity.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <optional>
#include <vector>

namespace ores::synthetic::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::trade_generation_config
trade_generation_config_mapper::map(const trade_generation_config_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::trade_generation_config r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);


    r.name = v.name;

    r.description = v.description;
    r.asset_class_code = v.asset_class_code;
    r.min_tenor_code = v.min_tenor_code;
    r.max_tenor_code = v.max_tenor_code;
    r.min_notional = ores::utility::decimal::decimal::from_string(v.min_notional).value();
    r.max_notional = ores::utility::decimal::decimal::from_string(v.max_notional).value();
    r.as_of_date = ores::platform::time::datetime::from_iso8601_date(v.as_of_date);
    r.seed = v.seed;
    r.trades_per_book = v.trades_per_book;
    r.enabled = v.enabled;
    r.reference_curve_id = v.reference_curve_id.value_or("");
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

trade_generation_config_entity
trade_generation_config_mapper::map(const domain::trade_generation_config& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    trade_generation_config_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);


    r.name = v.name;

    r.description = v.description;
    r.asset_class_code = v.asset_class_code;
    r.min_tenor_code = v.min_tenor_code;
    r.max_tenor_code = v.max_tenor_code;
    r.min_notional = v.min_notional.to_string();
    r.max_notional = v.max_notional.to_string();
    r.as_of_date = ores::platform::time::datetime::to_iso8601_date(v.as_of_date);
    r.seed = v.seed;
    r.trades_per_book = v.trades_per_book;
    r.enabled = v.enabled;
    r.reference_curve_id =
        v.reference_curve_id.empty() ? std::nullopt : std::optional(v.reference_curve_id);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::trade_generation_config>
trade_generation_config_mapper::map(const std::vector<trade_generation_config_entity>& v) {
    return map_vector<trade_generation_config_entity, domain::trade_generation_config>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<trade_generation_config_entity>
trade_generation_config_mapper::map(const std::vector<domain::trade_generation_config>& v) {
    return map_vector<domain::trade_generation_config, trade_generation_config_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
