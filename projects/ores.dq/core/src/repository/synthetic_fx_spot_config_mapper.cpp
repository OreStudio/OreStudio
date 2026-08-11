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
#include "ores.dq.core/repository/synthetic_fx_spot_config_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.dq.api/domain/synthetic_fx_spot_config_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::dq::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::synthetic_fx_spot_config
synthetic_fx_spot_config_mapper::map(const synthetic_fx_spot_config_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::synthetic_fx_spot_config r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.name = v.name;
    r.description = v.description;
    r.enabled = v.enabled;
    r.auto_start = v.auto_start;
    r.base_currency_code = v.base_currency_code;
    r.quote_currency_code = v.quote_currency_code;
    r.gmm_initial_price = v.gmm_initial_price;
    r.ticks_per_hour = v.ticks_per_hour;
    r.process_type = v.process_type;
    r.price_source = v.price_source;
    r.vintage_source = v.vintage_source;
    r.vintage_date = v.vintage_date;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

synthetic_fx_spot_config_entity
synthetic_fx_spot_config_mapper::map(const domain::synthetic_fx_spot_config& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    synthetic_fx_spot_config_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.name = v.name;
    r.description = v.description;
    r.enabled = v.enabled;
    r.auto_start = v.auto_start;
    r.base_currency_code = v.base_currency_code;
    r.quote_currency_code = v.quote_currency_code;
    r.gmm_initial_price = v.gmm_initial_price;
    r.ticks_per_hour = v.ticks_per_hour;
    r.process_type = v.process_type;
    r.price_source = v.price_source;
    r.vintage_source = v.vintage_source;
    r.vintage_date = v.vintage_date;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::synthetic_fx_spot_config>
synthetic_fx_spot_config_mapper::map(const std::vector<synthetic_fx_spot_config_entity>& v) {
    return map_vector<synthetic_fx_spot_config_entity, domain::synthetic_fx_spot_config>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<synthetic_fx_spot_config_entity>
synthetic_fx_spot_config_mapper::map(const std::vector<domain::synthetic_fx_spot_config>& v) {
    return map_vector<domain::synthetic_fx_spot_config, synthetic_fx_spot_config_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
