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
#include "ores.synthetic.core/repository/ir_curve_generation_config_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::synthetic::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::ir_curve_generation_config
ir_curve_generation_config_mapper::map(const ir_curve_generation_config_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::ir_curve_generation_config r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);

    r.config_id = boost::lexical_cast<boost::uuids::uuid>(v.config_id);


    r.currency_code = v.currency_code;


    r.index_name = v.index_name;

    r.process_type = v.process_type;
    r.kappa = v.kappa;
    r.theta = v.theta;
    r.sigma = v.sigma;
    r.initial_rate = v.initial_rate;
    r.ticks_per_hour = v.ticks_per_hour;
    r.enabled = v.enabled;
    r.auto_start = v.auto_start;
    r.description = v.description.value_or("");
    r.fixed_leg_payment_frequency_code = v.fixed_leg_payment_frequency_code;
    r.source_name = v.source_name;
    r.folder_id = v.folder_id.has_value() ?
                      std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.folder_id)) :
                      std::nullopt;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

ir_curve_generation_config_entity
ir_curve_generation_config_mapper::map(const domain::ir_curve_generation_config& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    ir_curve_generation_config_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);

    r.config_id = boost::uuids::to_string(v.config_id);


    r.currency_code = v.currency_code;


    r.index_name = v.index_name;

    r.process_type = v.process_type;
    r.kappa = v.kappa;
    r.theta = v.theta;
    r.sigma = v.sigma;
    r.initial_rate = v.initial_rate;
    r.ticks_per_hour = v.ticks_per_hour;
    r.enabled = v.enabled;
    r.auto_start = v.auto_start;
    r.description = v.description.empty() ? std::nullopt : std::optional(v.description);
    r.fixed_leg_payment_frequency_code = v.fixed_leg_payment_frequency_code;
    r.source_name = v.source_name;
    r.folder_id = v.folder_id.has_value() ? std::optional(boost::uuids::to_string(*v.folder_id)) :
                                            std::nullopt;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::ir_curve_generation_config>
ir_curve_generation_config_mapper::map(const std::vector<ir_curve_generation_config_entity>& v) {
    return map_vector<ir_curve_generation_config_entity, domain::ir_curve_generation_config>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<ir_curve_generation_config_entity>
ir_curve_generation_config_mapper::map(const std::vector<domain::ir_curve_generation_config>& v) {
    return map_vector<domain::ir_curve_generation_config, ir_curve_generation_config_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
