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
#include "ores.analytics.core/repository/credit_simulation_config_mapper.hpp"
#include "ores.analytics.api/domain/credit_simulation_config_json_io.hpp" // IWYU pragma: keep.
#include "ores.database/repository/mapper_helpers.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::analytics::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::credit_simulation_config
credit_simulation_config_mapper::map(const credit_simulation_config_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::credit_simulation_config r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.name = v.name;
    r.configuration_id = v.configuration_id.has_value() ?
                             boost::lexical_cast<boost::uuids::uuid>(*v.configuration_id) :
                             boost::uuids::uuid{};
    r.market = v.market.value_or("");
    r.credit = v.credit.value_or("");
    r.zero_market_pnl = v.zero_market_pnl.value_or(0);
    r.evaluation = v.evaluation.value_or("");
    r.double_default = v.double_default.value_or(0);
    r.seed = v.seed.value_or(0);
    r.paths = v.paths.value_or(0);
    r.credit_mode = v.credit_mode.value_or("");
    r.loan_exposure_mode = v.loan_exposure_mode.value_or("");
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

credit_simulation_config_entity
credit_simulation_config_mapper::map(const domain::credit_simulation_config& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    credit_simulation_config_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.name = v.name;
    r.configuration_id = v.configuration_id == boost::uuids::uuid{} ?
                             std::nullopt :
                             std::optional(boost::uuids::to_string(v.configuration_id));
    r.market = v.market.empty() ? std::nullopt : std::optional(v.market);
    r.credit = v.credit.empty() ? std::nullopt : std::optional(v.credit);
    r.zero_market_pnl = v.zero_market_pnl == 0 ? std::nullopt : std::optional(v.zero_market_pnl);
    r.evaluation = v.evaluation.empty() ? std::nullopt : std::optional(v.evaluation);
    r.double_default = v.double_default == 0 ? std::nullopt : std::optional(v.double_default);
    r.seed = v.seed == 0 ? std::nullopt : std::optional(v.seed);
    r.paths = v.paths == 0 ? std::nullopt : std::optional(v.paths);
    r.credit_mode = v.credit_mode.empty() ? std::nullopt : std::optional(v.credit_mode);
    r.loan_exposure_mode =
        v.loan_exposure_mode.empty() ? std::nullopt : std::optional(v.loan_exposure_mode);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::credit_simulation_config>
credit_simulation_config_mapper::map(const std::vector<credit_simulation_config_entity>& v) {
    return map_vector<credit_simulation_config_entity, domain::credit_simulation_config>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<credit_simulation_config_entity>
credit_simulation_config_mapper::map(const std::vector<domain::credit_simulation_config>& v) {
    return map_vector<domain::credit_simulation_config, credit_simulation_config_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
