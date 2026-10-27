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
#include "ores.refdata.core/repository/equity_underlying_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.refdata.api/domain/equity_underlying.hpp"
#include "ores.refdata.api/domain/equity_underlying_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/equity_underlying_entity.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <optional>
#include <vector>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::equity_underlying equity_underlying_mapper::map(const equity_underlying_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::equity_underlying r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.code = v.code.value();

    r.name = v.name;

    r.description = v.description;
    r.instrument_type = v.instrument_type;
    r.resolves_to = v.resolves_to.value_or("");
    r.currency_code = v.currency_code.value_or("");
    r.display_order = v.display_order;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

equity_underlying_entity equity_underlying_mapper::map(const domain::equity_underlying& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    equity_underlying_entity r;
    r.code = v.code;
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;

    r.name = v.name;

    r.description = v.description;
    r.instrument_type = v.instrument_type;
    r.resolves_to = v.resolves_to.empty() ? std::nullopt : std::optional(v.resolves_to);
    r.currency_code = v.currency_code.empty() ? std::nullopt : std::optional(v.currency_code);
    r.display_order = v.display_order;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::equity_underlying>
equity_underlying_mapper::map(const std::vector<equity_underlying_entity>& v) {
    return map_vector<equity_underlying_entity, domain::equity_underlying>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<equity_underlying_entity>
equity_underlying_mapper::map(const std::vector<domain::equity_underlying>& v) {
    return map_vector<domain::equity_underlying, equity_underlying_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
