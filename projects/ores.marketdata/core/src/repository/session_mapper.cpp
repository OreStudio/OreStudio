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
#include "ores.marketdata.core/repository/session_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.marketdata.api/domain/session.hpp"
#include "ores.marketdata.api/domain/session_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/session_entity.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <vector>

namespace ores::marketdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::session session_mapper::map(const session_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::session r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.code = v.code;
    r.name = v.name;
    r.supplier_code = v.supplier_code;
    r.adapter_code = v.adapter_code;
    r.environment = v.environment;
    r.endpoint = v.endpoint;
    r.credential_ref = v.credential_ref;
    r.settings = v.settings;
    r.enabled = v.enabled;
    r.description = v.description;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

session_entity session_mapper::map(const domain::session& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    session_entity r;
    r.code = v.code;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.name = v.name;
    r.supplier_code = v.supplier_code;
    r.adapter_code = v.adapter_code;
    r.environment = v.environment;
    r.endpoint = v.endpoint;
    r.credential_ref = v.credential_ref;
    r.settings = v.settings;
    r.enabled = v.enabled;
    r.description = v.description;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::session> session_mapper::map(const std::vector<session_entity>& v) {
    return map_vector<session_entity, domain::session>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<session_entity> session_mapper::map(const std::vector<domain::session>& v) {
    return map_vector<domain::session, session_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
