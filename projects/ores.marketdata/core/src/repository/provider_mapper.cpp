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
#include "ores.marketdata.core/repository/provider_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.marketdata.api/domain/provider.hpp"
#include "ores.marketdata.api/domain/provider_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/provider_entity.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <optional>
#include <vector>

namespace ores::marketdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::provider provider_mapper::map(const provider_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::provider r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.code = v.code;

    r.name = v.name;

    r.kind = v.kind;
    r.description = v.description;
    r.display_order = v.display_order;
    r.terms_url = v.terms_url;
    r.terms_credit = v.terms_credit;
    r.allows_storage = v.allows_storage;
    r.credential_ref = v.credential_ref;
    r.terms_accepted_digest = v.terms_accepted_digest;
    r.terms_accepted_by = v.terms_accepted_by;
    r.terms_accepted_at = v.terms_accepted_at.has_value() ?
                              std::optional(timestamp_to_timepoint(*v.terms_accepted_at)) :
                              std::nullopt;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

provider_entity provider_mapper::map(const domain::provider& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    provider_entity r;
    r.code = v.code;
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;

    r.name = v.name;

    r.kind = v.kind;
    r.description = v.description;
    r.display_order = v.display_order;
    r.terms_url = v.terms_url;
    r.terms_credit = v.terms_credit;
    r.allows_storage = v.allows_storage;
    r.credential_ref = v.credential_ref;
    r.terms_accepted_digest = v.terms_accepted_digest;
    r.terms_accepted_by = v.terms_accepted_by;
    r.terms_accepted_at =
        v.terms_accepted_at.has_value() ?
            std::optional(ores::platform::time::datetime::to_db_string(*v.terms_accepted_at)) :
            std::nullopt;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::provider> provider_mapper::map(const std::vector<provider_entity>& v) {
    return map_vector<provider_entity, domain::provider>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<provider_entity> provider_mapper::map(const std::vector<domain::provider>& v) {
    return map_vector<domain::provider, provider_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
