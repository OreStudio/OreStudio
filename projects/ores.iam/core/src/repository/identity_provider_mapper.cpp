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
#include "ores.iam.core/repository/identity_provider_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam.api/domain/identity_provider.hpp"
#include "ores.iam.api/domain/identity_provider_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/identity_provider_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <optional>
#include <vector>

namespace ores::iam::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::identity_provider identity_provider_mapper::map(const identity_provider_entity& v) {
    ORES_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::identity_provider r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.id = v.id;
    r.code = v.code;
    r.display_name = v.display_name;
    r.issuer = v.issuer;
    r.client_id = v.client_id;
    r.client_secret_ref = v.client_secret_ref.value_or("");
    r.scopes = v.scopes;
    r.allowed_email_domains = v.allowed_email_domains.value_or("");
    r.is_enabled = v.is_enabled;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    ORES_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

identity_provider_entity identity_provider_mapper::map(const domain::identity_provider& v) {
    ORES_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    identity_provider_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.code = v.code;
    r.display_name = v.display_name;
    r.issuer = v.issuer;
    r.client_id = v.client_id;
    r.client_secret_ref =
        v.client_secret_ref.empty() ? std::nullopt : std::optional(v.client_secret_ref);
    r.scopes = v.scopes;
    r.allowed_email_domains =
        v.allowed_email_domains.empty() ? std::nullopt : std::optional(v.allowed_email_domains);
    r.is_enabled = v.is_enabled;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    ORES_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::identity_provider>
identity_provider_mapper::map(const std::vector<identity_provider_entity>& v) {
    return map_vector<identity_provider_entity, domain::identity_provider>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<identity_provider_entity>
identity_provider_mapper::map(const std::vector<domain::identity_provider>& v) {
    return map_vector<domain::identity_provider, identity_provider_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
