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
#include "ores.iam.core/repository/external_identity_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam.api/domain/external_identity.hpp"
#include "ores.iam.api/domain/external_identity_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/external_identity_entity.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <optional>
#include <string_view>
#include <vector>

namespace ores::iam::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::external_identity external_identity_mapper::map(const external_identity_entity& v) {
    ORES_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::external_identity r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.id = v.id;
    r.issuer = v.issuer;
    r.subject = v.subject;
    r.account_id = v.account_id;
    r.identity_provider_id = v.identity_provider_id;
    r.email = v.email.value_or("");
    r.email_verified = v.email_verified;
    r.linked_at = v.linked_at;
    r.last_used_at = v.last_used_at.value_or(db_timestamp{});
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    ORES_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

external_identity_entity external_identity_mapper::map(const domain::external_identity& v) {
    ORES_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    external_identity_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.issuer = v.issuer;
    r.subject = v.subject;
    r.account_id = v.account_id;
    r.identity_provider_id = v.identity_provider_id;
    r.email = v.email.empty() ? std::nullopt : std::optional(v.email);
    r.email_verified = v.email_verified;
    r.linked_at = v.linked_at;
    r.last_used_at =
        v.last_used_at == db_timestamp{} ? std::nullopt : std::optional(v.last_used_at);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    ORES_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::external_identity>
external_identity_mapper::map(const std::vector<external_identity_entity>& v) {
    return map_vector<external_identity_entity, domain::external_identity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<external_identity_entity>
external_identity_mapper::map(const std::vector<domain::external_identity>& v) {
    return map_vector<domain::external_identity, external_identity_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
