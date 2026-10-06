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
#include "ores.iam.core/repository/account_credential_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam.api/domain/account_credential.hpp"
#include "ores.iam.api/domain/account_credential_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/account_credential_entity.hpp"
#include "ores.logging/boost_severity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <optional>
#include <vector>

namespace ores::iam::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::account_credential account_credential_mapper::map(const account_credential_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::account_credential r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.account_id = boost::lexical_cast<boost::uuids::uuid>(v.account_id);

    r.password_hash = v.password_hash.value_or("");
    r.service_password_hash = v.service_password_hash.value_or("");
    r.totp_secret = v.totp_secret.value_or("");
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

account_credential_entity account_credential_mapper::map(const domain::account_credential& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    account_credential_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.account_id = boost::uuids::to_string(v.account_id);

    r.password_hash =
        v.password_hash.get().empty() ? std::nullopt : std::optional(v.password_hash.get());
    r.service_password_hash = v.service_password_hash.get().empty() ?
                                  std::nullopt :
                                  std::optional(v.service_password_hash.get());
    r.totp_secret = v.totp_secret.get().empty() ? std::nullopt : std::optional(v.totp_secret.get());
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::account_credential>
account_credential_mapper::map(const std::vector<account_credential_entity>& v) {
    return map_vector<account_credential_entity, domain::account_credential>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<account_credential_entity>
account_credential_mapper::map(const std::vector<domain::account_credential>& v) {
    return map_vector<domain::account_credential, account_credential_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
