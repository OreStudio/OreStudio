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
#include "ores.iam.core/repository/account_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam.api/domain/account_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::iam::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::account account_mapper::map(const account_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::account r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());

    r.username = v.username;

    r.account_type = v.account_type;
    r.full_name = v.full_name.value_or("");
    r.password_hash = v.password_hash;
    r.password_salt = v.password_salt;
    r.totp_secret = v.totp_secret;
    r.email = v.email;
    r.default_party_id =
        v.default_party_id.has_value() ?
            std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.default_party_id)) :
            std::nullopt;
    r.image_id = v.image_id.has_value() ?
                     std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.image_id)) :
                     std::nullopt;
    r.job_title = v.job_title.value_or("");
    r.reports_to_account_id =
        v.reports_to_account_id.has_value() ?
            std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.reports_to_account_id)) :
            std::nullopt;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

account_entity account_mapper::map(const domain::account& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    account_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;

    r.username = v.username;

    r.account_type = v.account_type;
    r.full_name = v.full_name.empty() ? std::nullopt : std::optional(v.full_name);
    r.password_hash = v.password_hash;
    r.password_salt = v.password_salt;
    r.totp_secret = v.totp_secret;
    r.email = v.email;
    r.default_party_id = v.default_party_id.has_value() ?
                             std::optional(boost::uuids::to_string(*v.default_party_id)) :
                             std::nullopt;
    r.image_id =
        v.image_id.has_value() ? std::optional(boost::uuids::to_string(*v.image_id)) : std::nullopt;
    r.job_title = v.job_title.empty() ? std::nullopt : std::optional(v.job_title);
    r.reports_to_account_id = v.reports_to_account_id.has_value() ?
                                  std::optional(boost::uuids::to_string(*v.reports_to_account_id)) :
                                  std::nullopt;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::account> account_mapper::map(const std::vector<account_entity>& v) {
    return map_vector<account_entity, domain::account>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<account_entity> account_mapper::map(const std::vector<domain::account>& v) {
    return map_vector<domain::account, account_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
