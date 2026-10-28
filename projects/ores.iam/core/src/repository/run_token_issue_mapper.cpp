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
#include "ores.iam.core/repository/run_token_issue_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam.api/domain/run_token_issue.hpp"
#include "ores.iam.api/domain/run_token_issue_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/run_token_issue_entity.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <chrono>
#include <string_view>
#include <vector>

namespace ores::iam::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::run_token_issue run_token_issue_mapper::map(const run_token_issue_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::run_token_issue r;
    r.id = v.id;
    r.issued_at = timestamp_to_timepoint(std::string_view{v.issued_at});
    r.tenant_id = v.tenant_id;
    r.party_id = v.party_id;
    r.grant_id = v.grant_id;
    r.run_id = v.run_id;
    r.service = v.service;
    r.grantor_account_id = v.grantor_account_id;
    r.outcome = v.outcome;
    r.reason = v.reason;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

run_token_issue_entity run_token_issue_mapper::map(const domain::run_token_issue& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    run_token_issue_entity r;
    r.id = v.id;
    r.issued_at = ores::platform::time::datetime::to_db_string(v.issued_at);
    r.tenant_id = v.tenant_id;
    r.party_id = v.party_id;
    r.grant_id = v.grant_id;
    r.run_id = v.run_id;
    r.service = v.service;
    r.grantor_account_id = v.grantor_account_id;
    r.outcome = v.outcome;
    r.reason = v.reason;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::run_token_issue>
run_token_issue_mapper::map(const std::vector<run_token_issue_entity>& v) {
    return map_vector<run_token_issue_entity, domain::run_token_issue>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<run_token_issue_entity>
run_token_issue_mapper::map(const std::vector<domain::run_token_issue>& v) {
    return map_vector<domain::run_token_issue, run_token_issue_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
