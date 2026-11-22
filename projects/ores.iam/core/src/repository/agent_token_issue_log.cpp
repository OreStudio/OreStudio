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
#include "ores.iam.core/repository/agent_token_issue_log.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.iam.api/domain/agent_token_issue.hpp"
#include "ores.iam.core/repository/agent_token_issue_repository.hpp"
#include "ores.iam.core/repository/audit_scope.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <string>

namespace ores::iam::repository {

using namespace ores::logging;

agent_token_issue_log::agent_token_issue_log(const context& ctx)
    : ctx_(ctx) {}

void agent_token_issue_log::record(const agent_token_issue_record& record) {
    domain::agent_token_issue row;
    row.id = boost::lexical_cast<std::string>(boost::uuids::random_generator()());
    row.issued_at = record.issued_at;
    row.party_id = record.party_id;
    row.grant_id = record.grant_id;
    row.key_id = record.key_id;
    row.session_id = record.session_id;
    row.grantor_account_id = record.grantor_account_id;
    row.client_address = record.client_address;
    row.outcome = record.outcome;
    row.reason = record.reason;

    // Each row is written by a session of its own tenant, as the isolation
    // policy requires; a refusal that names no readable tenant is the platform's.
    const auto scope = audit_scope_for(ctx_, record.tenant_id);
    row.tenant_id = scope.tenant_id;
    agent_token_issue_repository().write(scope.ctx, row);
}

std::optional<agent_key_last_issue>
agent_token_issue_log::last_issue(const std::string& key_id) const {
    const auto rows = ores::database::query_builder::execute_parameterized_multi_column_query(
        ctx_,
        "select session_id, extract(epoch from issued_at)::bigint::text "
        "from ores_iam_agent_token_issues_tbl "
        "where key_id = $1 and outcome = $2 order by issued_at desc limit 1",
        {key_id, std::string(issued)},
        lg(),
        "Reading an agent key's last issue");
    if (rows.empty() || rows[0].size() != 2 || !rows[0][0] || !rows[0][1])
        return std::nullopt;
    return agent_key_last_issue{.session_id = *rows[0][0],
                                .issued_at = std::chrono::system_clock::time_point(
                                    std::chrono::seconds(std::stoll(*rows[0][1])))};
}

}
