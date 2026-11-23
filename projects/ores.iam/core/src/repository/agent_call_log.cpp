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
#include "ores.iam.core/repository/agent_call_log.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.iam.core/repository/agent_call_repository.hpp"
#include "ores.iam.core/repository/audit_scope.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <string>

namespace ores::iam::repository {

agent_call_log::agent_call_log(const context& ctx)
    : ctx_(ctx) {}

void agent_call_log::record(domain::agent_call call) {
    call.id = boost::lexical_cast<std::string>(boost::uuids::random_generator()());
    const auto scope = audit_scope_for(ctx_, call.tenant_id);
    call.tenant_id = scope.tenant_id;
    agent_call_repository().write(scope.ctx, call);
}

std::vector<domain::agent_call> agent_call_log::calls_for(const std::string& grantor_account_id,
                                                          const std::string& grant_id,
                                                          std::uint32_t offset,
                                                          std::uint32_t limit) const {
    const auto rows = ores::database::query_builder::execute_parameterized_multi_column_query(
        ctx_,
        "select tenant_id, party_id, grant_id, session_id, grantor_account_id, method, route, "
        "status::text, duration_ms::text, (extract(epoch from called_at) * 1000000)::bigint::text, "
        "id from ores_iam_agent_calls_tbl where grantor_account_id = $1 "
        "and ($2 = '' or grant_id = $2) order by called_at desc limit $3 offset $4",
        {grantor_account_id, grant_id, std::to_string(limit), std::to_string(offset)},
        lg(),
        "Reading a person's agent calls");
    std::vector<domain::agent_call> calls;
    calls.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() != 11)
            continue;
        const auto text = [&](std::size_t i) { return row[i].value_or(""); };
        domain::agent_call c;
        c.tenant_id = text(0);
        c.party_id = text(1);
        c.grant_id = text(2);
        c.session_id = text(3);
        c.grantor_account_id = text(4);
        c.method = text(5);
        c.route = text(6);
        c.status = std::stoi(row[7].value_or("0"));
        c.duration_ms = std::stoi(row[8].value_or("0"));
        c.called_at = std::chrono::system_clock::time_point(
            std::chrono::microseconds(std::stoll(row[9].value_or("0"))));
        c.id = text(10);
        calls.push_back(std::move(c));
    }
    return calls;
}

}
