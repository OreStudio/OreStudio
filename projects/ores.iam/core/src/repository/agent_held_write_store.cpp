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
#include "ores.iam.core/repository/agent_held_write_store.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.iam.core/repository/agent_held_write_repository.hpp"
#include "ores.iam.core/repository/audit_scope.hpp"
#include <chrono>
#include <string>

namespace ores::iam::repository {

agent_held_write_store::agent_held_write_store(const context& ctx)
    : ctx_(ctx) {}

void agent_held_write_store::hold(domain::agent_held_write write) {
    write.state = std::string(held);
    write.held_at = std::chrono::system_clock::now();
    const auto scope = audit_scope_for(ctx_, write.tenant_id);
    write.tenant_id = scope.tenant_id;
    agent_held_write_repository().write(scope.ctx, write);
}

std::optional<domain::agent_held_write>
agent_held_write_store::find(const std::string& request_id) const {
    const auto rows = ores::database::query_builder::execute_parameterized_multi_column_query(
        ctx_,
        "select tenant_id, party_id, grant_id, grantor_account_id, method, route, target, "
        "content_type, body, state, outcome, status::text, "
        "(extract(epoch from held_at) * 1000000)::bigint::text "
        "from ores_iam_agent_held_writes_tbl where request_id = $1",
        {request_id},
        lg(),
        "Reading an agent's held write");
    if (rows.empty() || rows.front().size() != 13)
        return std::nullopt;
    const auto& row = rows.front();
    const auto text = [&](std::size_t i) {
        return row[i].value_or("");
    };
    domain::agent_held_write w;
    w.request_id = request_id;
    w.tenant_id = text(0);
    w.party_id = text(1);
    w.grant_id = text(2);
    w.grantor_account_id = text(3);
    w.method = text(4);
    w.route = text(5);
    w.target = text(6);
    w.content_type = text(7);
    w.body = text(8);
    w.state = text(9);
    w.outcome = text(10);
    w.status = std::stoi(row[11].value_or("0"));
    w.held_at = std::chrono::system_clock::time_point(
        std::chrono::microseconds(std::stoll(row[12].value_or("0"))));
    return w;
}

bool agent_held_write_store::claim(const std::string& request_id) const {
    auto write = find(request_id);
    if (!write || write->state != held)
        return false;
    write->state = std::string(applying);
    agent_held_write_repository().write(ctx_, *write);
    return true;
}

void agent_held_write_store::finish(const std::string& request_id,
                                    std::string_view state,
                                    int status,
                                    const std::string& outcome) const {
    auto write = find(request_id);
    if (!write || write->state != applying)
        return;
    write->state = std::string(state);
    write->status = status;
    write->outcome = outcome.substr(0, 500);
    agent_held_write_repository().write(ctx_, *write);
}

}
