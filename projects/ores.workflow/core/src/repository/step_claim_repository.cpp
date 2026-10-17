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
#include "ores.workflow.core/repository/step_claim_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <string>

namespace ores::workflow::repository {

using namespace ores::logging;
using namespace ores::database::repository;

std::optional<int> step_claim_repository::claim(context ctx,
                                                const boost::uuids::uuid& step_id,
                                                const boost::uuids::uuid& work_tenant_id,
                                                const std::string& owner,
                                                std::chrono::seconds lease) {
    BOOST_LOG_SEV(lg(), debug) << "Claiming step " << boost::uuids::to_string(step_id) << " as "
                               << owner;
    // The insert is the first claim, and the conditional update is every claim
    // after it. The WHERE belongs to the update alone: it is what makes a
    // takeover conditional on the lease having ended, so an insert that loses
    // the race to a live claim changes nothing and returns no row.
    static const std::string sql = R"(
        INSERT INTO ores_workflow_step_claims_tbl
            (step_id, work_tenant_id, owner, lease_expires_at, attempts)
        VALUES ($1::uuid, $2::uuid, $3::text, now() + make_interval(secs => $4::int), 1)
        ON CONFLICT (step_id) DO UPDATE
            SET owner = excluded.owner,
                claimed_at = now(),
                lease_expires_at = excluded.lease_expires_at,
                attempts = ores_workflow_step_claims_tbl.attempts + 1
            WHERE ores_workflow_step_claims_tbl.lease_expires_at <= now()
        RETURNING step_id, attempts
    )";
    const auto rows =
        execute_parameterized_multi_column_query(ctx,
                                                 sql,
                                                 {boost::uuids::to_string(step_id),
                                                  boost::uuids::to_string(work_tenant_id),
                                                  owner,
                                                  std::to_string(lease.count())},
                                                 lg(),
                                                 "claiming a workflow step");
    // No row is a lost race, which the caller answers by returning the delivery
    // to the queue. It is not an error, so it is not an exception.
    if (rows.empty() || rows.front().size() < 2 || !rows.front()[1])
        return std::nullopt;
    return std::stoi(*rows.front()[1]);
}

bool step_claim_repository::release(context ctx, const boost::uuids::uuid& step_id) {
    // No owner predicate. The engine writes the step's state away from
    // in_progress, and that write is what decides the work is over; the claim
    // only governed the window while it was in flight. Naming the holder here
    // would tie the engine's own act to whoever happened to hold the row, and
    // the row would survive every release the engine made.
    static const std::string sql = R"(
        WITH released AS (
            DELETE FROM ores_workflow_step_claims_tbl
            WHERE step_id = $1::uuid
            RETURNING 1
        )
        SELECT count(*) FROM released
    )";
    const auto rows =
        execute_parameterized_multi_column_query(ctx,
                                                 sql,
                                                 {boost::uuids::to_string(step_id)},
                                                 lg(),
                                                 "releasing the claim on a workflow step");
    return !rows.empty() && !rows.front().empty() && rows.front().front() &&
           std::stoll(*rows.front().front()) > 0;
}

std::optional<std::string> step_claim_repository::holder(context ctx,
                                                         const boost::uuids::uuid& step_id) {
    static const std::string sql =
        "SELECT owner FROM ores_workflow_step_claims_tbl WHERE step_id = $1::uuid";
    const auto rows = execute_parameterized_multi_column_query(
        ctx, sql, {boost::uuids::to_string(step_id)}, lg(), "reading the holder of a step claim");
    if (rows.empty() || rows.front().empty() || !rows.front().front())
        return std::nullopt;
    return rows.front().front();
}

}
