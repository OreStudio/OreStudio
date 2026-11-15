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
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <string>

namespace ores::workflow::repository {

using namespace ores::logging;
using namespace ores::database::repository;

std::optional<int> step_claim_repository::claim(const context& ctx,
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
                attempts = ores_workflow_step_claims_tbl.attempts + 1,
                progress = null,
                progress_done = null,
                progress_total = null,
                progressed_at = now()
            WHERE ores_workflow_step_claims_tbl.lease_expires_at <= now()
        RETURNING step_id, attempts
    )";
    const auto rows = ores::database::query_builder::execute_parameterized_multi_column_query(
        ctx,
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

bool step_claim_repository::renew(const context& ctx,
                                  const boost::uuids::uuid& step_id,
                                  const std::string& owner,
                                  std::chrono::seconds lease,
                                  const std::optional<step_progress>& progress) {
    // The progress's time moves only when the report differs from the stored
    // one, so a holder that renews without advancing is seen to stand still. A
    // renewal with no report leaves the stored progress and its time alone.
    static const std::string sql = R"(
        UPDATE ores_workflow_step_claims_tbl
            SET lease_expires_at = now() + make_interval(secs => $3::int),
                progressed_at = CASE
                    WHEN $4::boolean AND (progress, progress_done, progress_total)
                         IS DISTINCT FROM ($5::text, $6::bigint, $7::bigint)
                    THEN now() ELSE progressed_at END,
                progress = CASE WHEN $4::boolean THEN $5::text ELSE progress END,
                progress_done = CASE WHEN $4::boolean THEN $6::bigint ELSE progress_done END,
                progress_total = CASE WHEN $4::boolean THEN $7::bigint ELSE progress_total END
            WHERE step_id = $1::uuid AND owner = $2::text
        RETURNING step_id
    )";
    const auto count = [](const std::optional<std::int64_t>& v) -> std::optional<std::string> {
        if (!v)
            return std::nullopt;
        return std::to_string(*v);
    };
    const auto rows = ores::database::query_builder::execute_parameterized_multi_column_query(
        ctx,
        sql,
        {boost::uuids::to_string(step_id),
         owner,
         std::to_string(lease.count()),
         progress ? "true" : "false",
         progress ? std::optional<std::string>(progress->phase) : std::nullopt,
         progress ? count(progress->done) : std::nullopt,
         progress ? count(progress->total) : std::nullopt},
        lg(),
        "renewing a workflow step claim");
    return !rows.empty();
}

bool step_claim_repository::release(const context& ctx, const boost::uuids::uuid& step_id) {
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
    const auto rows = ores::database::query_builder::execute_parameterized_multi_column_query(
        ctx,
        sql,
        {boost::uuids::to_string(step_id)},
        lg(),
        "releasing the claim on a workflow step");
    return !rows.empty() && !rows.front().empty() && rows.front().front() &&
           std::stoll(*rows.front().front()) > 0;
}

std::optional<step_claim_activity>
step_claim_repository::activity(const context& ctx, const boost::uuids::uuid& step_id) {
    static const std::string sql = R"(
        SELECT claimed_at, progressed_at, progress, progress_done, progress_total
            FROM ores_workflow_step_claims_tbl WHERE step_id = $1::uuid
    )";
    const auto rows = ores::database::query_builder::execute_parameterized_multi_column_query(
        ctx, sql, {boost::uuids::to_string(step_id)}, lg(), "reading a step claim's activity");
    if (rows.empty() || rows.front().size() < 5 || !rows.front()[0] || !rows.front()[1])
        return std::nullopt;
    const auto& row = rows.front();
    step_claim_activity result{.claimed_at = timestamp_to_timepoint(std::string_view{*row[0]}),
                               .progressed_at = timestamp_to_timepoint(std::string_view{*row[1]}),
                               .progress = std::nullopt};
    if (row[2] || row[3] || row[4]) {
        step_progress progress{.phase = row[2].value_or(""), .done = {}, .total = {}};
        if (row[3])
            progress.done = std::stoll(*row[3]);
        if (row[4])
            progress.total = std::stoll(*row[4]);
        result.progress = std::move(progress);
    }
    return result;
}

std::optional<std::string> step_claim_repository::holder(const context& ctx,
                                                         const boost::uuids::uuid& step_id) {
    static const std::string sql =
        "SELECT owner FROM ores_workflow_step_claims_tbl WHERE step_id = $1::uuid";
    const auto rows = ores::database::query_builder::execute_parameterized_multi_column_query(
        ctx, sql, {boost::uuids::to_string(step_id)}, lg(), "reading the holder of a step claim");
    if (rows.empty() || rows.front().empty() || !rows.front().front())
        return std::nullopt;
    return rows.front().front();
}

}
