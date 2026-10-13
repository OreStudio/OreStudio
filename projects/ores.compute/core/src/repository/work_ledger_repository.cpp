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
#include "ores.compute.core/repository/work_ledger_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <utility>

namespace ores::compute::repository {

using namespace ores::logging;
using namespace ores::database::repository;

namespace {

/// A text array literal for the platforms, each element quoted.
std::string array_literal(const std::vector<std::string>& values) {
    std::string out = "{";
    for (std::size_t i = 0; i < values.size(); ++i) {
        if (i != 0)
            out += ',';
        out += '"';
        for (const char c : values[i]) {
            if (c == '"' || c == '\\')
                out += '\\';
            out += c;
        }
        out += '"';
    }
    out += '}';
    return out;
}

boost::uuids::uuid uuid_of(const std::optional<std::string>& cell) {
    return cell ? boost::lexical_cast<boost::uuids::uuid>(*cell) : boost::uuids::uuid{};
}

} // namespace

void work_ledger_repository::enqueue(context ctx, const ledger_entry& entry) {
    BOOST_LOG_SEV(lg(), debug) << "Queueing result " << boost::uuids::to_string(entry.result_id);
    static const std::string sql = R"(
        INSERT INTO ores_compute_work_ledger_tbl
            (result_id, work_tenant_id, workunit_id, platforms, priority)
        VALUES ($1::uuid, $2::uuid, $3::uuid, $4::text[], $5::int)
        ON CONFLICT (result_id) DO NOTHING
    )";
    execute_parameterized_command(ctx,
                                  sql,
                                  {boost::uuids::to_string(entry.result_id),
                                   boost::uuids::to_string(entry.work_tenant_id),
                                   boost::uuids::to_string(entry.workunit_id),
                                   array_literal(entry.platforms),
                                   std::to_string(entry.priority)},
                                  lg(),
                                  "queueing a result");
}

std::optional<claimed_entry> work_ledger_repository::claim(context ctx,
                                                           const boost::uuids::uuid& host_id,
                                                           const std::string& platform,
                                                           std::chrono::seconds lease) {
    // The load of a tenant is the entries it holds claimed. The tenant with the
    // fewest goes first, so claims alternate between tenants that both have work
    // waiting. The lock is on the ledger row only.
    static const std::string sql = R"(
        WITH loads AS (
            SELECT work_tenant_id, count(*) AS claimed
            FROM ores_compute_work_ledger_tbl
            WHERE state = 'claimed'
            GROUP BY work_tenant_id
        ), pick AS (
            SELECT q.result_id
            FROM ores_compute_work_ledger_tbl q
            LEFT JOIN loads l ON l.work_tenant_id = q.work_tenant_id
            WHERE q.state = 'queued' AND $2::text = ANY (q.platforms)
            ORDER BY coalesce(l.claimed, 0), q.priority DESC, q.queued_at
            LIMIT 1
            FOR UPDATE OF q SKIP LOCKED
        )
        UPDATE ores_compute_work_ledger_tbl w
        SET state = 'claimed',
            host_id = $1::uuid,
            claimed_at = now(),
            lease_expires_at = now() + make_interval(secs => $3::int),
            attempts = w.attempts + 1
        FROM pick
        WHERE w.result_id = pick.result_id
        RETURNING w.result_id, w.work_tenant_id, w.workunit_id, w.attempts
    )";
    const auto rows = execute_parameterized_multi_column_query(
        ctx,
        sql,
        {boost::uuids::to_string(host_id), platform, std::to_string(lease.count())},
        lg(),
        "claiming the next job");
    if (rows.empty() || rows.front().size() < 4)
        return std::nullopt;
    const auto& row = rows.front();
    return claimed_entry{.result_id = uuid_of(row[0]),
                         .work_tenant_id = uuid_of(row[1]),
                         .workunit_id = uuid_of(row[2]),
                         .attempts = row[3] ? std::stoi(*row[3]) : 0};
}

void work_ledger_repository::release(context ctx, const boost::uuids::uuid& result_id) {
    static const std::string sql = R"(
        UPDATE ores_compute_work_ledger_tbl
        SET state = 'queued', host_id = NULL, claimed_at = NULL, lease_expires_at = NULL,
            attempts = greatest(attempts - 1, 0)
        WHERE result_id = $1::uuid AND state = 'claimed'
    )";
    execute_parameterized_command(
        ctx, sql, {boost::uuids::to_string(result_id)}, lg(), "releasing a claimed job");
}

void work_ledger_repository::remove(context ctx, const boost::uuids::uuid& result_id) {
    static const std::string sql =
        "DELETE FROM ores_compute_work_ledger_tbl WHERE result_id = $1::uuid";
    execute_parameterized_command(
        ctx, sql, {boost::uuids::to_string(result_id)}, lg(), "removing a ledger entry");
}

bool work_ledger_repository::extend_lease(context ctx,
                                          const boost::uuids::uuid& host_id,
                                          const boost::uuids::uuid& result_id,
                                          std::chrono::seconds lease) {
    static const std::string sql = R"(
        WITH extended AS (
            UPDATE ores_compute_work_ledger_tbl
            SET lease_expires_at = now() + make_interval(secs => $3::int)
            WHERE result_id = $2::uuid AND host_id = $1::uuid AND state = 'claimed'
            RETURNING 1
        )
        SELECT count(*) FROM extended
    )";
    const auto rows = execute_parameterized_multi_column_query(ctx,
                                                               sql,
                                                               {boost::uuids::to_string(host_id),
                                                                boost::uuids::to_string(result_id),
                                                                std::to_string(lease.count())},
                                                               lg(),
                                                               "extending the lease of a job");
    return !rows.empty() && !rows.front().empty() && rows.front().front() &&
           std::stoll(*rows.front().front()) > 0;
}

std::size_t work_ledger_repository::requeue_held(context ctx,
                                                 const boost::uuids::uuid& host_id,
                                                 int max_attempts) {
    static const std::string sql = R"(
        WITH returned AS (
            UPDATE ores_compute_work_ledger_tbl
            SET state = 'queued', host_id = NULL, claimed_at = NULL, lease_expires_at = NULL
            WHERE host_id = $1::uuid AND state = 'claimed' AND attempts < $2::int
            RETURNING 1
        )
        SELECT count(*) FROM returned
    )";
    const auto rows = execute_parameterized_multi_column_query(
        ctx,
        sql,
        {boost::uuids::to_string(host_id), std::to_string(max_attempts)},
        lg(),
        "returning the jobs a host abandoned");
    if (rows.empty() || rows.front().empty() || !rows.front().front())
        return 0;
    return static_cast<std::size_t>(std::stoll(*rows.front().front()));
}

std::size_t work_ledger_repository::requeue_expired(context ctx, int max_attempts) {
    static const std::string sql = R"(
        WITH returned AS (
            UPDATE ores_compute_work_ledger_tbl
            SET state = 'queued', host_id = NULL, claimed_at = NULL, lease_expires_at = NULL
            WHERE state = 'claimed' AND lease_expires_at <= now() AND attempts < $1::int
            RETURNING 1
        )
        SELECT count(*) FROM returned
    )";
    const auto rows = execute_parameterized_multi_column_query(
        ctx, sql, {std::to_string(max_attempts)}, lg(), "returning expired leases to the queue");
    if (rows.empty() || rows.front().empty() || !rows.front().front())
        return 0;
    return static_cast<std::size_t>(std::stoll(*rows.front().front()));
}

std::vector<expired_entry>
work_ledger_repository::exhausted(context ctx, int max_attempts, int limit) {
    static const std::string sql = R"(
        SELECT result_id, work_tenant_id, host_id, attempts
        FROM ores_compute_work_ledger_tbl
        WHERE state = 'claimed' AND lease_expires_at <= now() AND attempts >= $1::int
        ORDER BY lease_expires_at
        LIMIT $2::int
    )";
    const auto rows = execute_parameterized_multi_column_query(
        ctx,
        sql,
        {std::to_string(max_attempts), std::to_string(limit)},
        lg(),
        "reading the jobs that ran out of attempts");
    std::vector<expired_entry> out;
    for (const auto& row : rows) {
        if (row.size() < 4)
            continue;
        out.push_back({.result_id = uuid_of(row[0]),
                       .work_tenant_id = uuid_of(row[1]),
                       .host_id = uuid_of(row[2]),
                       .attempts = row[3] ? std::stoi(*row[3]) : 0});
    }
    return out;
}

bool work_ledger_repository::remove_expired(context ctx, const boost::uuids::uuid& result_id) {
    static const std::string sql = R"(
        WITH removed AS (
            DELETE FROM ores_compute_work_ledger_tbl
            WHERE result_id = $1::uuid AND state = 'claimed' AND lease_expires_at <= now()
            RETURNING 1
        )
        SELECT count(*) FROM removed
    )";
    const auto rows = execute_parameterized_multi_column_query(
        ctx, sql, {boost::uuids::to_string(result_id)}, lg(), "removing an expired job");
    return !rows.empty() && !rows.front().empty() && rows.front().front() &&
           std::stoll(*rows.front().front()) > 0;
}

std::int64_t work_ledger_repository::waiting(context ctx,
                                             const boost::uuids::uuid& work_tenant_id) {
    static const std::string sql = R"(
        SELECT count(*) FROM ores_compute_work_ledger_tbl
        WHERE work_tenant_id = $1::uuid AND state = 'queued'
    )";
    const auto rows =
        execute_parameterized_multi_column_query(ctx,
                                                 sql,
                                                 {boost::uuids::to_string(work_tenant_id)},
                                                 lg(),
                                                 "counting the jobs a tenant has waiting");
    if (rows.empty() || rows.front().empty() || !rows.front().front())
        return 0;
    return std::stoll(*rows.front().front());
}

std::optional<boost::uuids::uuid>
work_ledger_repository::holder(context ctx, const boost::uuids::uuid& result_id) {
    static const std::string sql = R"(
        SELECT host_id FROM ores_compute_work_ledger_tbl
        WHERE result_id = $1::uuid AND state = 'claimed'
    )";
    const auto rows = execute_parameterized_multi_column_query(
        ctx, sql, {boost::uuids::to_string(result_id)}, lg(), "reading the holder of a job");
    if (rows.empty() || rows.front().empty() || !rows.front().front())
        return std::nullopt;
    return uuid_of(rows.front().front());
}

std::int64_t work_ledger_repository::count(context ctx, const std::string& state) {
    static const std::string sql =
        "SELECT count(*) FROM ores_compute_work_ledger_tbl WHERE state = $1::text";
    const auto rows = execute_parameterized_multi_column_query(
        ctx, sql, {state}, lg(), "counting ledger entries");
    if (rows.empty() || rows.front().empty() || !rows.front().front())
        return 0;
    return std::stoll(*rows.front().front());
}

}
