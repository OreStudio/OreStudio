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
#include "ores.iam.core/repository/run_token_issue_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam.core/repository/run_token_issue_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <stdexcept>

namespace ores::iam::repository {

using namespace ores::logging;
using namespace ores::database::repository;
using ores::platform::time::datetime;

namespace {

/// The outcome of an issue that minted a token.
constexpr std::string_view issued = "issued";

}

run_token_issue_repository::run_token_issue_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void run_token_issue_repository::record(const run_token_issue& issue) {
    record(issue.issued_at,
           issue.tenant_id,
           issue.party_id,
           issue.grant_id,
           issue.run_id,
           issue.service,
           issue.grantor_account_id,
           issue.outcome,
           issue.reason);
}

void run_token_issue_repository::record(const std::chrono::system_clock::time_point& issued_at,
                                        const std::string& tenant_id,
                                        const std::string& party_id,
                                        const std::string& grant_id,
                                        const std::string& run_id,
                                        const std::string& service,
                                        const std::string& grantor_account_id,
                                        const std::string& outcome,
                                        const std::string& reason) {

    BOOST_LOG_SEV(lg(), debug) << "Recording run token issue for grant " << grant_id << " run "
                               << run_id << ": " << outcome;

    boost::uuids::random_generator uuid_gen;
    const auto id_str = boost::lexical_cast<std::string>(uuid_gen());

    run_token_issue_entity entity;
    entity.id = id_str;
    entity.issued_at = datetime::to_db_string(issued_at);
    entity.tenant_id = tenant_id;
    entity.party_id = party_id;
    entity.grant_id = grant_id;
    entity.run_id = run_id;
    entity.service = service;
    entity.grantor_account_id = grantor_account_id;
    entity.outcome = outcome;
    entity.reason = reason;

    const auto r = sqlgen::session(ctx_.connection_pool())
                       .and_then(sqlgen::begin_transaction)
                       .and_then(sqlgen::insert(entity))
                       .and_then(sqlgen::commit);
    ensure_success(r, lg());
}

std::size_t run_token_issue_repository::distinct_runs(const std::string& grant_id) const {
    // Only issued rows count: a refusal served no run, so counting it would
    // let a refused request consume one of the grant's runs.
    const auto rows = execute_parameterized_string_query(
        ctx_,
        "select count(distinct run_id)::text from ores_iam_run_token_issues_tbl "
        "where grant_id = $1 and outcome = $2",
        {grant_id, std::string(issued)},
        lg(),
        "Counting the runs a run grant has served");
    if (rows.empty())
        return 0;
    try {
        return static_cast<std::size_t>(std::stoull(rows.front()));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error)
            << "The run count for grant " << grant_id << " is not a number: " << rows.front()
            << " (" << e.what() << ")";
        throw;
    }
}

bool run_token_issue_repository::exists(const std::string& grant_id,
                                        const std::string& run_id) const {
    const auto rows = execute_parameterized_string_query(
        ctx_,
        "select 1::text from ores_iam_run_token_issues_tbl "
        "where grant_id = $1 and run_id = $2 and outcome = $3 limit 1",
        {grant_id, run_id, std::string(issued)},
        lg(),
        "Reading whether a run grant already served a run");
    return !rows.empty();
}

}
