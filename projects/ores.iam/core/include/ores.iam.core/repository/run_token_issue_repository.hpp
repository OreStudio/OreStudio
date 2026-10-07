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
#ifndef ORES_IAM_REPOSITORY_RUN_TOKEN_ISSUE_REPOSITORY_HPP
#define ORES_IAM_REPOSITORY_RUN_TOKEN_ISSUE_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstddef>
#include <sqlgen/postgres.hpp>
#include <string>
#include <string_view>
#include <vector>

namespace ores::iam::repository {

/**
 * @brief One row of the run token issue log, as the exchange writes and reads
 * it.
 *
 * The log is the exchange's record of every token it issued and refused, and
 * the store a grant's @c max_runs is counted in. It is insert-only.
 */
struct run_token_issue {
    std::chrono::system_clock::time_point issued_at;
    std::string tenant_id;
    std::string party_id;
    std::string grant_id;
    std::string run_id;
    std::string service;
    std::string grantor_account_id;
    std::string outcome;
    std::string reason;
};

/**
 * @brief Repository for the run token issue log.
 *
 * Writes to, and reads the counts from, the ores_iam_run_token_issues_tbl
 * TimescaleDB hypertable. Insert-only: an issue is a fact once recorded.
 *
 * This is a system-level audit log: no RLS is applied, so every tenant's rows
 * are reachable from the connection the exchange happens to hold.
 */
class ORES_IAM_CORE_EXPORT run_token_issue_repository {
private:
    inline static std::string_view logger_name = "ores.iam.repository.run_token_issue_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;
    explicit run_token_issue_repository(context ctx);

    /**
     * @brief Appends one issue-log row.
     *
     * @param issue The row: the grant, the run, the service and the outcome.
     */
    void record(const run_token_issue& issue);

    /**
     * @brief Appends one issue-log row from its parts.
     *
     * @param outcome Either @c issued or @c refused.
     * @param reason Why a refusal happened; empty on issue.
     */
    void record(const std::chrono::system_clock::time_point& issued_at,
                const std::string& tenant_id,
                const std::string& party_id,
                const std::string& grant_id,
                const std::string& run_id,
                const std::string& service,
                const std::string& grantor_account_id,
                const std::string& outcome,
                const std::string& reason);

    /**
     * @brief How many distinct runs a grant has already been served.
     *
     * The count a grant's @c max_runs is measured against. Issued rows alone
     * are counted: a refusal served no run.
     */
    [[nodiscard]] std::size_t distinct_runs(const std::string& grant_id) const;

    /**
     * @brief Whether a grant has already been served for this run.
     *
     * A second exchange for the same run is a retry of the same work, so it
     * neither consumes another run nor is refused as exhausted.
     */
    [[nodiscard]] bool exists(const std::string& grant_id, const std::string& run_id) const;

private:
    context ctx_;
};

}

#endif
