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
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_IAM_API_DOMAIN_RUN_TOKEN_ISSUE_HPP
#define ORES_IAM_API_DOMAIN_RUN_TOKEN_ISSUE_HPP

#include <chrono>
#include <string>
#include <string_view>

namespace ores::iam::domain {

/**
 * @brief One run grant exchange that reached the log, issued or refused; a TimescaleDB hypertable
 * partitioned by issued_at.
 *
 * A row says who asked for a run token, for which grant and which run, and whether
 * the token was issued or refused. The exchange reads it to count the distinct runs
 * a grant has already served, so a grant with a max_runs admits exactly that
 * many runs. The rows are append-only, so the model states :current_state:.
 *
 * The table is a system-level audit log. Its tenant_id is an ordinary text
 * column, and the table carries no row-level security. Only IAM's own exchange
 * reads and writes it.
 *
 * No retention policy is set. A grant that names a max_runs is counted from this
 * table, so dropping rows would let a grant serve more runs than its grantor
 * allowed.
 *
 * The table is internal, so only the data layer and the SQL schema are generated.
 * The reads stay hand-written in run_token_issue_log, because they are counts
 * the generated reads do not express.
 */
struct run_token_issue final {
    /**
     * @brief The row's id, a uuid in its text form. It sits in the primary key beside the partition
     * column.
     */
    std::string id;

    /**
     * @brief When the exchange reached the log. The partition column.
     */
    std::chrono::system_clock::time_point issued_at;

    /**
     * @brief The tenant of the grant, in its text form.
     */
    std::string tenant_id;

    /**
     * @brief The party the grant names, in its text form.
     */
    std::string party_id;

    /**
     * @brief The run grant the exchange was made against.
     */
    std::string grant_id;

    /**
     * @brief The run the token was asked for.
     */
    std::string run_id;

    /**
     * @brief The service that asked.
     */
    std::string service;

    /**
     * @brief The account whose consent the grant records.
     */
    std::string grantor_account_id;

    /**
     * @brief What the exchange answered: issued, when a token was minted, or refused.
     */
    std::string outcome;

    /**
     * @brief The check that refused the exchange, or unavailable when a limit refused before the
     * checks. Empty for an issue.
     */
    std::string reason;

    /**
     * @brief Value equality.
     *
     * Every generated domain type is a value: two of them are equal when their
     * members are, whatever the entity means. A test that round-trips one
     * through the wire asserts exactly that, so equality is part of the shape
     * rather than something each entity decides -- an entity without it cannot
     * be round-trip tested at all, which is why the omission went unnoticed
     * until the diff payloads were the first generated types to have a test.
     */
    friend bool operator==(const run_token_issue&, const run_token_issue&) = default;
};

/**
 * @brief Dispatch-key identifier for run_token_issue, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const run_token_issue&) {
    return "ores.iam.run_token_issue";
}

}

#endif
