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
#ifndef ORES_IAM_REPOSITORY_RUN_TOKEN_ISSUE_ENTITY_HPP
#define ORES_IAM_REPOSITORY_RUN_TOKEN_ISSUE_ENTITY_HPP

#include "sqlgen/PrimaryKey.hpp"
#include <ostream>
#include <string>

namespace ores::iam::repository {

/**
 * @brief Entity for one run grant exchange that reached the issue log.
 *
 * A row names the grant, the run, the service that asked, the tenant and the
 * party, and the outcome. It is IAM's record of every run token it issued, and
 * the count of distinct runs a grant has served is read from it.
 *
 * No RLS, as for the auth events log. The primary key's second column is bound
 * as a string, the estate-wide convention for a timestamp column read through
 * the text protocol.
 */
struct run_token_issue_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_iam_run_token_issues_tbl";

    /**
     * @brief UUID for this issue -- part of composite primary key.
     */
    sqlgen::PrimaryKey<std::string> id;

    /**
     * @brief When the issue happened -- part of the composite primary key and
     * the hypertable's partition column.
     */
    sqlgen::PrimaryKey<std::string> issued_at;

    /**
     * @brief The tenant the runs act in. Empty when no tenant was resolved.
     */
    std::string tenant_id;

    /**
     * @brief The party the runs act for. Empty when no party was resolved.
     */
    std::string party_id;

    /**
     * @brief The run grant the exchange names.
     */
    std::string grant_id;

    /**
     * @brief The run the token serves.
     */
    std::string run_id;

    /**
     * @brief The requesting service name, the token's actor.
     */
    std::string service;

    /**
     * @brief The person who consented to the grant.
     */
    std::string grantor_account_id;

    /**
     * @brief Either issued or refused.
     */
    std::string outcome;

    /**
     * @brief Why a refusal happened: the check that refused. Empty on issue.
     */
    std::string reason;
};

inline std::ostream& operator<<(std::ostream& s, const run_token_issue_entity& v) {
    s << "id: " << v.id.value() << ", issued_at: " << v.issued_at.value()
      << ", tenant_id: " << v.tenant_id << ", party_id: " << v.party_id
      << ", grant_id: " << v.grant_id << ", run_id: " << v.run_id << ", service: " << v.service
      << ", grantor_account_id: " << v.grantor_account_id << ", outcome: " << v.outcome
      << ", reason: " << v.reason;
    return s;
}

}

#endif
