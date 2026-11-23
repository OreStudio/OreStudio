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
#ifndef ORES_IAM_API_DOMAIN_AGENT_CALL_HPP
#define ORES_IAM_API_DOMAIN_AGENT_CALL_HPP

#include <chrono>
#include <string>
#include <string_view>

namespace ores::iam::domain {

/**
 * @brief One call an LLM agent made through ores.http, read or write; a TimescaleDB hypertable
 * partitioned by called_at.
 *
 * A row says that an LLM agent, holding a grant, called one route of ores.http
 * for its person, with which method, how it was answered and how long it took.
 * Reads are logged as writes are, because a person who wants to know what an
 * agent did wants to know what it looked at. It never holds a key, a token or a
 * request body. A person reads their agents' rows; the record each write changed
 * carries the agent in its own acting_agent stamp.
 *
 * The table is an audit log like the agent token issue log, and follows it: its
 * tenant_id is an ordinary text column isolated by a policy in
 * iam_rls_policies_create.sql, and only ores.http writes it, after it answers
 * the call, each row through a session of the call's own tenant. No retention
 * policy is set.
 *
 * The table is internal, so only the data layer and the SQL schema are generated.
 * The reads stay hand-written in agent_call_log.
 */
struct agent_call final {
    /**
     * @brief The row's id, a uuid in its text form. It sits in the primary key beside the partition
     * column.
     */
    std::string id;

    /**
     * @brief When ores.http answered the call.
     */
    std::chrono::system_clock::time_point called_at;

    /**
     * @brief The tenant the call acted in.
     */
    std::string tenant_id;

    /**
     * @brief The party the call acted in.
     */
    std::string party_id;

    /**
     * @brief The agent grant the call was made under.
     */
    std::string grant_id;

    /**
     * @brief The agent's session the call belongs to.
     */
    std::string session_id;

    /**
     * @brief The person the agent acted for.
     */
    std::string grantor_account_id;

    /**
     * @brief The HTTP method.
     */
    std::string method;

    /**
     * @brief The route's pattern, never the address with its values, so no identifier or search
     * reaches the log.
     */
    std::string route;

    /**
     * @brief The HTTP status ores.http answered with.
     */
    int status = 0;

    /**
     * @brief How long the call took, in milliseconds.
     */
    int duration_ms = 0;

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
    friend bool operator==(const agent_call&, const agent_call&) = default;
};

/**
 * @brief Dispatch-key identifier for agent_call, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const agent_call&) {
    return "ores.iam.agent_call";
}

}

#endif
