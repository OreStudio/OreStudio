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
#ifndef ORES_IAM_API_DOMAIN_AGENT_HELD_WRITE_HPP
#define ORES_IAM_API_DOMAIN_AGENT_HELD_WRITE_HPP

#include <chrono>
#include <string>
#include <string_view>

namespace ores::iam::domain {

/**
 * @brief One write an LLM agent made through ores.http, held as the call itself until its person
 * approves the approval request it raised.
 *
 * A row is the pending change of an iam.write_by_agent approval request: the call
 * an agent made, kept as it was made, so that the approval replays it as the
 * person with the agent as actor. The replay then meets the controls the person's
 * own write would meet, which is the rest of the six eyes. The row's state says
 * whether the call waits, is being replayed, or has been applied or failed.
 *
 * The table holds one row per request and is rewritten in place as its state
 * moves, so the model states :current_state:. Its tenant_id is a text column
 * isolated by a policy in iam_rls_policies_create.sql, and only ores.http writes
 * it, in the write's own tenant.
 */
struct agent_held_write final {
    /**
     * @brief The approval request that holds the write, a uuid in its text form.
     */
    std::string request_id;

    /**
     * @brief When ores.http held the write.
     */
    std::chrono::system_clock::time_point held_at;

    /**
     * @brief The tenant the write acts in.
     */
    std::string tenant_id;

    /**
     * @brief The party the write acts in.
     */
    std::string party_id;

    /**
     * @brief The agent grant the write was made under.
     */
    std::string grant_id;

    /**
     * @brief The person the agent acts for, who decides the request.
     */
    std::string grantor_account_id;

    /**
     * @brief The HTTP method.
     */
    std::string method;

    /**
     * @brief The route's pattern, as the review panel names the act.
     */
    std::string route;

    /**
     * @brief The address the agent called, values included, which the replay calls again.
     */
    std::string target;

    /**
     * @brief The body's media type.
     */
    std::string content_type;

    /**
     * @brief The body the agent sent, which the replay sends again. A key is never in it: the agent
     * sends its key as a header.
     */
    std::string body;

    /**
     * @brief held, until the request is decided; applying, while ores.http replays it; applied or
     * failed after.
     */
    std::string state;

    /**
     * @brief What the replay answered, for the person and the review panel.
     */
    std::string outcome;

    /**
     * @brief The HTTP status the replay answered with, or zero before it ran.
     */
    int status = 0;

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
    friend bool operator==(const agent_held_write&, const agent_held_write&) = default;
};

/**
 * @brief Dispatch-key identifier for agent_held_write, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const agent_held_write&) {
    return "ores.iam.agent_held_write";
}

}

#endif
