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
#ifndef ORES_IAM_API_DOMAIN_AGENT_GRANT_HPP
#define ORES_IAM_API_DOMAIN_AGENT_GRANT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::iam::domain {

/**
 * @brief A person's consent that one named LLM agent acts for them in one party.
 *
 * An agent grant records that a person allows one named LLM agent to act for
 * them in one tenant and one party, bounded by a role made for the work, until an
 * expiry of at most ninety days or until the grant is revoked. It holds no secret: an agent key
 * redeems it, and IAM re-checks the grant and its person at every exchange. It is IAM's second form
 * of the Grant Management pattern, after the run grant, and
 * [[id:A7E2942C-66AD-449C-B80B-CA789EBC34FA][Agent grants: delegated consent for LLM agents]]
 * states its checks and its lifecycle.
 *
 * The table is readable over the wire and written only by IAM's own create and
 * revoke operations, which take the person's own session:
 * :client_read_only: removes the generic write verbs from the wire and keeps
 * the repository writable for those operations.
 */
struct agent_grant final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this agent grant.
     *
     * The grant id. It is random, and it is not a credential.
     */
    boost::uuids::uuid id;

    /**
     * @brief The party the agent acts in. Enforced by row level security, so a party sees only its
     * own grants.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The person who gave the consent, and for whom the agent acts. A token made from the
     * grant names this account as its subject.
     */
    boost::uuids::uuid grantor_account_id;

    /**
     * @brief The name the person gives the agent, such as claude-code or desk-assistant. A token
     * made from the grant names it as its actor, so every call the agent makes is recorded as this
     * agent's. A person grants each agent once per party.
     */
    std::string agent_name;

    /**
     * @brief The role the agent acts under. At creation the person must hold every permission of
     * the role, and the role may name no permission an agent is never given; at every exchange the
     * token carries the role's permissions the person still holds, less those withheld from agents.
     */
    boost::uuids::uuid role_id;

    /**
     * @brief The instant after which the grant serves no exchange: at most ninety days after the
     * grant was made. A grant is never extended; continuing is a new grant.
     */
    std::chrono::system_clock::time_point not_after;

    /**
     * @brief When the grant was revoked; empty while it is active.
     */
    std::chrono::system_clock::time_point revoked_at = {};

    /**
     * @brief Who revoked the grant: its person, or an administrator.
     */
    std::string revoked_by;

    /**
     * @brief Why the grant was revoked, in the revoker's words.
     */
    std::string revoke_reason;

    /**
     * @brief Username of the person who last modified this agent grant.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     *
     * The transaction-time window's start, which the store sets from its own
     * clock. It travels with the audit members because it is only ever read
     * with them: the history builder takes a version type that carries an
     * actor *and* this timestamp, so an entity without the actor has no use
     * for the timestamp either.
     */
    std::chrono::system_clock::time_point recorded_at;

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
    friend bool operator==(const agent_grant&, const agent_grant&) = default;
};

/**
 * @brief Dispatch-key identifier for agent_grant, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const agent_grant&) {
    return "ores.iam.agent_grant";
}

}

#endif
