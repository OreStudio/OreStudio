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
#ifndef ORES_IAM_API_DOMAIN_AGENT_KEY_HPP
#define ORES_IAM_API_DOMAIN_AGENT_KEY_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::iam::domain {

/**
 * @brief A key that redeems an agent grant, stored as a hash of its secret.
 *
 * An agent key redeems an [[id:ED9E53BF-27A3-4261-9767-3A25B98BFCA4][agent grant]]: an LLM agent
 * presents it as a bearer token, and IAM exchanges it for a short token that names the grant's
 * person and the agent ([[id:1162906F-BB72-486E-9A11-30D567C65F3E][Delegation tokens for LLM
 * agents: subject and actor claims]]). The key is shown once, when it is made, and is
 * ores_ak_<tenant id>_<key id>_<secret>. Only its id, which finds it, and the
 * SHA-256 hash of its secret, which checks it, are stored. A grant may have
 * several keys, so a key can be replaced before the old one is revoked.
 *
 * The table is readable over the wire and written only by IAM's own operations:
 * :client_read_only: removes the generic write verbs from the wire.
 */
struct agent_key final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this agent key.
     *
     * The key id, which is part of the key and finds it. It is not a credential on its own.
     */
    boost::uuids::uuid id;

    /**
     * @brief The grant the key redeems.
     */
    boost::uuids::uuid grant_id;

    /**
     * @brief The SHA-256 hash of the key's secret, in hexadecimal. A fast hash suffices because the
     * secret is 256 random bits, not a password a person chose.
     */
    std::string secret_hash;

    /**
     * @brief When the key was revoked; empty while it redeems its grant.
     */
    std::chrono::system_clock::time_point revoked_at = {};

    /**
     * @brief Username of the person who last modified this agent key.
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
    friend bool operator==(const agent_key&, const agent_key&) = default;
};

/**
 * @brief Dispatch-key identifier for agent_key, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const agent_key&) {
    return "ores.iam.agent_key";
}

}

#endif
