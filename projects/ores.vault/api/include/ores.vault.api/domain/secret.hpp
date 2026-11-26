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
#ifndef ORES_VAULT_API_DOMAIN_SECRET_HPP
#define ORES_VAULT_API_DOMAIN_SECRET_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::vault::domain {

/**
 * @brief A secret a tenant keeps, sealed under a key version.
 *
 * A secret one tenant keeps, sealed under a key that lives outside the database.
 * The row holds the sealed value and the version of the key that sealed it, and
 * the value is bound to the row it sits in, so a ciphertext copied into another
 * row, another tenant or another version does not open.
 *
 * The plaintext is never a column and never a wire field. A consumer asks for the
 * value by name, and the answer leaves sealed to that consumer's own key, so the
 * value exists in the clear only inside the process that must use it.
 *
 * The sealed value itself is on the generated wire surfaces, because the sealed box
 * hands over one opaque string and the codec carries it as one. That is the
 * baseline and not the contract: describe answers the metadata alone, and only
 * the write request and the resolve reply carry a sealed value.
 *
 * Two invariants decide the shape. A tenant keeps a secret once by its name,
 * which is the natural key: the name is what a consumer asks for and what the
 * audit records, so a tenant cannot hold two secrets that answer one ask. And the
 * owner names the consumer permitted to resolve it, by the actor its token
 * carries, which is a fact about the row and not about the caller: the vault answers a resolve with
 * the owner's key, and a row with no owner is a row nobody may read.
 */
struct secret final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The secret's surrogate key. A consumer addresses a secret by its name, never by this
     * value.
     */
    boost::uuids::uuid id;

    /**
     * @brief The name a consumer asks for, such as identity_provider.google.client_secret, unique
     * within the tenant. It is the name the audit records and the name a rotation reports, so it is
     * stable and a person rarely types it.
     */
    std::string name;

    /**
     * @brief The consumer permitted to resolve this secret, named by the actor its token carries,
     * such as http@system.ores. The vault seals a resolve to the public key registered for the
     * subject that actor's token carries, so the owner is what decides who can open the answer.
     * Naming the actor rather than the subject lets a person write a secret for a service, whose
     * subject changes with the process that registers, without knowing which process answers.
     */
    std::string owner;

    /**
     * @brief The sealed value: one Base64 string carrying the nonce, the tag and the ciphertext,
     * bound to this row's tenant, name, owner and key version. It is opaque to every reader: a copy
     * of it opens only under the key encryption key the vault's process holds.
     */
    std::string sealed_value;

    /**
     * @brief The version of the key encryption key that sealed this value. A rotation re-wraps the
     * value and moves this number, and a resolve uses the version to choose the key it opens with,
     * so a rotation never has to happen in one instant.
     */
    int key_version = 0;

    /**
     * @brief What the secret is for, in a person's words, such as The Google client's secret.
     * Nothing reads it but a person, and it never repeats the value.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this secret.
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
    friend bool operator==(const secret&, const secret&) = default;
};

/**
 * @brief Dispatch-key identifier for secret, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const secret&) {
    return "ores.vault.secret";
}

}

#endif
