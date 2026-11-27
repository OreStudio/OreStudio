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
#ifndef ORES_VAULT_API_DOMAIN_CONSUMER_HPP
#define ORES_VAULT_API_DOMAIN_CONSUMER_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::vault::domain {

/**
 * @brief A service the vault seals its answers to.
 *
 * A consumer of the vault: the identity a service's token carries, and the public
 * half of the key pair that service holds. The vault never takes a key from a
 * request. It reads the subject out of the verified token, finds the row that
 * names it, and seals the answer to the public key held here.
 *
 * The private half is never in this table, never in the database and never in the
 * tree. It is a system credential of the consumer's unit, provisioned with the
 * deployment, so a copy of this table discloses nothing that can open an answer.
 *
 * Two invariants decide the shape. A consumer registers once by its name, the
 * subject its token presents, which is the natural key: two rows for one subject
 * would give one service two keys, and the vault would have to choose. And a row
 * with no public key cannot be sealed to, so a consumer that has not been
 * provisioned is a consumer that receives nothing.
 */
struct consumer final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The consumer's surrogate key. Nothing addresses a consumer by it; the subject is what
     * the token carries and what the vault looks up.
     */
    boost::uuids::uuid id;

    /**
     * @brief The subject the consumer's token presents, such as ores.http, unique within the
     * tenant. It is the identity the service runner authenticates, and the only thing the vault
     * uses to find the key an answer is sealed to.
     */
    std::string name;

    /**
     * @brief The Base64 of the consumer's 32-byte X25519 public key. It is not a secret: it is the
     * half the vault needs in order to seal, and it is useless without the private half held by the
     * consumer's unit.
     */
    std::string public_key;

    /**
     * @brief Whether the vault seals to this consumer at all. A disabled row keeps its key and its
     * history and refuses every resolve, which is how a consumer is withdrawn without deleting what
     * it asked for.
     */
    bool is_enabled = false;

    /**
     * @brief The account the consumer runs as, in the form the audit columns take, such as
     * powershell@system.ores:9000. A secret names its owner by this value, so the vault compares an
     * owner with a consumer whether the caller proved itself with a token or with the key it holds:
     * the token path reads the actor from the context, and the bootstrap path reads it from this
     * row.
     */
    std::string actor;

    /**
     * @brief What the consumer is, in a person's words, such as The gateway. Nothing reads it but a
     * person.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this consumer.
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
    friend bool operator==(const consumer&, const consumer&) = default;
};

/**
 * @brief Dispatch-key identifier for consumer, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const consumer&) {
    return "ores.vault.consumer";
}

}

#endif
