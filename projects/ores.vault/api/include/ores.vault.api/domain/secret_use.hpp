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
#ifndef ORES_VAULT_API_DOMAIN_SECRET_USE_HPP
#define ORES_VAULT_API_DOMAIN_SECRET_USE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::vault::domain {

/**
 * @brief One act on a secret: the tenant, the name, the subject that asked, the outcome, the key
 * version and the instant.
 *
 * One row per act on a secret, appended and never changed. The row says who asked
 * and what happened; it never says what the value was, and there is no column for a
 * value, a plaintext or a ciphertext.
 *
 * The rows are append-only, so the model states :current_state:: an act is a fact
 * about an instant, and the bi-temporal pair has no meaning on it. The audit columns
 * go too, because the row has no author and no version of its own: the subject
 * column is the actor.
 *
 * Three acts append a row: a resolve, a rotation and a delete. A refusal appends one
 * as well, with outcome naming what was refused, because a caller that asks for a
 * secret it does not own is the row an operator most wants to read.
 *
 * The name is the secret's name at the time of the act and not a reference to the
 * secret row: the record has to outlive the row it describes, and a delete leaves a
 * use behind. The key_version is the version the row was opened under, and it is
 * zero for an act that opened nothing, such as a refusal or a delete.
 */
struct secret_use final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The use's surrogate key. Nothing addresses a use by it.
     */
    boost::uuids::uuid id;

    /**
     * @brief The secret's name at the time of the act.
     */
    std::string name;

    /**
     * @brief The subject of the verified token that asked, which is the actor the vault can name. A
     * refusal records the subject that was refused.
     */
    std::string subject;

    /**
     * @brief What happened: resolved, rotated, deleted, refused_unknown_secret or
     * refused_not_owner.
     */
    std::string outcome;

    /**
     * @brief The key version the value was opened under. Zero when the act opened nothing.
     */
    int key_version = 0;

    /**
     * @brief The instant of the act, taken by the vault and not by the caller.
     */
    std::chrono::system_clock::time_point used_at;

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
    friend bool operator==(const secret_use&, const secret_use&) = default;
};

/**
 * @brief Dispatch-key identifier for secret_use, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const secret_use&) {
    return "ores.vault.secret_use";
}

}

#endif
