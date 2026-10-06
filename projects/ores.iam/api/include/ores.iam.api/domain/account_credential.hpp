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
#ifndef ORES_IAM_API_DOMAIN_ACCOUNT_CREDENTIAL_HPP
#define ORES_IAM_API_DOMAIN_ACCOUNT_CREDENTIAL_HPP

#include "ores.utility/rfl/skip_comparison.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <string_view>

namespace ores::iam::domain {

/**
 * @brief The secret material an account authenticates with.
 *
 * The secret an account proves when it authenticates, held apart from the
 * account's own profile. One row per account, keyed by account_id, and no
 * row for an account that holds no credential.
 *
 * The split exists because the two are written by different access paths.
 * ores.iam.account is the profile: a screen writes it, and every column
 * travels on the wire. This entity is the credential: the IAM service alone
 * writes it, and no column travels anywhere. Held in one table, a profile
 * write replaced the whole row, and a column the domain type could not carry
 * came back empty.
 *
 * Every secret column is :no_wire:, so the domain struct carries the value
 * the service needs while a serialisation of that struct omits it.
 *
 * The table is bi-temporal and audited, like every other iam entity: it
 * carries version, the four audit columns and the valid_from/valid_to
 * pair with the GIST exclusion, so the model takes the ordinary audited
 * shape and needs no shape flag. A credential write therefore keeps its own
 * history, and the account's history never mentions it.
 *
 * The entity is internal. It renders the data layer and the SQL schema and
 * nothing else: a credential is written and proved inside the IAM service,
 * never read by a caller, so the model declares no protocol, handler,
 * service, shell command or TypeScript twin. See the * Physical space
 * section for the reason each facet is withdrawn.
 */
struct account_credential final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique identifier for the credential row. A surrogate, like the account's own: nothing
     * outside this table names it.
     */
    boost::uuids::uuid id;

    /**
     * @brief The account this credential belongs to. One row per account, so the generated table
     * adds the unique index on (tenant_id, account_id).
     */
    boost::uuids::uuid account_id;

    /**
     * @brief The hash a user account proves when it signs in. Unset for an account that cannot sign
     * in with a password: a service, algorithm or llm account.
     *
     * The stored hash embeds its own salt, so no salt column sits beside it.
     */
    rfl::Skip<std::string> password_hash;

    /**
     * @brief The SHA-256 hash, as hex, of a service account's machine password. Unset for an
     * account that does not authenticate as a service.
     */
    rfl::Skip<std::string> service_password_hash;

    /**
     * @brief Time-based One-Time Password secret for two-factor authentication. No verification
     * path reads it yet, and no caller may ever see it.
     */
    rfl::Skip<std::string> totp_secret;

    /**
     * @brief Username of the person who last modified this account credential.
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
    friend bool operator==(const account_credential&, const account_credential&) = default;
};

/**
 * @brief Dispatch-key identifier for account_credential, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const account_credential&) {
    return "ores.iam.account_credential";
}

}

#endif
