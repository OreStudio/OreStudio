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
#ifndef ORES_IAM_API_DOMAIN_ACCOUNT_HPP
#define ORES_IAM_API_DOMAIN_ACCOUNT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::iam::domain {

/**
 * @brief An account that can authenticate against the system.
 *
 * An account that can authenticate against the system: one row per user,
 * service, algorithm or LLM identity, carrying the password material, the
 * TOTP secret, the email address and the optional profile and reporting
 * links. The table is bi-temporal and audited (see
 * projects/ores.sql/create/iam/iam_accounts_create.sql): it carries
 * version, the four audit columns and the valid_from/valid_to pair
 * with the GIST exclusion, so the model takes the ordinary audited shape
 * and needs no shape flag.
 *
 * The table is a composite parent: ores_iam_accounts_touch_version_fn
 * lets a child entity (account contact information, party association)
 * bump this account's own version when the child is written. The model
 * declares :generate_touch_function: true, which renders that function
 * under its existing name rather than leaving it hand-written.
 *
 * The model describes the table and nothing else. Two columns need care:
 *
 * - service_password_hash is a real column with no domain member: it is
 *   reached only by check_service_credentials and never travels on the
 *   wire, so it is declared :sql_only: true and the generated domain
 *   struct omits it while the entity struct and the mapper keep it.
 * - image_id and reports_to_account_id are nullable UUID soft
 *   references. The hand-written domain struct represented both as a plain
 *   boost::uuids::uuid with a nil sentinel, on the claim that a second
 *   std::optional<boost::uuids::uuid> member corrupts reflect-cpp
 *   aggregate serialisation for multi-element vectors. Re-verified under
 *   the generated estate: all three nullable UUIDs are modelled as
 *   std::optional<boost::uuids::uuid>, and the api suite's multi-element
 *   JSON and table tests plus the core repository's five-account round trip
 *   pass, so the workaround is not needed here.
 *
 * Two behavioural facets are switched off, each with a reason:
 *
 * - The entity's CRUD handler and sub-registrar, because the hand-written
 *   account_operations_handler already owns every iam.v1.accounts.* subject.
 * - The generated CRUD service, because the hand-written account_operations_service
 *   is the authentication surface (login, lock, unlock, password change
 *   and reset, party selection, service-credential check) and the
 *   generated service's get_account_history(id) collides in name and
 *   signature with the hand-written get_account_history(username) while
 *   meaning a different read. The generated account_protocol.hpp is
 *   suppressed by the same one-owner gate that the operation model already
 *   satisfies.
 */
struct account final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique identifier for the account.
     */
    boost::uuids::uuid id;

    /**
     * @brief Unique username for login purposes. It is the natural key, so the generated table adds
     * the partial unique index on (tenant_id, username) the hand-written table already had.
     */
    std::string username;

    /**
     * @brief Account type classification. Determines account capabilities: user accounts can login
     * with password, while service, algorithm and llm accounts authenticate via sessions.
     */
    std::string account_type = std::string("user");

    /**
     * @brief The account holder's full (real) name. Not every account represents a person (service,
     * algorithm and llm accounts leave this empty), and this is the only place a human account's
     * real name is recorded.
     */
    std::string full_name;

    /**
     * @brief Hashed password for secure authentication.
     */
    std::string password_hash;

    /**
     * @brief Salt used in password hashing for additional security.
     */
    std::string password_salt;

    /**
     * @brief Time-based One-Time Password secret for two-factor authentication.
     */
    std::string totp_secret;

    /**
     * @brief Email address associated with the account. It is unique within a tenant, so the
     * generated table adds the partial unique index on (tenant_id, email) the hand-written table
     * already had.
     */
    std::string email;

    /**
     * @brief Party to log into automatically when quick-login is enabled. Soft reference to a party
     * owned by ores.refdata; unset means the account must always go through the party picker.
     */
    std::optional<boost::uuids::uuid> default_party_id;

    /**
     * @brief Profile picture for this account. Soft reference to an image owned by ores.assets;
     * unset means the account has no profile picture.
     */
    std::optional<boost::uuids::uuid> image_id;

    /**
     * @brief Job title / functional role of the person holding this account (for example "Head of
     * Desk", "Senior Trader"). Distinct from the RBAC role assignments, which grant coarse
     * permission sets rather than describe what the person actually does.
     */
    std::string job_title;

    /**
     * @brief The account this person reports to, capturing the functional reporting line. Soft
     * self-reference to another row in this same table; unset means no reporting line is recorded.
     */
    std::optional<boost::uuids::uuid> reports_to_account_id;

    /**
     * @brief Username of the person who last modified this account.
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
     */
    std::chrono::system_clock::time_point recorded_at;
};

/**
 * @brief Dispatch-key identifier for account, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const account&) {
    return "ores.iam.account";
}

}

#endif
