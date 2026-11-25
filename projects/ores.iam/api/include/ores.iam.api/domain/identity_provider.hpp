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
#ifndef ORES_IAM_API_DOMAIN_IDENTITY_PROVIDER_HPP
#define ORES_IAM_API_DOMAIN_IDENTITY_PROVIDER_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::iam::domain {

/**
 * @brief A provider a tenant enables for sign-in.
 *
 * A provider one tenant enables, so that a person can sign up and sign in with an
 * account it holds. It carries what ORE needs to send the browser there and to
 * read back the answer: the issuer to read the provider's discovery and its keys
 * from, the client ORE registered with it, the scopes to ask for, and the address
 * domains the tenant will accept from it.
 *
 * The client's secret is not a column. The row names an environment variable or a
 * configuration key, and the deployment sets the value, so no secret that can be
 * read back enters the database or its backups. A provider that takes no secret,
 * such as a test provider or a public client, leaves the name empty.
 *
 * Two invariants decide the shape. A tenant enables a provider once, by its
 * code, which is the natural key: the code names the rows the door reads and the
 * routes the browser is sent to, so google appears once per tenant. And a tenant
 * with no row for a provider has no such door at all, so an installation that
 * enables nothing behaves exactly as it does today, with a password alone.
 */
struct identity_provider final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The provider's surrogate key. Nothing outside the table addresses a provider by it;
     * the code is what the door and the routes carry.
     */
    boost::uuids::uuid id;

    /**
     * @brief The provider's short name, such as google, unique within the tenant. It names the
     * routes the browser follows and the row the door reads, so it is stable and lower case, and a
     * person never types it.
     */
    std::string code;

    /**
     * @brief The name the sign-in screen shows on the door, such as Google. The screen draws the
     * provider's own name here rather than a translation of it, because a provider is a company's
     * name and not a word.
     */
    std::string display_name;

    /**
     * @brief The issuer identifier, such as https://accounts.google.com, which is also the address
     * the provider's discovery document and its published keys are read from. The ID token's iss
     * claim is checked against this value, so a token from another provider is refused even if it
     * is otherwise valid.
     */
    std::string issuer;

    /**
     * @brief The identifier ORE registered with the provider. It travels in the authorization
     * request and is checked against the ID token's aud claim.
     */
    std::string client_id;

    /**
     * @brief The name of the environment variable or configuration key that holds the client's
     * secret, never the secret itself. The gateway resolves the name when it starts and keeps the
     * value in memory. Empty for a provider that issues public clients, which presents no secret.
     */
    std::string client_secret_ref;

    /**
     * @brief The scopes ORE asks the provider for, space separated. ORE asks for identity alone and
     * never for access to a person's mail, calendar or files, so the default is what every provider
     * receives.
     */
    std::string scopes = std::string("openid email profile");

    /**
     * @brief The address domains this tenant accepts from the provider, comma separated, such as
     * acme.com,acme.co.uk. Empty accepts any address the provider verifies, and the registration
     * policy still decides what the person receives. A domain here is a door rule, not a link rule:
     * nothing links an existing account to a provider account by its address.
     */
    std::string allowed_email_domains;

    /**
     * @brief Whether the door offers this provider. A disabled row keeps its registration and its
     * history and hides the door, which is how a tenant withdraws a provider without forgetting how
     * to restore it.
     */
    bool is_enabled = false;

    /**
     * @brief Username of the person who last modified this identity provider.
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
    friend bool operator==(const identity_provider&, const identity_provider&) = default;
};

/**
 * @brief Dispatch-key identifier for identity_provider, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const identity_provider&) {
    return "ores.iam.identity_provider";
}

}

#endif
