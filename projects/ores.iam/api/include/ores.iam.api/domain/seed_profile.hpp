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
#ifndef ORES_IAM_API_DOMAIN_SEED_PROFILE_HPP
#define ORES_IAM_API_DOMAIN_SEED_PROFILE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::iam::domain {

/**
 * @brief A starting point a new tenant is provisioned from.
 *
 * A seed profile is the data set that provisioning gives a new tenant. One
 * row orders step kinds from the catalogue in code, declares the parameters
 * its form takes, carries the tenant details it prefills, and carries the card
 * the person choosing a starting point reads.
 *
 * A new profile is a row, so an operator adds one with no code change. A new
 * step kind is code. The accepted seed profile contract fixes both lists,
 * and the first two rows are:
 *
 * - empty_operational: the production starting point. It publishes the
 *   base bundle, takes a counterparty count and a GLEIF root LEI to
 *   import, and creates no test data.
 * - acme_demo: the Acme Corporation holding group, with its staff, books
 *   and market data. Every test datum lives here.
 *
 * A profile is system-owned registered data. The system_scope flag states
 * that the table stores it under the system tenant and that the insert
 * trigger forces that tenant. Provisioning reads a profile before the tenant
 * it creates exists, so a profile cannot belong to the tenant being made.
 *
 * The profile the model binds is uuid-identified-lookup, the one its
 * sibling ores.iam.tenant binds, because that profile leaves
 * system_scope to the model. uuid-surrogate-lookup fixes it to false,
 * which a row the platform owns cannot be.
 *
 * The ordered steps are rows of ores.iam.seed_profile_step, and the form
 * schema is rows of ores.iam.seed_profile_parameter. Both are children of
 * this entity.
 */
struct seed_profile final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate identifier for the profile. Provisioning names a profile by its code, so the
     * surrogate exists for referential stability only.
     */
    boost::uuids::uuid id;

    /**
     * @brief Stable code the request names the profile by. The unique index states the code once,
     * because a profile is one row per deployment and not one row per tenant.
     *
     * Examples: 'empty_operational', 'acme_demo'.
     */
    std::string code;

    /**
     * @brief Name shown on the starting-point card.
     */
    std::string name;

    /**
     * @brief The card's tagline: one line saying what the profile sets up, for example
     * "Production-ready setup".
     */
    std::string summary;

    /**
     * @brief Who the profile is for, as the line beside its name on the card, for example "For real
     * use" or "For demos and testing".
     */
    std::string audience;

    /**
     * @brief The card's bullets, in the order it shows them, as a serialised JSON array of short
     * lines. The card shows at most three. A profile that states none shows none, which is not the
     * same as stating an empty line.
     */
    std::string bullets_json;

    /**
     * @brief Tenant display name the profile prefills. An empty value states that the form starts
     * blank and the administrator supplies the name.
     */
    std::string tenant_name;

    /**
     * @brief Tenant code the profile prefills. Together with the hostname it is what a later
     * sign-in principal names.
     */
    std::string tenant_code;

    /**
     * @brief Hostname the profile prefills. A principal of the form username@hostname resolves its
     * tenant through this value, so a profile that prefills one makes the tenant reachable without
     * further input.
     */
    std::string tenant_hostname;

    /**
     * @brief Username the profile prefills for the tenant administrator.
     */
    std::string admin_username;

    /**
     * @brief Email the profile prefills for the tenant administrator.
     */
    std::string admin_email;

    /**
     * @brief Whether the tenant administrator starts with the password of the administrator who
     * runs the provisioning. A demonstration profile sets it, so its journey asks for no password;
     * a production profile does not.
     */
    bool inherits_admin_password = false;

    /**
     * @brief Whether the tenant administrator must set a password of their own at first sign-in.
     */
    bool force_password_change = false;

    /**
     * @brief Order the cards are offered in. Lower numbers appear first.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this seed profile.
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
    friend bool operator==(const seed_profile&, const seed_profile&) = default;
};

/**
 * @brief Dispatch-key identifier for seed_profile, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const seed_profile&) {
    return "ores.iam.seed_profile";
}

}

#endif
