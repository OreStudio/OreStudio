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
#ifndef ORES_REFDATA_API_DOMAIN_SANDBOX_HPP
#define ORES_REFDATA_API_DOMAIN_SANDBOX_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief A space where users import and experiment without touching official data.
 *
 * A sandbox is a space where actions have no effect outside it: users import
 * samples, try what-ifs and run reports there without touching official data
 * (see [[id:4AB0BC63-D73A-4FC3-B9AF-16C1BB90653F][Sandbox]]).
 *
 * The anchor is the node of the official [[id:282C87C1-11E1-42F0-BEE6-D6983A5F836B][portfolio]]
 * tree the sandbox belongs to. It decides who may open the sandbox and who may see it; it does not
 * place the sandbox in the official tree. The sandbox's own portfolios carry its id, and a
 * portfolio always shares its parent's sandbox, so no official portfolio contains a sandbox
 * portfolio.
 *
 * Opening a sandbox needs the open_sandbox [[id:F783F38A-C123-489A-881D-5BCB5DE6D05D][portfolio
 * right]] at an official anchor; the check runs again when the owner changes.
 * ores_refdata_account_sees_sandbox_fn answers whether an account may see a
 * sandbox: its owner always may; anyone with read at the anchor may when it
 * is shared; its [[id:6B3A0A06-EE24-411F-BF99-BBEF04A43E06][members]] may when it is shared with
 * members. A restrictive row-level security policy on portfolios applies it to the session's actor
 * through ores_refdata_actor_sees_sandbox_fn, so a sandbox's portfolios are
 * read and written only by those who may see the sandbox, and a session with
 * no actor sees none of them.
 *
 * Closing a sandbox does not close its portfolios, as for every soft foreign
 * key in the schema. Handing a sandbox to a new owner needs the right at the
 * anchor on the new owner's side; the new owner's consent is not recorded.
 */
struct sandbox final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this sandbox.
     *
     * Surrogate key for the sandbox record.
     */
    boost::uuids::uuid id;

    /**
     * @brief The sandbox's name.
     *
     * Unique within the tenant.
     */
    std::string name;

    /**
     * @brief What the sandbox is for.
     *
     * experiment, sample or import.
     */
    std::string purpose;

    /**
     * @brief The official portfolio node the sandbox belongs to.
     *
     * Fixed for the sandbox's life.
     */
    boost::uuids::uuid anchor_portfolio_id;

    /**
     * @brief The account answerable for the sandbox.
     *
     * It must hold open_sandbox at the anchor.
     */
    boost::uuids::uuid owner_account_id;

    /**
     * @brief Who may see the sandbox.
     *
     * private to the owner; shared with everyone who may read the anchor; members for the named
     * members.
     */
    std::string visibility;

    /**
     * @brief Whether the sandbox is in use.
     *
     * open, or archived and read-only.
     */
    std::string status;

    /**
     * @brief When the owner must confirm the sandbox is still needed.
     */
    std::chrono::year_month_day review_date;

    /**
     * @brief Optional description of the sandbox.
     */
    std::optional<std::string> description;

    /**
     * @brief Username of the person who last modified this sandbox.
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
    friend bool operator==(const sandbox&, const sandbox&) = default;
};

/**
 * @brief Dispatch-key identifier for sandbox, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const sandbox&) {
    return "ores.refdata.sandbox";
}

}

#endif
