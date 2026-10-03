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
#ifndef ORES_REFDATA_API_DOMAIN_PORTFOLIO_RIGHT_HPP
#define ORES_REFDATA_API_DOMAIN_PORTFOLIO_RIGHT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief A right an account holds at a node of the portfolio tree.
 *
 * A portfolio right says that one account holds one named right at one node
 * of the [[id:282C87C1-11E1-42F0-BEE6-D6983A5F836B][portfolio]] tree. A right held at a node
 * applies to every node below it, so a right granted on a desk covers its sub-desks and not its
 * sibling desks.
 *
 * Two rights exist, the ones [[id:4AB0BC63-D73A-4FC3-B9AF-16C1BB90653F][sandboxes]] need: read, to
 * see what a node holds, and open_sandbox, to open a sandbox anchored at the node.
 * ores_refdata_account_holds_portfolio_right_fn answers whether an account
 * holds a right at a node, directly or through an ancestor.
 */
struct portfolio_right final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this grant.
     *
     * Surrogate key for the portfolio right record.
     */
    boost::uuids::uuid id;

    /**
     * @brief The account that holds the right.
     *
     * References the IAM accounts table.
     */
    boost::uuids::uuid account_id;

    /**
     * @brief The node the right is held at.
     *
     * The right also applies to every node below it.
     */
    boost::uuids::uuid portfolio_id;

    /**
     * @brief The right held.
     *
     * read or open_sandbox.
     */
    std::string right_code;

    /**
     * @brief Username of the person who last modified this portfolio right.
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
    friend bool operator==(const portfolio_right&, const portfolio_right&) = default;
};

/**
 * @brief Dispatch-key identifier for portfolio_right, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const portfolio_right&) {
    return "ores.refdata.portfolio_right";
}

}

#endif
