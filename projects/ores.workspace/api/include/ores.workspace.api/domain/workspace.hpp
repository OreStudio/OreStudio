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
#ifndef ORES_WORKSPACE_API_DOMAIN_WORKSPACE_HPP
#define ORES_WORKSPACE_API_DOMAIN_WORKSPACE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::workspace::domain {

/**
 * @brief Named, isolated data context for workspace-level operations.
 *
 * A named, isolated data context. Data a workspace does not carry is
 * resolved from its parent chain, up to the Live workspace
 * (ores_utility_live_workspace_id_fn()).
 *
 * The table is bi-temporal and audited (see
 * projects/ores.sql/create/workspace/workspace_create.sql): it carries
 * version, the four audit columns and the valid_from/valid_to pair
 * with the GIST exclusion and the delete rule, so the model takes the
 * ordinary audited shape and needs no shape flag.
 *
 * A workspace belongs to one tenant and one party, and its name is unique
 * within that pair. party_id is a natural key beside name for that
 * reason: two parties in one tenant may each hold a workspace named prod.
 * Only an active workspace holds its name, so the uniqueness index covers
 * the active rows and an archived name is free to be taken again.
 *
 * parent_workspace_id is a self-referencing soft foreign key, so the model
 * binds self-referencing-hierarchy for its UUID surrogate key, its tenant
 * scope and its self-reference. The same profile generates the recursive
 * subtree read over parent_workspace_id.
 *
 * scope_portfolio_id optionally narrows a workspace to one portfolio. It is
 * a soft foreign key with no declared target, because ores.refdata is
 * created after this component.
 *
 * The component's row-level security stays hand-written in
 * create/workspace/workspace_rls_policies_create.sql: the live fleet creates
 * this table before the iam section that defines
 * ores_iam_current_tenant_id_fn, so an inline policy cannot be emitted here.
 */
struct workspace final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID primary key.
     *
     * The Live workspace uses the sentinel ores_utility_live_workspace_id_fn(), one row per tenant.
     */
    boost::uuids::uuid id;

    /**
     * @brief Unique workspace name within its party.
     *
     * The Live workspace is named Live in every tenant.
     */
    std::string name;

    /**
     * @brief Party that owns this workspace.
     *
     * The Live workspace belongs to the system party of its tenant.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief UUID of the IAM account that owns this workspace.
     *
     * The account is looked up across every tenant, because tenant provisioning seeds a Live
     * workspace owned by the provisioning account rather than by an account of the tenant it is
     * creating.
     */
    boost::uuids::uuid owner_id;

    /**
     * @brief Optional free-text description.
     */
    std::string description = "";

    /**
     * @brief Optional path to the source data this workspace was created from.
     */
    std::string source_path;

    /**
     * @brief Optional parent workspace, which resolves this workspace's inherited data.
     *
     * NULL on the Live workspace, the root of every chain.
     */
    std::optional<boost::uuids::uuid> parent_workspace_id;

    /**
     * @brief Optional portfolio that scopes this workspace.
     */
    std::optional<boost::uuids::uuid> scope_portfolio_id;

    /**
     * @brief Lifecycle status: active or archived.
     *
     * An archived workspace keeps its row and its history.
     *
     * The synthetic generator states the value rather than sampling a word, because the table's
     * check constraint admits only these two.
     */
    std::string status_code = "active";

    /**
     * @brief Username of the person who last modified this workspace.
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
    friend bool operator==(const workspace&, const workspace&) = default;
};

/**
 * @brief Dispatch-key identifier for workspace, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const workspace&) {
    return "ores.workspace.workspace";
}

}

#endif
