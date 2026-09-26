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
#ifndef ORES_COMPUTE_API_DOMAIN_PLATFORM_HPP
#define ORES_COMPUTE_API_DOMAIN_PLATFORM_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <string>
#include <string_view>

namespace ores::compute::domain {

/**
 * @brief A compute target triplet an engine package is built for.
 *
 * A target a wrapper and engine can be built for, named by its triplet code
 * (for example x64-linux). A package belongs to an app version and a
 * platform, so the orchestrator matches incoming workunits against the host's
 * triplet through this row.
 *
 * The platform registry is a global one: the platform rows ship under the
 * system tenant and every tenant reads them, which
 * :system_tenant_visible: true states on the repository.
 *
 * is_active retires a triplet without deleting it, so a package that names
 * it still resolves. It is a plain column and not a declared filter, because
 * the generated read refuses a filter, and a filter the service rejects is
 * worse than no filter at all.
 */
struct platform final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID primary key for the platform.
     */
    boost::uuids::uuid id;

    /**
     * @brief Triplet code, e.g. 'x64-linux'. This is the key callers address the row by.
     */
    std::string code;

    /**
     * @brief Human-readable name, e.g. 'Linux x86-64'.
     */
    std::string display_name;

    /**
     * @brief What the triplet targets.
     */
    std::string description;

    /**
     * @brief Operating system family: linux, macos or windows.
     */
    std::string os_family;

    /**
     * @brief CPU architecture: x86_64 or aarch64.
     */
    std::string cpu_arch;

    /**
     * @brief Application binary interface, where the triplet distinguishes one. Empty for a triplet
     * that does not.
     */
    std::string abi;

    /**
     * @brief Whether the triplet is offered to hosts. A retired triplet keeps its row so packages
     * that name it still resolve.
     */
    bool is_active = false;

    /**
     * @brief Username of the person who last modified this compute platform.
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
    friend bool operator==(const platform&, const platform&) = default;
};

/**
 * @brief Dispatch-key identifier for platform, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const platform&) {
    return "ores.compute.platform";
}

}

#endif
