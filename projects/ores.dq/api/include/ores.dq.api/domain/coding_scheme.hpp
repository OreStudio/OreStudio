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
#ifndef ORES_DQ_API_DOMAIN_CODING_SCHEME_HPP
#define ORES_DQ_API_DOMAIN_CODING_SCHEME_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <string>
#include <string_view>

namespace ores::dq::domain {

/**
 * @brief A published code list a subject area's values come from.
 *
 * A published code list that a subject area's values come from, such as an ISO
 * currency list or an FpML codelist.
 *
 * The scheme names the authority that publishes it and the subject area it
 * applies to, and may point at the document that defines it. The authority and
 * the subject-area columns are plain text rather than declared foreign keys,
 * because the table has never carried a constraint and adding one would refuse
 * rows the platform accepts today.
 */
struct coding_scheme final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique code identifying this coding scheme.
     *
     * Examples: 'ISO_4217', 'FpML_currency'.
     */
    std::string code;

    /**
     * @brief Human-readable name for the coding scheme.
     */
    std::string name;

    /**
     * @brief The authority that publishes the scheme.
     */
    std::string authority_type;

    /**
     * @brief The subject area the scheme's values belong to.
     */
    std::string subject_area_name;

    /**
     * @brief The domain half of the subject area's composite key.
     */
    std::string domain_name;

    /**
     * @brief Where the published list is defined, when there is a document for it.
     */
    std::string uri;

    /**
     * @brief Detailed description of the scheme and what its codes mean.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this coding scheme.
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
    friend bool operator==(const coding_scheme&, const coding_scheme&) = default;
};

/**
 * @brief Dispatch-key identifier for coding_scheme, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const coding_scheme&) {
    return "ores.dq.coding_scheme";
}

}

#endif
