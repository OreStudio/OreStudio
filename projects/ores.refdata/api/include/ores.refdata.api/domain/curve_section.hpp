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
#ifndef ORES_REFDATA_API_DOMAIN_CURVE_SECTION_HPP
#define ORES_REFDATA_API_DOMAIN_CURVE_SECTION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief One section of an ORE curveconfig.xml, with the element its entries use.
 *
 * The vocabulary a curveconfig.xml is built from. ORE ships seventy-six of
 * those documents, and across them nineteen sections appear: YieldCurves,
 * DefaultCurves, FXVolatilities, SwaptionVolatilities, Correlations and
 * the rest. Each is a container element holding entries of one kind, and the
 * entry element's name is not the container's -- YieldCurves holds
 * YieldCurve, Correlations holds Correlation.
 *
 * The section is a long-lived domain vocabulary of a fixed size, so it is a
 * table rather than a text column with a check: the curve tables that follow
 * reference it, the entry element it names is what the export writes back, and
 * adding a section is a row rather than a migration. This is the same choice the
 * report configuration registry made for every one of its discriminators.
 */
struct curve_section final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The section's element name in curveconfig.xml, exactly as ORE spells it:
     * 'YieldCurves', 'DefaultCurves', 'Correlations'.
     */
    std::string code;

    /**
     * @brief The element one entry of this section is written as: 'YieldCurve' for 'YieldCurves',
     * 'Correlation' for 'Correlations'. Export needs it, and it cannot be derived from code by
     * dropping the final 's' -- YieldVolatilities holds YieldVolatility, and FXVolatilities holds
     * FXVolatility.
     */
    std::string entry_element;

    /**
     * @brief What the section holds, in one line, for a reader who does not know ORE.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this curve section.
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
    friend bool operator==(const curve_section&, const curve_section&) = default;
};

/**
 * @brief Dispatch-key identifier for curve_section, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const curve_section&) {
    return "ores.refdata.curve_section";
}

}

#endif
