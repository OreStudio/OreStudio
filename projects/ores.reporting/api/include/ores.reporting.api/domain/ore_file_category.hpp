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
#ifndef ORES_REPORTING_API_DOMAIN_ORE_FILE_CATEGORY_HPP
#define ORES_REPORTING_API_DOMAIN_ORE_FILE_CATEGORY_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief A group of the files of an ORE run.
 *
 * Reference data grouping the files of an ORE run, so a screen can show the market
 * data apart from the configuration and the risk reports apart from the XVA
 * reports. Each ORE file belongs to one category.
 *
 * The categories a run is given follow the chapters of the ORE user guide: the
 * master input, the configuration documents, the trade data and the market data.
 * The categories ORE writes follow the three steps of the guide's ORE Data Flow
 * chapter, with the reports a person reads kept apart from the data files one
 * analytic writes for another. One category is ORE Studio's own: the archives a run
 * travels in to the engine and back.
 */
struct ore_file_category final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The stable code of the category, in lower case with underscores: configuration,
     * market_data, valuation_report.
     */
    std::string code;

    /**
     * @brief The name of the category as a screen shows it, for example Market Data.
     */
    std::string name;

    /**
     * @brief Where the files of the category come from: supplied when a run is given them, produced
     * when ORE writes them, packaged when ORE Studio makes them.
     */
    std::string origin;

    /**
     * @brief What the files of the category have in common and what a person uses them for.
     */
    std::string description;

    /**
     * @brief The order the category appears in a list.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this ORE file category.
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
    friend bool operator==(const ore_file_category&, const ore_file_category&) = default;
};

/**
 * @brief Dispatch-key identifier for ore_file_category, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const ore_file_category&) {
    return "ores.reporting.ore_file_category";
}

}

#endif
