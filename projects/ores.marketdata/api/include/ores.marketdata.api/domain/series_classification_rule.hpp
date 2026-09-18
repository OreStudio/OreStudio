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
#ifndef ORES_MARKETDATA_API_DOMAIN_SERIES_CLASSIFICATION_RULE_HPP
#define ORES_MARKETDATA_API_DOMAIN_SERIES_CLASSIFICATION_RULE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief How one ORE series type maps onto the refdata asset class and series subclass codes.
 *
 * The taxonomy rule for an ORE market data series, one row per series
 * type and metric. An import reads the whole table once, builds a
 * classifier from it, and asks that classifier for the asset classes and
 * the series subclass of every key it meets.
 *
 * The table replaces a compiled C++ map, so the taxonomy is now managed
 * like every other catalogue: a type ORE adds later, or one a user
 * brings, is an inserted row rather than a rebuild. This matters because
 * an import aborts on a type it cannot classify, so a compiled table
 * makes every new ORE type a code change.
 *
 * The class codes name rows in refdata.asset_class_code and the
 * subclass codes name rows in refdata.series_subclass_code, and the
 * insert trigger checks both against those catalogues.
 *
 * Two things a single class column could not hold, and how this table
 * holds them:
 *
 * - A pairwise correlation relates two classes and belongs to both, so
 *   neither is a property of the type. Its row names no class at all and
 *   its classes are read from the two operands of the key, which is what
 *   asset_class_source records.
 * - GENERIC-MD is a wrapper whose metric slot names the instrument
 *   inside it, so its rule is keyed by the metric rather than by the type
 *   alone.
 *
 * The reader rejects an empty table, because an empty table aborts the
 * first import that reads it and the message should name the table rather
 * than the first series it met.
 */
struct series_classification_rule final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The ORE key type: the first segment of a market data key, verbatim.
     *
     * Examples: 'DISCOUNT', 'SWAPTION', 'FIXING', 'CORRELATION'.
     */
    std::string series_type;

    /**
     * @brief The metric this rule applies to, or the empty string for a rule that applies to every
     * metric of its type.
     *
     * Most types classify the same way whatever their metric carries, so their row holds the empty
     * string. GENERIC-MD is the exception: its metric slot names the instrument inside it, so
     * GENERIC-MD/EQUITY_OPTION/PRICE/... is classified by the row whose metric is EQUITY_OPTION. A
     * lookup tries the exact metric first and falls back to the empty one.
     */
    std::string metric;

    /**
     * @brief How this rule obtains its asset classes: literal or correlation_operands.
     *
     * A literal rule names its class in asset_class_code. A correlation rule reads the classes from
     * the two operands at the front of the qualifier, because a pairwise correlation belongs to the
     * two classes it relates and to no class of its own.
     */
    std::string asset_class_source;

    /**
     * @brief Code of the asset class this type measures, from refdata.asset_class_code.
     *
     * Null on a correlation rule, which names no class of its own, and on a rule whose classes come
     * from the key. The column is nullable rather than empty because null is the honest "no class
     * here", and the insert trigger validates a code only where one is set.
     */
    std::optional<std::string> asset_class_code;

    /**
     * @brief Code of the series subclass this type belongs to, from refdata.series_subclass_code.
     *
     * Example: 'spot' for FX, 'index_fixing' for FIXING, 'correlation' for CORRELATION.
     */
    std::string series_subclass_code;

    /**
     * @brief Why the type carries this taxonomy, and the evidence behind the rule where the metric
     * decides it.
     *
     * Example: 'The two operands of a correlation key name the classes it relates; the subclass is
     * the correlation itself.'
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this series classification rule.
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
 * @brief Dispatch-key identifier for series_classification_rule, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const series_classification_rule&) {
    return "ores.marketdata.series_classification_rule";
}

}

#endif
