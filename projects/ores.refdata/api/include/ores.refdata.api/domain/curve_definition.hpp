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
#ifndef ORES_REFDATA_API_DOMAIN_CURVE_DEFINITION_HPP
#define ORES_REFDATA_API_DOMAIN_CURVE_DEFINITION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief One curve recipe in an ORE curveconfig.xml, with the settings common to every section.
 *
 * One curve entry of one section of a curveconfig.xml. Seventy-six of those
 * documents ship and hold two thousand eight hundred and fifty-three entries
 * across nineteen sections.
 *
 * Every entry, whatever its section, opens the same way -- a CurveId and a
 * CurveDescription occur once per entry in the corpus, and the settings that
 * follow are drawn from a much smaller vocabulary than the sections suggest:
 * Currency in two thousand one hundred and ninety-two entries, DiscountCurve
 * in one thousand two hundred and eighty-nine, DayCounter in one thousand two
 * hundred and seventy-five, InterpolationMethod in seven hundred and
 * thirty-four. Those are columns. What is left over differs by section --
 * ImplyDefaultFromMarket and RecoveryRate belong to default curves,
 * ExerciseStyle to equity curves, BootstrapConfig to most of them -- and is
 * written into extras as a stated list of name and value pairs, so a section
 * the mapper has not read yet is carried rather than dropped.
 *
 * The lists an entry may hold -- Segments, Quotes, Pillars,
 * Configurations -- are ordered and heterogeneous and belong in child tables
 * that follow this one. This table is the entry's own settings.
 *
 * The natural key is the section and the curve id together: EUR-ESTR names a
 * curve inside one section, and nothing stops another section naming its own.
 */
struct curve_definition final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate key for the curve entry.
     */
    boost::uuids::uuid id;

    /**
     * @brief The section the entry belongs to, as curve_section.code names it. Half of the natural
     * key: two sections may hold a curve of the same id.
     *
     * References ores_refdata_curve_sections_tbl.code (soft FK).
     */
    std::string section_code;

    /**
     * @brief ORE's CurveId, the name the document gives the curve and the name every other part of
     * the configuration refers to it by. The other half of the natural key.
     */
    std::string curve_id;

    /**
     * @brief ORE's CurveDescription, a free line about the curve. It occurs once per entry in the
     * corpus but is not required by the schema.
     */
    std::optional<std::string> description;

    /**
     * @brief The curve's currency, as the document spells it. Held as text so the export writes
     * back exactly what the document held.
     */
    std::optional<std::string> currency;

    /**
     * @brief The day counter the curve is built on. The sections name the same idea three ways --
     * DayCounter, YieldCurveDayCounter, DayCountConvention -- and this column holds whichever
     * spelling the entry's section uses, because the reader and the writer both know the section.
     */
    std::optional<std::string> day_counter;

    /**
     * @brief How the curve is interpolated between its pillars.
     */
    std::optional<std::string> interpolation_method;

    /**
     * @brief What the interpolation is a function of -- the tenor, the date, or the discount
     * factor.
     */
    std::optional<std::string> interpolation_variable;

    /**
     * @brief Whether and how the curve is extrapolated beyond its last pillar.
     */
    std::optional<std::string> extrapolation;

    /**
     * @brief The bootstrap tolerance, kept as the document's own spelling of the number: carrying
     * it as text means the export cannot turn 0.0001 into 9.999999999999999e-05.
     */
    std::optional<std::string> tolerance;

    /**
     * @brief The entry's remaining settings, one per semicolon, each a pipe-separated name and
     * value. They differ by section and none is common to all, so a section whose reader has not
     * been written yet still round trips.
     */
    std::optional<std::string> extras;

    /**
     * @brief The order the document wrote the entry in. The store does not order by it, and the
     * export restores the document's order from it.
     */
    int position = 0;

    /**
     * @brief Username of the person who last modified this curve definition.
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
    friend bool operator==(const curve_definition&, const curve_definition&) = default;
};

/**
 * @brief Dispatch-key identifier for curve_definition, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const curve_definition&) {
    return "ores.refdata.curve_definition";
}

}

#endif
