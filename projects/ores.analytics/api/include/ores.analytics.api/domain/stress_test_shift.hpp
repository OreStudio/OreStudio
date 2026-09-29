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
#ifndef ORES_ANALYTICS_API_DOMAIN_STRESS_TEST_SHIFT_HPP
#define ORES_ANALYTICS_API_DOMAIN_STRESS_TEST_SHIFT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace ores::analytics::domain {

/**
 * @brief One shift a stress scenario applies to one market object.
 *
 * One entry of one shift block of a stress scenario: a market object, the kind of
 * shift applied to it, and the shift itself. The corpus uses fourteen of the
 * eighteen families the schema allows -- DiscountCurves, IndexCurves, FxSpots,
 * FxVolatilities, SwaptionVolatilities, CapFloorVolatilities and the rest -- in
 * three hundred and forty-eight entries.
 *
 * The families look different and are the same shape: an object named by one
 * attribute -- ccy, IndexCurve, FxVolatility, SwaptionVolatility -- a
 * ShiftType, and the shift as parallel lists of values and of tenors or
 * expiries. Fourteen tables would be fourteen copies of those five columns with
 * different attribute names. One table holds them all, with the family as a
 * column and the object's name in object_key, which is what makes adding a
 * family a seeded value rather than a table.
 *
 * The entries carry a few things besides the shift -- ShiftSize, IRCurves,
 * ShiftTerms -- and those are written into extras as a stated list of
 * name and value pairs, because they differ from family to family and none is
 * common to all.
 *
 * Each row belongs to exactly one scenario, in the order the document wrote it.
 */
struct stress_test_shift final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate key for the shift.
     */
    boost::uuids::uuid id;

    /**
     * @brief The scenario this shift belongs to.
     */
    boost::uuids::uuid stress_test_scenario_id;

    /**
     * @brief The shift family's element name in the document: DiscountCurves, FxSpots and so on.
     */
    std::string family;

    /**
     * @brief The market object the shift applies to, as the entry's own attribute names it.
     */
    std::string object_key;

    /**
     * @brief How the shift is applied -- Absolute, Relative, EqualTo or another of ORE's spellings.
     */
    std::optional<std::string> shift_type;

    /**
     * @brief The shift values, comma separated, as ORE writes them.
     */
    std::optional<std::string> shifts;

    /**
     * @brief The tenors the shifts apply at, comma separated.
     */
    std::optional<std::string> shift_tenors;

    /**
     * @brief The expiries the shifts apply at, comma separated, for the families that shift by
     * expiry rather than by tenor.
     */
    std::optional<std::string> shift_expiries;

    /**
     * @brief The entry's other elements, one per semicolon, each a pipe-separated name and value.
     */
    std::optional<std::string> extras;

    /**
     * @brief The order the document wrote the shift in.
     */
    int position = 0;

    /**
     * @brief Username of the person who last modified this stress test shift.
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
    friend bool operator==(const stress_test_shift&, const stress_test_shift&) = default;
};

/**
 * @brief Dispatch-key identifier for stress_test_shift, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const stress_test_shift&) {
    return "ores.analytics.stress_test_shift";
}

}

#endif
