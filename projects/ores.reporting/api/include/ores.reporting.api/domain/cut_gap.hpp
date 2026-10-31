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
#ifndef ORES_REPORTING_API_DOMAIN_CUT_GAP_HPP
#define ORES_REPORTING_API_DOMAIN_CUT_GAP_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief A part of a cut's body the assembly could not fill.
 *
 * One part of a cut's body that the assembly could not fill: the part, the item
 * inside it and the reason it could not be filled.
 *
 * A cut that fixed the data and omitted a part it could not read would look
 * smaller than it is. The gap list is the answer to that: the body that could not
 * be assembled is a fact a person acts on, and the cut's own page is where the
 * list is read. A take that could not close a gap saves the attempt as an
 * incomplete cut and raises no approval request, because a decider would be asked
 * to sign a body that does not meet the definition.
 *
 * The parts are the four inputs the body holds: the market data, the deal
 * population, the reference data, and the engine and the code.
 */
struct cut_gap final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID identifying this gap.
     */
    boost::uuids::uuid id;

    /**
     * @brief The cut whose body could not be filled.
     */
    boost::uuids::uuid cut_id;

    /**
     * @brief The part of the body the gap belongs to: market_data, deal_population, reference_data
     * or engine_code.
     *
     * A closed set and not a word, so a synthetic value takes no uniqueness suffix: the row is made
     * unique by the cut it hangs off, and a suffixed part would name a part the body does not have.
     */
    std::string part_code;

    /**
     * @brief The item inside the part that could not be filled: a series with no value at or before
     * the instant, a deal at no version, a curve nobody has approved.
     */
    std::string item;

    /**
     * @brief The party the cut belongs to, set by the take from the cut it records.
     *
     * Row level security reads it, so a session sees the gaps of the parties it works in.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Why the part could not be filled, in the words the person who closes it reads.
     */
    std::string reason;

    /**
     * @brief Username of the person who last modified this cut gap.
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
    friend bool operator==(const cut_gap&, const cut_gap&) = default;
};

/**
 * @brief Dispatch-key identifier for cut_gap, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const cut_gap&) {
    return "ores.reporting.cut_gap";
}

}

#endif
