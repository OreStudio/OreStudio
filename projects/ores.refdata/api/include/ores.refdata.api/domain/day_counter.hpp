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
#ifndef ORES_REFDATA_API_DOMAIN_DAY_COUNTER_HPP
#define ORES_REFDATA_API_DOMAIN_DAY_COUNTER_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief One spelling of a day counter that an ORE document may write.
 *
 * ORE names a day counter by text, and its schema accepts about seventy
 * spellings: A365, A365F, Actual/365 (Fixed) and ACT/365.FIXED are four
 * of them. The curve configuration corpus writes nine distinct spellings, and five
 * of them have no row in day_count_fraction_type, whose codes are canonical
 * names rather than ORE's text.
 *
 * A curve keeps the spelling its document used, so the export writes it back
 * unchanged, and refers to this table by it, so an unknown spelling is refused.
 * The set is the schema's dayCounter enumeration in ore_types.xsd. Which
 * canonical day count fraction each spelling means is not recorded here: nothing
 * reads it yet, and a wrong mapping would be worse than none.
 */
struct day_counter final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The day counter exactly as an ORE document may write it: 'A365', 'Actual/365 (Fixed)',
     * 'ACT/ACT', '30E/360'. One row per spelling, because ORE accepts several spellings of one day
     * counter and the export writes back the one the document used.
     */
    std::string code;

    /**
     * @brief What the spelling means, in one line, for a reader who does not know ORE.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this day counter.
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
    friend bool operator==(const day_counter&, const day_counter&) = default;
};

/**
 * @brief Dispatch-key identifier for day_counter, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const day_counter&) {
    return "ores.refdata.day_counter";
}

}

#endif
