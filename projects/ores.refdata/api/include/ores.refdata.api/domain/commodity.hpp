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
#ifndef ORES_REFDATA_API_DOMAIN_COMMODITY_HPP
#define ORES_REFDATA_API_DOMAIN_COMMODITY_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief A commodity a trade may name.
 *
 * The commodities a population may draw from. A commodity trade names its
 * commodity in free text today, and nothing holds the set a tenant may use, so
 * a generator that needs one has to invent it.
 *
 * The code is the name the engine resolves, not a display name: ORE's own
 * index corpus and example trades spell a commodity as an
 * exchange-prefixed index, such as NYMEX:CL, ICE:B, XLME:AH or
 * PM:XAUUSD. The catalogue therefore stores that spelling as its key.
 *
 * The unit and the quotation currency travel with the commodity because they
 * are facts about how it trades and not about a particular trade: WTI is
 * quoted in barrels for dollars, gas in MMBtu for dollars, LME aluminium in
 * tonnes, gold in ounces. A population that quoted gold per barrel would be
 * worse than one with no commodity at all.
 *
 * This model holds the vocabulary only. The forward curves a commodity prices
 * on are market data, and they are not here.
 */
struct commodity final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The commodity as the engine spells it, exchange prefix included.
     *
     * Examples: 'NYMEX:CL', 'ICE:B', 'XLME:AH', 'PM:XAUUSD'.
     */
    std::string code;

    /**
     * @brief Human-readable name for the commodity.
     *
     * Examples: 'WTI Crude Oil', 'Brent Crude Oil', 'LME Aluminium', 'Gold Spot'.
     */
    std::string name;

    /**
     * @brief What the commodity is and where it is traded.
     */
    std::string description;

    /**
     * @brief The unit the commodity is quoted in, in the spelling the market uses.
     *
     * Examples: 'bbl' for a barrel of crude, 'MMBtu' for a million British thermal units of gas,
     * 't' for a tonne of metal or coal, 'oz' for a troy ounce of gold. It is text and not a lookup
     * because the unit a commodity quotes in is part of how the exchange lists it, and the set of
     * units is the exchanges' rather than ours.
     */
    std::string unit_code;

    /**
     * @brief The currency the commodity's price is quoted in. It is a soft reference to
     * ores_refdata_currencies_tbl and not a database foreign key: a tenant holds the currencies it
     * publishes, and a catalogue row that could not be seeded until that tenant's currency list
     * arrived would be unusable in an empty database.
     */
    std::string quotation_currency_code;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this commodity.
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
    friend bool operator==(const commodity&, const commodity&) = default;
};

/**
 * @brief Dispatch-key identifier for commodity, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const commodity&) {
    return "ores.refdata.commodity";
}

}

#endif
