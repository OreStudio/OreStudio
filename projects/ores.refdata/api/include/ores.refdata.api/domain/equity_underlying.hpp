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
#ifndef ORES_REFDATA_API_DOMAIN_EQUITY_UNDERLYING_HPP
#define ORES_REFDATA_API_DOMAIN_EQUITY_UNDERLYING_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief An equity underlying a trade may name.
 *
 * The equity underlyings a population may draw from. An equity instrument
 * states underlying_name, documented as the ORE equity Name identifier, and
 * nothing held the set a tenant may use, so a generator that needed one had to
 * invent it.
 *
 * The code is the name the engine resolves, not a display name. ORE's own
 * example trades price underlyings as bare index names such as .SPX and
 * .STOXX50E, as exchange-suffixed shares such as MSFT.O and CR.N, and as
 * ISINs such as GB0005405286. The catalogue therefore stores the spelling a
 * document states, because that is the string the engine resolves.
 *
 * The same instrument appears under more than one spelling: .SPX and
 * RIC:.SPX are one index, and the catalogue holds both because a document may
 * state either. Resolving them to one instrument is what resolves_to is for: a
 * row that is the instrument leaves it empty, and a row that is an alias names
 * the instrument it resolves to. A population that read the two as two
 * instruments would price one index twice.
 *
 * The kind travels with the underlying because it decides what the trade is.
 * An option on an index and an option on a share are the same instrument type in
 * the store, but a book that claimed to trade single names while holding only
 * indices would misreport itself.
 *
 * This model holds the vocabulary only. The prices and the volatility surfaces an
 * equity prices on are market data, and they are not here.
 */
struct equity_underlying final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The underlying as the engine spells it, in the spelling a document states.
     *
     * Examples: '.SPX', 'RIC:.SPX', '.STOXX50E', 'MSFT.O', 'GB0005405286'.
     */
    std::string code;

    /**
     * @brief Human-readable name for the underlying.
     *
     * Examples: 'S&P 500 Index', 'Euro Stoxx 50', 'Microsoft Corporation'.
     */
    std::string name;

    /**
     * @brief What the underlying is.
     */
    std::string description;

    /**
     * @brief Whether the underlying is an index or a share.
     *
     * An index is a published level over many constituents, such as '.SPX'. A share is one listed
     * company, such as 'MSFT.O'. It is text and not a lookup because the two the tree distinguishes
     * are a fact of how the underlying is listed and not a vocabulary a tenant curates.
     */
    std::string instrument_type;

    /**
     * @brief The code of the instrument an alias names, empty when the row is the instrument
     * itself.
     *
     * 'RIC:.SPX' resolves to '.SPX'. It is a soft reference to this table and not a database
     * foreign key, because a catalogue is published as a set and a row that had to wait for the row
     * it points at would make the order of the seed a correctness requirement.
     */
    std::string resolves_to;

    /**
     * @brief The currency the underlying is quoted in, empty when the source does not say.
     *
     * It is a soft reference to ores_refdata_currencies_tbl and not a database foreign key, for the
     * reason the commodity catalogue states: a tenant holds the currencies it publishes, and a row
     * that could not be seeded until the currency list arrived would be unusable in an empty
     * database.
     */
    std::string currency_code;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this equity underlying.
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
    friend bool operator==(const equity_underlying&, const equity_underlying&) = default;
};

/**
 * @brief Dispatch-key identifier for equity_underlying, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const equity_underlying&) {
    return "ores.refdata.equity_underlying";
}

}

#endif
