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
#ifndef ORES_TRADING_API_DOMAIN_EQUITY_POSITION_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_EQUITY_POSITION_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Equity Position instrument.
 *
 * Represents EquityPosition and EquityOptionPosition trades: a plain
 * equity holding or an equity option position. quantity is the number of
 * shares or contracts and stays on this row, because it belongs to the
 * position and not to one underlying. An option position's members are not
 * a column: each is a row of
 * ores.trading.equity_position_option_underlying, keyed to this
 * instrument and its ordinal in the document. A text column held the list
 * as JSON and could not be typed, indexed or questioned, so the collection
 * is a child table now.
 */
struct equity_position_instrument final {
    instrument_identity identity;

    /**
     * @brief Name of the underlying equity.
     */
    std::string underlying_name;

    /**
     * @brief ISO 4217 currency code.
     *
     * Soft FK to ores_refdata_currencies_tbl: ISO 4217 currency codes belong to ores.refdata, so
     * the dependency is recorded rather than copied. PR 4 tightens the soft reference into a real
     * foreign key.
     *
     * e.g., USD, EUR, GBP.
     */
    std::string currency;

    /**
     * @brief Number of shares / contracts held.
     */
    double quantity = 0.0;

    /**
     * @brief Entry price; absent for market-price positions.
     */
    std::optional<ores::utility::decimal::decimal> price;

    /**
     * @brief Optional free-text description.
     *
     * Human-readable notes about this instrument.
     */
    std::string description;

    ores::dq::domain::audit_record audit;
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
    friend bool operator==(const equity_position_instrument&,
                           const equity_position_instrument&) = default;
};

/**
 * @brief Dispatch-key identifier for equity_position_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const equity_position_instrument&) {
    return "ores.trading.equity_position_instrument";
}

}

#endif
