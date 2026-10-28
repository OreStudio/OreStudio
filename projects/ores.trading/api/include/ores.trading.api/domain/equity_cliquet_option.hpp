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
#ifndef ORES_TRADING_API_DOMAIN_EQUITY_CLIQUET_OPTION_HPP
#define ORES_TRADING_API_DOMAIN_EQUITY_CLIQUET_OPTION_HPP

#include "ores.trading.api/domain/equity_option_instrument.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <chrono>
#include <stdexcept>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Refuses to construct a product from facts that do not state one.
 */
class equity_cliquet_option_error final : public std::invalid_argument {
public:
    using std::invalid_argument::invalid_argument;
};

/**
 * @brief The facts an equity cliquet option states.
 *
 * The frequency has no default and no empty meaning: a cliquet observes
 * its own returns over periods, so a cliquet without a frequency is not a
 * cliquet. A caller therefore cannot reach the construction without stating
 * one.
 *
 * The terms are the cliquet's own. The generator's book, counterparty and
 * trade date are not here, because they describe the trade and not the
 * product.
 */
struct equity_cliquet_option_terms final {
    std::string underlying_name;
    std::string currency;
    ores::utility::decimal::decimal notional;
    std::string option_type;
    ores::utility::decimal::decimal strike;
    std::chrono::year_month_day expiry_date;
    std::string exercise_type;
    std::string long_short;
    std::string settlement_type;
    std::string frequency;
};

/**
 * @brief Constructs an equity cliquet option from the facts it states.
 *
 * The row this returns is the one the database accepts, so the rules the
 * schema states are refused here instead: a non-positive notional, a
 * negative strike, an empty underlying or currency, a vocabulary outside
 * the closed set the column holds, or a missing frequency. The SQL check
 * constraints stay as the backstop, but a generator no longer reaches them
 * with a combination that cannot be booked.
 *
 * The trade type code is fixed by this construction and is not a parameter,
 * so the caller cannot ask for a cliquet row stamped as another product.
 */
[[nodiscard]] inline equity_option_instrument
construct_equity_cliquet_option(const equity_cliquet_option_terms& terms) {
    const auto refuse = [](std::string_view fact) -> void {
        throw equity_cliquet_option_error("An equity cliquet option states " + std::string(fact) +
                                          ".");
    };
    const auto states_one_of = [](const std::string& value, std::initializer_list<std::string_view> allowed) {
        for (const auto candidate : allowed) {
            if (value == candidate)
                return true;
        }
        return false;
    };

    if (terms.underlying_name.empty())
        refuse("an underlying name");
    if (terms.currency.empty())
        refuse("a currency");
    if (!(terms.notional > ores::utility::decimal::decimal{}))
        refuse("a positive notional");
    if (terms.strike < ores::utility::decimal::decimal{})
        refuse("a non-negative strike");
    if (!states_one_of(terms.option_type, {"Call", "Put"}))
        refuse("a Call or Put option type");
    if (terms.expiry_date == std::chrono::year_month_day{})
        refuse("an expiry date");
    if (!states_one_of(terms.exercise_type, {"European", "Bermudan", "American"}))
        refuse("a European, Bermudan or American exercise type");
    if (!states_one_of(terms.long_short, {"Long", "Short"}))
        refuse("a Long or Short direction");
    if (!terms.settlement_type.empty() &&
        !states_one_of(terms.settlement_type, {"Cash", "Physical"}))
        refuse("a Cash or Physical settlement type where it states one");
    if (terms.frequency.empty())
        refuse("a frequency, because that is how often it observes");

    equity_option_instrument row;
    row.identity.trade_type_code = "EquityCliquetOption";
    row.underlying_name = terms.underlying_name;
    row.currency = terms.currency;
    row.notional = terms.notional;
    row.option_type = terms.option_type;
    row.strike = terms.strike;
    row.expiry_date = terms.expiry_date;
    row.exercise_type = terms.exercise_type;
    row.long_short = terms.long_short;
    row.settlement_type = terms.settlement_type;
    row.cliquet_frequency = terms.frequency;
    return row;
}

}

#endif
