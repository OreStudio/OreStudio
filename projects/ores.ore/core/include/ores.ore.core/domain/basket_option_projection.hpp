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
#ifndef ORES_ORE_CORE_DOMAIN_BASKET_OPTION_PROJECTION_HPP
#define ORES_ORE_CORE_DOMAIN_BASKET_OPTION_PROJECTION_HPP

#include "ores.ore.core/domain/option_block_projection.hpp"
#include "ores.ore.core/domain/scalar_terms_projection.hpp"
#include "ores.ore.core/xml/record.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <string>
#include <string_view>

/**
 * @file basket_option_projection.hpp
 * @brief The element ORE's equity and FX basket and rainbow options share.
 *
 * The four products state one shape, so one read and one write serve the two
 * families' rows. The row keeps the element's scalars; the members, the
 * OptionData block and the observation dates are the carrier's shared rows,
 * which the families' tables read and write.
 */

namespace ores::ore::domain::basket_option {

using ores::utility::decimal::decimal;

/// Whether the trade type is one of the four products this element serves.
inline bool is_basket_product(std::string_view trade_type) {
    return trade_type == "EquityBasketOption" || trade_type == "EquityRainbowOption" ||
           trade_type == "FxBasketOption" || trade_type == "FxRainbowOption";
}

/**
 * @brief Reads the element's scalars onto the row.
 *
 * The schema requires the currency and the notional, and the strike on a
 * rainbow, so the document reaches the reader with them stated.
 */
template <typename Row>
void read(const xml::record_reader& data, Row& row) {
    row.currency = scalar_terms::read_text(data, "Currency").value_or("");
    if (const auto notional =
            decimal::from_string(scalar_terms::read_text(data, "Notional").value_or("")))
        row.notional = *notional;
    if (const auto strike = scalar_terms::read_text(data, "Strike"))
        if (const auto parsed = decimal::from_string(*strike))
            row.strike = *parsed;
    if (const auto settlement = scalar_terms::read_text(data, "Settlement"))
        row.settlement_date = option_block::to_optional_domain_date(*settlement);
}

/// The exercise dates the option rows keep, which the row has no column for.
template <typename Data>
void write_exercise_dates(xml::record_writer& option, const Data& carrier) {
    const auto dates = option_block::exercise_dates_of(carrier);
    if (dates.empty())
        return;
    auto list = option.child("ExerciseDates");
    for (const auto& date : dates)
        list.append_value("ExerciseDate", option_block::to_ore_date(date));
}

/// Writes the row's scalars into the element.
template <typename Row>
void write(xml::record_writer& data, const Row& row) {
    scalar_terms::write_text(data, "Currency", row.currency);
    scalar_terms::write_text(data, "Notional", row.notional.to_string());
    if (row.strike)
        scalar_terms::write_text(data, "Strike", row.strike->to_string());
    if (row.settlement_date)
        scalar_terms::write_text(
            data, "Settlement", option_block::to_ore_date(*row.settlement_date));
}

}

#endif
