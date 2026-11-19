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
#ifndef ORES_ORE_CORE_DOMAIN_BARRIER_PRODUCT_PROJECTION_HPP
#define ORES_ORE_CORE_DOMAIN_BARRIER_PRODUCT_PROJECTION_HPP

#include "ores.ore.core/domain/decimal_projection.hpp"
#include "ores.ore.core/domain/option_block_projection.hpp"
#include "ores.ore.core/domain/scalar_terms_projection.hpp"
#include "ores.ore.core/domain/volatility_product_projection.hpp"
#include "ores.ore.core/xml/record.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

/**
 * @file barrier_product_projection.hpp
 * @brief The scalars ORE's window and generic barrier options state.
 *
 * A window barrier option has one shape in the equity and FX families, so one
 * read and one write serve both families' rows. The option data, the barriers,
 * the barrier schedule and the underlying are the carrier's shared rows, which
 * the families' tables read and write.
 */

namespace ores::ore::domain::barrier_product {

using volatility_product::decimal;
using volatility_product::read_date;
using volatility_product::read_decimal;
using volatility_product::write_date;
using volatility_product::write_decimal;

/**
 * @brief Whether the product keeps its exercise date only as an exercise date
 * row, with no expiry column of its own.
 *
 * The families write such a product's ExerciseDates from the rows.
 */
inline bool keeps_exercise_dates(std::string_view trade_type) {
    return trade_type == "EquityWindowBarrierOption" || trade_type == "FxWindowBarrierOption" ||
           trade_type == "EquityGenericBarrierOption";
}

inline std::optional<std::chrono::year_month_day> read_optional_date(const xml::record_reader& data,
                                                                     std::string_view element) {
    if (const auto text = scalar_terms::read_text(data, element))
        return option_block::to_optional_domain_date(*text);
    return std::nullopt;
}

inline void write_optional_text(xml::record_writer& data,
                                std::string_view element,
                                const std::optional<std::string>& value) {
    if (value)
        scalar_terms::write_text(data, element, *value);
}

/**
 * @brief Reads a window barrier option's scalars onto the row.
 *
 * The strike is a plain =Strike=, or the =Value= of a =StrikeData= that may
 * name its currency.
 */
template <typename Row>
void read_window_barrier(const xml::record_reader& data, Row& row) {
    row.currency = scalar_terms::read_text(data, "Currency").value_or("");
    row.fixing_amount = read_decimal(data, "FixingAmount").value_or(decimal{});
    if (const auto strike = read_decimal(data, "Strike")) {
        row.strike = *strike;
    } else if (const auto strike_data = data.child("StrikeData")) {
        row.strike = read_decimal(*strike_data, "Value").value_or(decimal{});
        row.strike_currency = scalar_terms::read_text(*strike_data, "Currency");
    }
    row.start_date = read_date(data, "StartDate");
    row.end_date = read_date(data, "EndDate");
}

/// Writes a window barrier option's scalars into the element.
template <typename Row>
void write_window_barrier(xml::record_writer& data, const Row& row) {
    scalar_terms::write_text(data, "Currency", row.currency);
    write_decimal(data, "FixingAmount", row.fixing_amount);
    if (row.strike_currency) {
        auto strike = data.child("StrikeData");
        write_decimal(strike, "Value", row.strike);
        scalar_terms::write_text(strike, "Currency", *row.strike_currency);
    } else {
        scalar_terms::write_text(data, "Strike", row.strike.to_string());
    }
    write_date(data, "StartDate", row.start_date);
    write_date(data, "EndDate", row.end_date);
}

/// Reads a generic barrier option's scalars, and those its Barriers block states, onto the row.
template <typename Row>
void read_generic_barrier(const xml::record_reader& data, Row& row) {
    row.pay_currency = scalar_terms::read_text(data, "PayCurrency").value_or("");
    row.quantity = decimal_as_double(data, "Quantity");
    row.strike = read_decimal(data, "Strike");
    row.amount = read_decimal(data, "Amount");
    row.settlement_date = read_optional_date(data, "SettlementDate");
    row.settlement_lag = scalar_terms::read_text(data, "SettlementLag");
    row.settlement_calendar = scalar_terms::read_text(data, "SettlementCalendar");
    row.settlement_convention = scalar_terms::read_text(data, "SettlementConvention");
    if (const auto barriers = data.child("Barriers")) {
        row.barrier_start_date = read_optional_date(*barriers, "StartDate");
        row.barrier_end_date = read_optional_date(*barriers, "EndDate");
        row.kiko_type = scalar_terms::read_text(*barriers, "KikoType");
    }
}

/// Writes a generic barrier option's scalars, and those of its Barriers block.
template <typename Row>
void write_generic_barrier(xml::record_writer& data, const Row& row) {
    scalar_terms::write_text(data, "PayCurrency", row.pay_currency);
    if (row.quantity)
        set_decimal_from_double(data, "Quantity", *row.quantity);
    write_decimal(data, "Strike", row.strike);
    write_decimal(data, "Amount", row.amount);
    if (row.settlement_date)
        write_date(data, "SettlementDate", *row.settlement_date);
    write_optional_text(data, "SettlementLag", row.settlement_lag);
    write_optional_text(data, "SettlementCalendar", row.settlement_calendar);
    write_optional_text(data, "SettlementConvention", row.settlement_convention);
    if (row.barrier_start_date || row.barrier_end_date || row.kiko_type) {
        auto barriers = data.child("Barriers");
        if (row.barrier_start_date)
            write_date(barriers, "StartDate", *row.barrier_start_date);
        if (row.barrier_end_date)
            write_date(barriers, "EndDate", *row.barrier_end_date);
        write_optional_text(barriers, "KikoType", row.kiko_type);
    }
}

}

#endif
