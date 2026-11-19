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
#ifndef ORES_ORE_CORE_DOMAIN_ENTRY_RESET_OPTION_PROJECTION_HPP
#define ORES_ORE_CORE_DOMAIN_ENTRY_RESET_OPTION_PROJECTION_HPP

#include "ores.ore.core/domain/decimal_projection.hpp"
#include "ores.ore.core/domain/scalar_terms_projection.hpp"
#include "ores.ore.core/domain/volatility_product_projection.hpp"
#include "ores.ore.core/xml/record.hpp"

/**
 * @file entry_reset_option_projection.hpp
 * @brief The elements ORE's equity and FX best-entry and strike-resettable
 * options state.
 *
 * Each element has one shape in both families, so one read and one write serve
 * the two families' rows. The schema requires every scalar, so the document
 * reaches the reader with them stated. The underlying and the observation dates
 * are the carrier's shared rows, which the families' tables read and write.
 */

namespace ores::ore::domain::entry_reset_option {

using volatility_product::decimal;
using volatility_product::read_date;
using volatility_product::read_decimal;
using volatility_product::write_date;
using volatility_product::write_decimal;

/// Reads a best-entry option's scalars onto the row.
template <typename Row>
void read_best_entry(const xml::record_reader& data, Row& row) {
    row.long_short = scalar_terms::read_text(data, "LongShort").value_or("");
    row.currency = scalar_terms::read_text(data, "Currency").value_or("");
    row.notional = read_decimal(data, "Notional").value_or(decimal{});
    row.multiplier = read_decimal(data, "Multiplier").value_or(decimal{});
    row.cap = read_decimal(data, "Cap").value_or(decimal{});
    row.strike = read_decimal(data, "Strike").value_or(decimal{});
    row.trigger_level = read_decimal(data, "TriggerLevel").value_or(decimal{});
    row.reset_minimum = read_decimal(data, "ResetMinimum").value_or(decimal{});
    row.premium = read_decimal(data, "Premium").value_or(decimal{});
    row.expiry_date = read_date(data, "ExpiryDate");
    row.strike_date = read_date(data, "StrikeDate");
    row.premium_date = read_date(data, "PremiumDate");
    row.settlement_date = read_date(data, "SettlementDate");
}

/// Writes a best-entry option's scalars into the element.
template <typename Row>
void write_best_entry(xml::record_writer& data, const Row& row) {
    scalar_terms::write_text(data, "LongShort", row.long_short);
    scalar_terms::write_text(data, "Currency", row.currency);
    write_decimal(data, "Notional", row.notional);
    write_decimal(data, "Multiplier", row.multiplier);
    write_decimal(data, "Cap", row.cap);
    write_decimal(data, "Strike", row.strike);
    write_decimal(data, "TriggerLevel", row.trigger_level);
    write_decimal(data, "ResetMinimum", row.reset_minimum);
    write_decimal(data, "Premium", row.premium);
    write_date(data, "ExpiryDate", row.expiry_date);
    write_date(data, "StrikeDate", row.strike_date);
    write_date(data, "PremiumDate", row.premium_date);
    write_date(data, "SettlementDate", row.settlement_date);
}

/// Reads a strike-resettable option's scalars onto the row.
template <typename Row>
void read_strike_resettable(const xml::record_reader& data, Row& row) {
    row.long_short = scalar_terms::read_text(data, "LongShort").value_or("");
    row.option_type = scalar_terms::read_text(data, "OptionType").value_or("");
    row.currency = scalar_terms::read_text(data, "Currency").value_or("");
    row.strike = read_decimal(data, "Strike").value_or(decimal{});
    row.reset_strike = read_decimal(data, "ResetStrike").value_or(decimal{});
    row.quantity = decimal_as_double(data, "Quantity").value_or(0.0);
    row.trigger_type = scalar_terms::read_text(data, "TriggerType").value_or("");
    row.trigger_price = read_decimal(data, "TriggerPrice").value_or(decimal{});
    row.expiry_date = read_date(data, "ExpiryDate");
    row.settlement_date = read_date(data, "SettlementDate");
    row.premium = read_decimal(data, "Premium").value_or(decimal{});
    row.premium_date = read_date(data, "PremiumDate");
}

/// Writes a strike-resettable option's scalars into the element.
template <typename Row>
void write_strike_resettable(xml::record_writer& data, const Row& row) {
    scalar_terms::write_text(data, "LongShort", row.long_short);
    scalar_terms::write_text(data, "OptionType", row.option_type);
    scalar_terms::write_text(data, "Currency", row.currency);
    write_decimal(data, "Strike", row.strike);
    write_decimal(data, "ResetStrike", row.reset_strike);
    set_decimal_from_double(data, "Quantity", row.quantity);
    scalar_terms::write_text(data, "TriggerType", row.trigger_type);
    write_decimal(data, "TriggerPrice", row.trigger_price);
    write_date(data, "ExpiryDate", row.expiry_date);
    write_date(data, "SettlementDate", row.settlement_date);
    write_decimal(data, "Premium", row.premium);
    write_date(data, "PremiumDate", row.premium_date);
}

}

#endif
