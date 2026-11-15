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
#ifndef ORES_ORE_CORE_DOMAIN_UNDERLYING_BLOCK_PROJECTION_HPP
#define ORES_ORE_CORE_DOMAIN_UNDERLYING_BLOCK_PROJECTION_HPP

#include "ores.ore.core/domain/decimal_projection.hpp"
#include "ores.ore.core/domain/index_reference.hpp"
#include "ores.ore.core/domain/option_block_projection.hpp"
#include "ores.ore.core/xml/binding.hpp"
#include "ores.trading.api/domain/instrument_underlying.hpp"
#include <algorithm>
#include <cstddef>
#include <string>
#include <string_view>
#include <vector>

/**
 * @file underlying_block_projection.hpp
 * @brief ORE's Underlying block, read into and written from the shared
 * underlying rows, for any family whose composite holds them.
 *
 * ORE's underlying type states a type and a name, and optionally the
 * identifier type, currency, exchange, weight, price type and commodity
 * future terms. A product states it once, as a list directly under the
 * product, or as a list inside an Underlyings element. Each block is an
 * instrument_underlying row, and the block asks the holder's record type
 * whether the holder states one block or a list.
 */

namespace ores::ore::domain::underlying_block {

using trading::domain::instrument_underlying;
using xml::record_reader;
using xml::record_writer;

/// Whether the holder states the element as a list rather than once.
template <typename Record>
bool states_a_list(const Record& holder, std::string_view element) {
    const auto* f = xml::find_field(holder.type(), element, false);
    return f != nullptr && f->kind == xml::field_kind::repeated_nested;
}

inline auto optional_text(const record_reader& block,
                          std::string_view name) -> std::optional<std::string> {
    if (!block.declares(name) || !block.has(name))
        return std::nullopt;
    return block.text(name);
}

inline auto optional_integer(const record_reader& block,
                             std::string_view name) -> std::optional<int> {
    if (!block.declares(name) || !block.has(name))
        return std::nullopt;
    if (const auto value = block.integer(name))
        return static_cast<int>(*value);
    return std::nullopt;
}

inline auto optional_decimal(const record_reader& block, std::string_view name)
    -> std::optional<ores::utility::decimal::decimal> {
    if (!block.declares(name) || !block.has(name))
        return std::nullopt;
    return block.decimal(name);
}

template <typename Data>
void read_underlying(Data& result,
                     const record_reader& block,
                     std::string_view role,
                     int sequence_number) {
    instrument_underlying row;
    row.underlying_role = std::string(role);
    row.sequence_number = sequence_number;
    row.underlying_type = optional_text(block, "Type").value_or("");
    row.name =
        underlying_address_of(row.underlying_type, optional_text(block, "Name").value_or(""));
    row.identifier_type = optional_text(block, "IdentifierType");
    row.currency = optional_text(block, "Currency");
    row.exchange = optional_text(block, "Exchange");
    row.weight = decimal_as_double(block, "Weight");
    row.price_type = optional_text(block, "PriceType");
    row.future_month_offset = optional_integer(block, "FutureMonthOffset");
    row.delivery_roll_days = optional_integer(block, "DeliveryRollDays");
    row.delivery_roll_calendar = optional_text(block, "DeliveryRollCalendar");
    row.future_expiry_date = optional_text(block, "FutureExpiryDate");
    row.future_contract_month = optional_text(block, "FutureContractMonth");
    row.interpolation = optional_text(block, "Interpolation");
    row.bid_ask_adjustment = optional_decimal(block, "BidAskAdjustment");
    option_block::stamp_row(row);
    result.instrument_underlyings.push_back(std::move(row));
}

/// Reads the element the holder states, once or as a list, under the role.
template <typename Data>
void read_underlyings(Data& result,
                      const record_reader& holder,
                      std::string_view element,
                      std::string_view role) {
    if (!holder.declares(element))
        return;
    if (states_a_list(holder, element)) {
        int sequence_number = 0;
        for (const auto& block : holder.children(element))
            read_underlying(result, block, role, ++sequence_number);
    } else if (const auto block = holder.child(element)) {
        read_underlying(result, *block, role, 1);
    }
}

template <typename Data>
std::vector<const instrument_underlying*> underlyings_of(const Data& v, std::string_view role) {
    std::vector<const instrument_underlying*> rows;
    for (const auto& row : v.instrument_underlyings)
        if (row.underlying_role == role)
            rows.push_back(&row);
    std::sort(rows.begin(), rows.end(), [](const auto* a, const auto* b) {
        return a->sequence_number < b->sequence_number;
    });
    return rows;
}

inline void write_underlying(record_writer block, const instrument_underlying& row) {
    block.set_text("Type", row.underlying_type);
    block.set_text("Name", underlying_name_of(row.name));
    const auto text = [&](std::string_view name, const std::optional<std::string>& value) {
        if (value && block.declares(name))
            block.set_text(name, *value);
    };
    const auto integer = [&](std::string_view name, const std::optional<int>& value) {
        if (value && block.declares(name))
            block.set_integer(name, *value);
    };
    const auto decimal = [&](std::string_view name,
                             const std::optional<ores::utility::decimal::decimal>& value) {
        if (value && block.declares(name))
            block.set_decimal(name, *value);
    };
    const auto real = [&](std::string_view name, const std::optional<double>& value) {
        if (value && block.declares(name))
            set_decimal_from_double(block, name, *value);
    };
    text("IdentifierType", row.identifier_type);
    text("Currency", row.currency);
    text("Exchange", row.exchange);
    real("Weight", row.weight);
    text("PriceType", row.price_type);
    integer("FutureMonthOffset", row.future_month_offset);
    integer("DeliveryRollDays", row.delivery_roll_days);
    text("DeliveryRollCalendar", row.delivery_roll_calendar);
    text("FutureExpiryDate", row.future_expiry_date);
    text("FutureContractMonth", row.future_contract_month);
    text("Interpolation", row.interpolation);
    decimal("BidAskAdjustment", row.bid_ask_adjustment);
}

/**
 * @brief Writes the underlyings kept under the role into the holder's
 * element, once or as a list as the holder states it.
 *
 * @return false when the rows keep no underlying under the role, so the
 * caller can write the underlying a row stored before the underlyings were
 * kept.
 */
template <typename Data>
bool write_underlyings(record_writer& holder,
                       std::string_view element,
                       const Data& v,
                       std::string_view role) {
    const auto rows = underlyings_of(v, role);
    if (rows.empty() || !holder.declares(element))
        return false;
    if (states_a_list(holder, element)) {
        for (const auto* row : rows)
            write_underlying(holder.append(element), *row);
    } else {
        write_underlying(holder.child(element), *rows.front());
    }
    return true;
}

/**
 * @brief Where a product states its underlyings: the trade type, the element
 * under the product that holds them (empty for the product itself), the
 * element each underlying is, and the role its rows are kept under.
 */
struct underlying_holder {
    std::string_view trade_type;
    std::string_view holder;
    std::string_view element;
    std::string_view role;
};

/// Reads every underlying the table names for the trade type.
template <typename Data, std::size_t N>
void read_product_underlyings(Data& result,
                              const record_reader& trade,
                              std::string_view trade_type,
                              const underlying_holder (&table)[N]) {
    for (const auto& entry : table) {
        if (entry.trade_type != trade_type)
            continue;
        const auto product = trade.child(std::string(trade_type) + "Data");
        if (!product)
            return;
        if (entry.holder.empty()) {
            read_underlyings(result, *product, entry.element, entry.role);
        } else if (product->declares(entry.holder)) {
            if (const auto holder = product->child(entry.holder))
                read_underlyings(result, *holder, entry.element, entry.role);
        }
    }
}

}

#endif
