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
#ifndef ORES_ORE_CORE_DOMAIN_BARRIER_BLOCK_PROJECTION_HPP
#define ORES_ORE_CORE_DOMAIN_BARRIER_BLOCK_PROJECTION_HPP

#include "ores.ore.core/domain/option_block_projection.hpp"
#include "ores.ore.core/xml/binding.hpp"
#include "ores.trading.api/domain/instrument_barrier.hpp"
#include "ores.trading.api/domain/instrument_barrier_level.hpp"
#include <algorithm>
#include <cstddef>
#include <string>
#include <string_view>
#include <vector>

/**
 * @file barrier_block_projection.hpp
 * @brief ORE's BarrierData block, read into and written from the shared
 * barrier rows, for any family whose composite holds them.
 *
 * Fifteen products state a BarrierData: one barrier, a list of them, or, for a
 * generic barrier option, a transatlantic barrier beside the list. Each block
 * is an instrument_barrier row and each of its levels an
 * instrument_barrier_level row, so a single, a double and a multi barrier are
 * the same shape. A holder states BarrierData either once or as a list, and
 * the block asks the holder's record type which.
 */

namespace ores::ore::domain::barrier_block {

using trading::domain::instrument_barrier;
using trading::domain::instrument_barrier_level;
using xml::record_reader;
using xml::record_writer;

/// Whether the holder states BarrierData as a list rather than once.
template <typename Record>
bool states_a_list(const Record& holder) {
    const auto* f = xml::find_field(holder.type(), "BarrierData", false);
    return f != nullptr && f->kind == xml::field_kind::repeated_nested;
}

template <typename Data>
void read_barrier(Data& result,
                  const record_reader& block,
                  std::string_view role,
                  int sequence_number) {
    using option_block::spelling_of;
    instrument_barrier row;
    row.barrier_role = std::string(role);
    row.sequence_number = sequence_number;
    row.barrier_type = spelling_of(block, "Type");
    if (block.has("Style"))
        row.style = spelling_of(block, "Style");
    if (block.has("StrictComparison"))
        row.strict_comparison = spelling_of(block, "StrictComparison");
    row.rebate = block.decimal("Rebate");
    if (block.has("RebateCurrency"))
        row.rebate_currency = spelling_of(block, "RebateCurrency");
    if (block.has("RebatePayTime"))
        row.rebate_pay_time = spelling_of(block, "RebatePayTime");
    if (block.has("OverrideTriggered"))
        row.override_triggered = spelling_of(block, "OverrideTriggered");
    option_block::stamp_row(row);
    result.barriers.push_back(std::move(row));

    if (const auto levels = block.child("Levels")) {
        int level_number = 0;
        for (const auto& value : levels->decimals("Level")) {
            instrument_barrier_level level;
            level.barrier_role = std::string(role);
            level.barrier_sequence_number = sequence_number;
            level.sequence_number = ++level_number;
            level.level = value;
            option_block::stamp_row(level);
            result.barrier_levels.push_back(std::move(level));
        }
    }
}

/// Reads the BarrierData the holder states, once or as a list, under the role.
template <typename Data>
void read_barriers(Data& result, const record_reader& holder, std::string_view role) {
    if (states_a_list(holder)) {
        int sequence_number = 0;
        for (const auto& block : holder.children("BarrierData"))
            read_barrier(result, block, role, ++sequence_number);
    } else if (const auto block = holder.child("BarrierData")) {
        read_barrier(result, *block, role, 1);
    }
}

template <typename Data>
std::vector<const instrument_barrier*> barriers_of(const Data& v, std::string_view role) {
    std::vector<const instrument_barrier*> rows;
    for (const auto& row : v.barriers)
        if (row.barrier_role == role)
            rows.push_back(&row);
    std::sort(rows.begin(), rows.end(), [](const auto* a, const auto* b) {
        return a->sequence_number < b->sequence_number;
    });
    return rows;
}

template <typename Data>
bool has_barriers(const Data& v, std::string_view role) {
    return !barriers_of(v, role).empty();
}

template <typename Data>
void write_barrier(record_writer block, const instrument_barrier& row, const Data& v) {
    block.set_spelling("Type", row.barrier_type, "barrier type");
    if (row.style)
        block.set_spelling("Style", *row.style, "barrier style");
    if (row.strict_comparison)
        block.set_spelling("StrictComparison", *row.strict_comparison, "strict comparison");
    std::vector<const instrument_barrier_level*> levels;
    for (const auto& level : v.barrier_levels)
        if (level.barrier_role == row.barrier_role &&
            level.barrier_sequence_number == row.sequence_number)
            levels.push_back(&level);
    std::sort(levels.begin(), levels.end(), [](const auto* a, const auto* b) {
        return a->sequence_number < b->sequence_number;
    });
    if (!levels.empty()) {
        auto list = block.child("Levels");
        for (const auto* level : levels)
            list.append_decimal("Level", level->level);
    }
    if (row.rebate)
        block.set_decimal("Rebate", *row.rebate);
    if (row.rebate_currency)
        block.set_spelling("RebateCurrency", *row.rebate_currency, "currency");
    if (row.rebate_pay_time)
        block.set_spelling("RebatePayTime", *row.rebate_pay_time, "rebate pay time");
    if (row.override_triggered)
        block.set_spelling("OverrideTriggered", *row.override_triggered, "boolean");
}

/**
 * @brief Writes the barriers kept under the role into the holder, once or as
 * a list as the holder states them.
 *
 * @return false when the rows keep no barrier under the role, so the caller
 * can fall back to the barrier a row stored before the barriers were kept.
 */
template <typename Data>
bool write_barriers(record_writer& holder, const Data& v, std::string_view role) {
    const auto rows = barriers_of(v, role);
    if (rows.empty())
        return false;
    if (states_a_list(holder)) {
        for (const auto* row : rows)
            write_barrier(holder.append("BarrierData"), *row, v);
    } else {
        write_barrier(holder.child("BarrierData"), *rows.front(), v);
    }
    return true;
}

/**
 * @brief Where a product states its barriers: the trade type, the element
 * under the product that holds the BarrierData (empty for the product
 * itself) and the role its rows are kept under.
 */
struct barrier_holder {
    std::string_view trade_type;
    std::string_view element;
    std::string_view role;
};

/// Reads every barrier the table names for the trade type.
template <typename Data, std::size_t N>
void read_product_barriers(Data& result,
                           const record_reader& trade,
                           std::string_view trade_type,
                           const barrier_holder (&table)[N]) {
    for (const auto& entry : table) {
        if (entry.trade_type != trade_type)
            continue;
        const auto product = trade.child(std::string(trade_type) + "Data");
        if (!product)
            return;
        if (entry.element.empty()) {
            read_barriers(result, *product, entry.role);
        } else if (const auto holder = product->child(entry.element)) {
            read_barriers(result, *holder, entry.role);
        }
    }
}

/**
 * @brief Writes the barriers kept under the role into the product, or into
 * the named element under it.
 *
 * @return false when the rows keep none, so the caller writes the barrier a
 * row stored before the barriers were kept.
 */
template <typename Data>
bool write_kept_barriers(record_writer& product,
                         std::string_view element,
                         const Data& v,
                         std::string_view role) {
    if (!has_barriers(v, role))
        return false;
    if (element.empty())
        return write_barriers(product, v, role);
    auto holder = product.child(element);
    return write_barriers(holder, v, role);
}

/**
 * @brief A product row that keeps when and against what its barrier is
 * monitored: the start date, the calendar and the fixing index.
 */
template <typename Row>
concept monitored = requires(Row& row) {
    row.barrier_start_date;
    row.barrier_calendar;
    row.barrier_fixing_index;
};

/// A text or enumerated scalar the product declares, as written.
inline auto declared_scalar(const record_reader& product, std::string_view name)
    -> std::optional<std::string> {
    const auto* f = xml::find_field(product.type(), name, false);
    if (f == nullptr || !product.has(name))
        return std::nullopt;
    if (f->kind == xml::field_kind::enumeration)
        return product.spelling(name);
    if (f->kind == xml::field_kind::text)
        return product.text(name);
    return std::nullopt;
}

inline void set_declared_scalar(record_writer& product,
                                std::string_view name,
                                const std::optional<std::string>& value) {
    const auto* f = xml::find_field(product.type(), name, false);
    if (f == nullptr || !value)
        return;
    if (f->kind == xml::field_kind::enumeration)
        product.set_spelling(name, *value, name);
    else if (f->kind == xml::field_kind::text)
        product.set_text(name, *value);
}

/// Reads the monitoring terms the product declares into a monitored row.
template <monitored Row>
void read_monitoring(Row& row, const record_reader& product, std::string_view index_element) {
    if (const auto start = declared_scalar(product, "StartDate"))
        row.barrier_start_date = option_block::to_optional_domain_date(*start);
    row.barrier_calendar = declared_scalar(product, "Calendar");
    row.barrier_fixing_index = declared_scalar(product, index_element);
}

/// Writes a monitored row's terms where the product declares them.
template <monitored Row>
void write_monitoring(record_writer& product, const Row& row, std::string_view index_element) {
    if (row.barrier_start_date)
        set_declared_scalar(
            product, "StartDate", option_block::to_ore_date(*row.barrier_start_date));
    set_declared_scalar(product, "Calendar", row.barrier_calendar);
    set_declared_scalar(product, index_element, row.barrier_fixing_index);
}

}

#endif
