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
#ifndef ORES_ORE_CORE_DOMAIN_SCALAR_TERMS_PROJECTION_HPP
#define ORES_ORE_CORE_DOMAIN_SCALAR_TERMS_PROJECTION_HPP

#include "ores.ore.core/domain/option_block_projection.hpp"
#include "ores.ore.core/xml/binding.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <chrono>
#include <cstddef>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <variant>

/**
 * @file scalar_terms_projection.hpp
 * @brief A product's single-valued terms, read into and written from a row's
 * optional members by a table of element and member.
 *
 * A product states many scalars beside its blocks: a calendar, a lag, a flag,
 * a cap. Each is one element and one optional member of the product's row, so
 * a product lists its terms once and the read and the write both walk that
 * list. The record type decides how the element is spelled, as text, an
 * enumeration, an integer, a decimal or a repeated leaf, and the member's
 * type decides what the row keeps, so a term is the same line whichever way
 * the schema states it.
 */

namespace ores::ore::domain::scalar_terms {

using ores::utility::decimal::decimal;
using xml::record_reader;
using xml::record_writer;

/// One term: the element the product states and the member that keeps it.
template <typename Row>
struct term {
    std::string_view element;
    std::variant<std::optional<std::string> Row::*,
                 std::optional<int> Row::*,
                 std::optional<decimal> Row::*,
                 std::optional<double> Row::*,
                 std::optional<bool> Row::*,
                 std::optional<std::chrono::year_month_day> Row::*>
        member;
};

/// The element's value as written, whichever kind the record declares.
inline auto read_text(const record_reader& product,
                      std::string_view element) -> std::optional<std::string> {
    const auto* f = xml::find_field(product.type(), element, false);
    if (f == nullptr)
        return std::nullopt;
    switch (f->kind) {
        case xml::field_kind::repeated_leaf: {
            const auto values = f->scalar == xml::scalar_kind::enumeration ?
                                    product.spellings(element) :
                                    product.values(element);
            if (values.empty())
                return std::nullopt;
            return values.front();
        }
        case xml::field_kind::enumeration:
            return product.spelling(element);
        case xml::field_kind::text:
            return product.text(element);
        case xml::field_kind::integer:
            if (const auto value = product.integer(element))
                return std::to_string(*value);
            return std::nullopt;
        case xml::field_kind::decimal:
            if (const auto value = product.decimal(element))
                return value->to_string();
            return std::nullopt;
        default:
            return std::nullopt;
    }
}

/// Writes the value in the kind the record declares for the element.
inline void write_text(record_writer& product, std::string_view element, const std::string& value) {
    const auto* f = xml::find_field(product.type(), element, false);
    if (f == nullptr)
        return;
    switch (f->kind) {
        case xml::field_kind::repeated_leaf:
            if (f->scalar == xml::scalar_kind::enumeration)
                product.append_spelling(element, value, element);
            else
                product.append_value(element, value);
            return;
        case xml::field_kind::enumeration:
            product.set_spelling(element, value, element);
            return;
        case xml::field_kind::text:
            product.set_text(element, value);
            return;
        case xml::field_kind::integer:
            product.set_integer(element, std::stoll(value));
            return;
        case xml::field_kind::decimal:
            if (const auto parsed = decimal::from_string(value))
                product.set_decimal(element, *parsed);
            return;
        default:
            return;
    }
}

template <typename Row, std::size_t N>
void read_terms(Row& row, const record_reader& product, const term<Row> (&terms)[N]) {
    for (const auto& t : terms) {
        if (!product.declares(t.element) || !product.has(t.element))
            continue;
        const auto text = read_text(product, t.element);
        if (!text)
            continue;
        std::visit(
            [&](auto member) {
                using Value = typename std::remove_reference_t<decltype(row.*member)>::value_type;
                if constexpr (std::is_same_v<Value, std::string>) {
                    row.*member = *text;
                } else if constexpr (std::is_same_v<Value, int>) {
                    row.*member = std::stoi(*text);
                } else if constexpr (std::is_same_v<Value, decimal>) {
                    if (const auto parsed = decimal::from_string(*text))
                        row.*member = *parsed;
                } else if constexpr (std::is_same_v<Value, double>) {
                    row.*member = std::stod(*text);
                } else if constexpr (std::is_same_v<Value, bool>) {
                    row.*member = option_block::is_flag_on(*text);
                } else {
                    row.*member = option_block::to_optional_domain_date(*text);
                }
            },
            t.member);
    }
}

template <typename Row, std::size_t N>
void write_terms(record_writer& product, const Row& row, const term<Row> (&terms)[N]) {
    for (const auto& t : terms) {
        if (!product.declares(t.element))
            continue;
        std::visit(
            [&](auto member) {
                const auto& value = row.*member;
                if (!value)
                    return;
                using Value = typename std::remove_cvref_t<decltype(value)>::value_type;
                if constexpr (std::is_same_v<Value, std::string>) {
                    write_text(product, t.element, *value);
                } else if constexpr (std::is_same_v<Value, int>) {
                    write_text(product, t.element, std::to_string(*value));
                } else if constexpr (std::is_same_v<Value, decimal>) {
                    write_text(product, t.element, value->to_string());
                } else if constexpr (std::is_same_v<Value, double>) {
                    write_text(product, t.element, std::format("{}", *value));
                } else if constexpr (std::is_same_v<Value, bool>) {
                    write_text(product, t.element, std::string(option_block::flag_text(*value)));
                } else {
                    write_text(product, t.element, option_block::to_ore_date(*value));
                }
            },
            t.member);
    }
}

}

#endif
