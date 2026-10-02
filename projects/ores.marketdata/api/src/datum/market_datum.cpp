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
#include "ores.marketdata.api/datum/market_datum.hpp"
#include <algorithm>
#include <format>
#include <optional>
#include <stdexcept>

namespace ores::marketdata::datum {

namespace {

bool holds_kind(value_kind kind, const value& v) {
    switch (kind) {
        case value_kind::text:
            return std::holds_alternative<std::string>(v);
        case value_kind::term:
            return std::holds_alternative<term>(v);
        case value_kind::decimal:
            return std::holds_alternative<decimal>(v);
        case value_kind::strike:
            return std::holds_alternative<strike>(v);
        case value_kind::code:
            return std::holds_alternative<code>(v);
    }
    return false;
}

// ASCII only, so the result does not depend on the program's locale.
std::string upper(std::string_view s) {
    std::string out(s);
    std::ranges::transform(
        out, out.begin(), [](char c) { return c >= 'a' && c <= 'z' ? char(c - 'a' + 'A') : c; });
    return out;
}

// ORE reads a time unit in any case; every other code it compares exactly.
bool in_vocabulary(field f, const code& c) {
    const auto vocabulary = codes_of(f);
    const auto token = f == field::time_unit ? upper(c.text()) : c.text();
    return std::ranges::find(vocabulary, token) != vocabulary.end();
}

std::unexpected<std::string> refuse(instrument_type type, std::string_view what) {
    return std::unexpected(std::format("{}: {}", ore_name(type), what));
}

}

market_datum::market_datum(instrument_type type,
                           quote_type quote,
                           bool series,
                           std::vector<field_value> fields)
    : type_(type)
    , quote_(quote)
    , series_(series)
    , fields_(std::move(fields)) {}

std::expected<market_datum, std::string> market_datum::checked(instrument_type type,
                                                               quote_type quote,
                                                               bool series,
                                                               std::vector<field_value> fields) {
    const auto& row = schema_of(type);
    std::vector<bool> used(fields.size(), false);
    std::vector<std::size_t> order;
    order.reserve(row.fields.size());

    for (const auto& spec : row.fields) {
        if (series && spec.role != field_role::identity)
            continue;
        std::optional<std::size_t> found;
        for (std::size_t i = 0; i < fields.size(); ++i) {
            if (fields[i].name != spec.name)
                continue;
            if (found)
                return refuse(type, std::format("{} is given twice", name_of(spec.name)));
            found = i;
        }
        if (!found)
            return refuse(type, std::format("{} is missing", name_of(spec.name)));

        const auto& held = fields[*found].held;
        if (std::holds_alternative<none_t>(held)) {
            if (!spec.may_be_none)
                return refuse(type, std::format("{} cannot be none", name_of(spec.name)));
        } else if (!holds_kind(kind_of(spec.name), held)) {
            return refuse(type,
                          std::format("{} holds a value of the wrong type", name_of(spec.name)));
        } else if (const auto* c = std::get_if<code>(&held); c && !in_vocabulary(spec.name, *c)) {
            return refuse(
                type, std::format("'{}' is not a value {} takes", c->text(), name_of(spec.name)));
        }
        used[*found] = true;
        order.push_back(*found);
    }

    for (std::size_t i = 0; i < fields.size(); ++i) {
        if (!used[i])
            return refuse(type,
                          std::format("{} is not a field of {}",
                                      name_of(fields[i].name),
                                      series ? "its series" : "this type"));
    }

    std::vector<field_value> ordered;
    ordered.reserve(order.size());
    for (const auto i : order)
        ordered.push_back(std::move(fields[i]));
    return market_datum(type, quote, series, std::move(ordered));
}

std::expected<market_datum, std::string>
market_datum::make(instrument_type type, quote_type quote, std::vector<field_value> fields) {
    return checked(type, quote, false, std::move(fields));
}

std::expected<market_datum, std::string>
market_datum::make_series(instrument_type type, quote_type quote, std::vector<field_value> fields) {
    return checked(type, quote, true, std::move(fields));
}

bool market_datum::holds(field f) const noexcept {
    return std::ranges::any_of(fields_, [f](const auto& fv) { return fv.name == f; });
}

const value& market_datum::at(field f) const {
    for (const auto& fv : fields_) {
        if (fv.name == f)
            return fv.held;
    }
    throw std::out_of_range(std::format("{} does not hold {}", ore_name(type_), name_of(f)));
}

market_datum series_of(const market_datum& d) {
    std::vector<field_value> identity;
    const auto& row = schema_of(d.type());
    for (const auto& spec : row.fields) {
        if (spec.role == field_role::identity)
            identity.push_back({spec.name, d.at(spec.name)});
    }
    // The fields came from a checked datum of the same row, so this cannot fail.
    return market_datum::make_series(d.type(), d.quote(), std::move(identity)).value();
}

}
