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
#ifndef ORES_MARKETDATA_API_DATUM_MARKET_DATUM_HPP
#define ORES_MARKETDATA_API_DATUM_MARKET_DATUM_HPP

#include "ores.marketdata.api/datum/ore_types.hpp"
#include "ores.marketdata.api/datum/schema.hpp"
#include "ores.marketdata.api/datum/value.hpp"
#include "ores.marketdata.api/export.hpp"
#include <expected>
#include <span>
#include <string>
#include <variant>
#include <vector>

namespace ores::marketdata::datum {

/// One field of a datum and the value it holds.
struct field_value {
    field name;
    value held;
    friend bool operator==(const field_value&, const field_value&) = default;
};

/**
 * @brief One ORE market datum, or the series it belongs to.
 *
 * The instrument type, the quote type and the fields the type's schema row
 * lists, in the row's order. A datum holds every field of its row, none where
 * the key leaves one out; a series holds only the row's identity fields, so
 * every datum of one series has the same series.
 *
 * The only ways to build one check it against its row, so a value of this type
 * always matches its schema.
 */
class ORES_MARKETDATA_API_EXPORT market_datum final {
public:
    /**
     * @brief A datum, checked against the row for @p type.
     *
     * Refused: a field the row does not list, a field given twice, a field the
     * row lists and the input leaves out, none in a field the row says is never
     * none, a value of the wrong type, and a code outside its field's
     * vocabulary. The fields may come in any order; the datum holds them in the
     * row's.
     */
    [[nodiscard]] static std::expected<market_datum, std::string>
    make(instrument_type type, quote_type quote, std::vector<field_value> fields);

    /// A series: the row's identity fields only, checked the same way.
    [[nodiscard]] static std::expected<market_datum, std::string>
    make_series(instrument_type type, quote_type quote, std::vector<field_value> fields);

    [[nodiscard]] instrument_type type() const noexcept {
        return type_;
    }
    [[nodiscard]] quote_type quote() const noexcept {
        return quote_;
    }
    [[nodiscard]] bool is_series() const noexcept {
        return series_;
    }
    [[nodiscard]] std::span<const field_value> fields() const noexcept {
        return fields_;
    }

    /// Whether the datum holds @p f: its row lists it, and for a series it is
    /// an identity field.
    [[nodiscard]] bool holds(field f) const noexcept;

    /// The value of @p f; throws std::out_of_range when the datum does not
    /// hold it.
    [[nodiscard]] const value& at(field f) const;

    /**
     * @brief The typed value of @p F, or null when the field holds none.
     *
     * Throws std::out_of_range, like at(), when the datum does not hold @p F:
     * a field its row does not list, or a coordinate of a series. Ask holds()
     * first where either can happen.
     */
    template <field F>
    [[nodiscard]] const field_value_t<F>* get() const {
        return std::get_if<field_value_t<F>>(&at(F));
    }

    /**
     * @brief Equal when the types, the quote types and every field's text are
     * equal.
     *
     * Values compare by the text the key carried, not by what it denotes: a
     * strike of 1.0 and one of 1.00 are different datums and different
     * series, because they are different keys.
     */
    friend bool operator==(const market_datum&, const market_datum&) = default;

private:
    market_datum(instrument_type type,
                 quote_type quote,
                 bool series,
                 std::vector<field_value> fields);

    static std::expected<market_datum, std::string>
    checked(instrument_type type, quote_type quote, bool series, std::vector<field_value> fields);

    instrument_type type_;
    quote_type quote_;
    bool series_;
    std::vector<field_value> fields_;
};

/**
 * @brief The series @p d belongs to: @p d without its coordinate fields.
 *
 * Two datums share a series when their identity fields have the same text; see
 * operator==.
 */
ORES_MARKETDATA_API_EXPORT market_datum series_of(const market_datum& d);

/**
 * @brief The datum of @p series at @p coordinates: the series' identity fields
 * and the coordinate fields given, with any coordinate not given holding none.
 *
 * The inverse of series_of, for a writer that holds a series and a point rather
 * than a key, such as a curve bootstrap writing its pillars.
 */
ORES_MARKETDATA_API_EXPORT std::expected<market_datum, std::string>
datum_at(const market_datum& series, std::vector<field_value> coordinates);

/**
 * @brief The datum of @p series at @p point: the text of its one coordinate,
 * such as a curve pillar's tenor. A series with more or fewer than one
 * coordinate has no single point, and the error says so.
 */
ORES_MARKETDATA_API_EXPORT std::expected<market_datum, std::string>
datum_at_point(const market_datum& series, std::string_view point);

/**
 * @brief The value field @p f holds when written as @p text, read by the field's
 * value kind; the error names what the text is not.
 */
ORES_MARKETDATA_API_EXPORT std::expected<value, std::string> parse_value(field f,
                                                                         std::string_view text);

}

#endif
