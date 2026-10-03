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
#ifndef ORES_MARKETDATA_API_DATUM_MARKET_INDEX_HPP
#define ORES_MARKETDATA_API_DATUM_MARKET_INDEX_HPP

#include "ores.marketdata.api/datum/schema.hpp"
#include "ores.marketdata.api/export.hpp"
#include <array>
#include <cstdint>
#include <expected>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

namespace ores::marketdata::datum {

/**
 * @brief The families of index ORE's parseIndex reads, which a fixing is quoted
 * against.
 */
enum class index_family : std::uint8_t {
    /// An IBOR or overnight rate: CCY-NAME[-TENOR], such as EUR-EURIBOR-6M.
    ibor,
    /// A CMS swap rate: CCY-CMS[-TAG]-TENOR.
    swap,
    /// A zero inflation index, such as UKRPI.
    inflation,
    /// An FX fixing: FX-SOURCE-CCY1-CCY2.
    fx,
    /// An equity: EQ-NAME.
    equity,
    /// A commodity spot or future: COMM-NAME[-YYYY-MM[-DD]].
    commodity,
    /// An intraday power index: POWER-NAME[-YYYY-MM-DD[-START-END]].
    power,
    /// A bond, or a bond future when dated: BOND-ID[-YYYY-MM[-DD]].
    bond,
    /// A bond future by contract: BOND_FUTURE-CONTRACT.
    bond_future,
    /// A constant maturity bond yield: CMB-FAMILY-TENOR.
    cmb,
    /// A generic index: GENERIC-NAME.
    generic
};

inline constexpr std::size_t index_family_count = 11;

/// The family's name as a fixing URI writes it: ibor, bond_future.
ORES_MARKETDATA_API_EXPORT std::string_view name_of(index_family f);

/// The family @p name names, or nothing.
ORES_MARKETDATA_API_EXPORT std::optional<index_family> index_family_named(std::string_view name);

struct index_field_spec {
    std::string_view name;
    bool optional;
};

/**
 * @brief One family's identity: its asset class, the field that names what the
 * index is about, and its other fields in the order an index name writes them.
 */
struct index_row {
    index_family family;
    asset_class asset;
    std::string_view subject;
    std::span<const index_field_spec> fields;
};

namespace detail {

inline constexpr std::array<index_field_spec, 2> ibor_fields{{{"name", false}, {"tenor", true}}};
inline constexpr std::array<index_field_spec, 2> swap_fields{{{"tag", true}, {"tenor", false}}};
inline constexpr std::array<index_field_spec, 2> fx_fields{{{"source", false}, {"ccy", false}}};
inline constexpr std::array<index_field_spec, 1> expiry_fields{{{"expiry", true}}};
inline constexpr std::array<index_field_spec, 3> power_fields{
    {{"delivery", true}, {"start", true}, {"end", true}}};
inline constexpr std::array<index_field_spec, 1> tenor_fields{{{"tenor", false}}};
inline constexpr std::array<index_field_spec, 0> no_fields{};

}

/// The index rows, in the enum's order.
inline constexpr std::array<index_row, index_family_count> index_rows{{
    {index_family::ibor, asset_class::ir, "ccy", detail::ibor_fields},
    {index_family::swap, asset_class::ir, "ccy", detail::swap_fields},
    {index_family::inflation, asset_class::inflation, "name", detail::no_fields},
    {index_family::fx, asset_class::fx, "unit", detail::fx_fields},
    {index_family::equity, asset_class::equity, "name", detail::no_fields},
    {index_family::commodity, asset_class::commodity, "name", detail::expiry_fields},
    {index_family::power, asset_class::commodity, "name", detail::power_fields},
    {index_family::bond, asset_class::security, "security", detail::expiry_fields},
    {index_family::bond_future, asset_class::security, "contract", detail::no_fields},
    {index_family::cmb, asset_class::security, "family", detail::tenor_fields},
    {index_family::generic, asset_class::generic, "name", detail::no_fields},
}};

constexpr const index_row& index_row_of(index_family f) {
    return index_rows[static_cast<std::size_t>(f)];
}

namespace detail {

constexpr bool index_rows_follow_the_enum() {
    for (std::size_t i = 0; i < index_rows.size(); ++i) {
        if (static_cast<std::size_t>(index_rows[i].family) != i)
            return false;
    }
    return true;
}

}

static_assert(detail::index_rows_follow_the_enum(), "index rows must follow index_family's order");

/**
 * @brief The identity of an index a fixing is quoted against: its family, its
 * subject and its other fields, as text.
 *
 * The only constructor checks the fields against the family's row: every field
 * known, none repeated, none empty, and every field that is not optional given.
 * An absent optional field is simply not held, and fields() lists the held ones
 * in the row's order. Equality is by every part.
 */
class ORES_MARKETDATA_API_EXPORT market_index final {
public:
    struct field_text {
        std::string name;
        std::string text;
        friend bool operator==(const field_text&, const field_text&) = default;
    };

    [[nodiscard]] static std::expected<market_index, std::string>
    make(index_family family, std::string subject, std::vector<field_text> fields);

    [[nodiscard]] index_family family() const noexcept {
        return family_;
    }
    [[nodiscard]] const std::string& subject() const noexcept {
        return subject_;
    }
    [[nodiscard]] std::span<const field_text> fields() const noexcept {
        return fields_;
    }

    /// The text field @p name holds, or nullptr when it holds none.
    [[nodiscard]] const std::string* get(std::string_view name) const;

    friend bool operator==(const market_index&, const market_index&) = default;

private:
    market_index(index_family family, std::string subject, std::vector<field_text> fields);

    index_family family_;
    std::string subject_;
    std::vector<field_text> fields_;
};

}

#endif
