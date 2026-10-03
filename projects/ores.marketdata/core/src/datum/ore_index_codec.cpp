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
#include "ores.marketdata.core/datum/ore_index_codec.hpp"
#include "ores.marketdata.api/datum/value.hpp"
#include <algorithm>
#include <array>
#include <cctype>
#include <chrono>
#include <format>
#include <utility>
#include <vector>

/**
 * @file ore_index_codec.cpp
 * @brief ORE's parseIndex, family by family. Line numbers cite
 * OREData/ored/utilities/indexparser.cpp at the ORE commit the catalogue names.
 */

namespace ores::marketdata::datum {

namespace {

using f = index_family;
using fields = std::vector<market_index::field_text>;

std::vector<std::string_view> split(std::string_view s) {
    std::vector<std::string_view> parts;
    std::size_t start = 0;
    while (true) {
        const auto dash = s.find('-', start);
        parts.push_back(s.substr(start, dash - start));
        if (dash == std::string_view::npos)
            break;
        start = dash + 1;
    }
    return parts;
}

bool is_currency(std::string_view t) {
    return t.size() == 3 &&
           std::ranges::all_of(t, [](unsigned char c) { return c >= 'A' && c <= 'Z'; });
}

bool is_period(std::string_view t) {
    const auto parsed = term::parse(t);
    return parsed && parsed->which() == term::kind::period;
}

bool all_digits(std::string_view t) {
    return !t.empty() && std::ranges::all_of(t, [](unsigned char c) { return std::isdigit(c); });
}

/// Whether @p t is a real date written YYYY-MM-DD.
bool is_iso_date(std::string_view t) {
    if (t.size() != 10 || t[4] != '-' || t[7] != '-')
        return false;
    if (!all_digits(t.substr(0, 4)) || !all_digits(t.substr(5, 2)) || !all_digits(t.substr(8, 2)))
        return false;
    const std::chrono::year_month_day d{
        std::chrono::year{std::stoi(std::string(t.substr(0, 4)))},
        std::chrono::month{static_cast<unsigned>(std::stoi(std::string(t.substr(5, 2))))},
        std::chrono::day{static_cast<unsigned>(std::stoi(std::string(t.substr(8, 2))))}};
    return d.ok();
}

/// Whether @p t is a month written YYYY-MM.
bool is_iso_month(std::string_view t) {
    if (t.size() != 7 || t[4] != '-' || !all_digits(t.substr(0, 4)) || !all_digits(t.substr(5, 2)))
        return false;
    const auto m = std::stoi(std::string(t.substr(5, 2)));
    return m >= 1 && m <= 12;
}

/// A name with an optional trailing -YYYY-MM-DD or -YYYY-MM split off, as ORE
/// reads a commodity or a bond (:676, :763).
std::pair<std::string_view, std::string_view> split_expiry(std::string_view rest) {
    if (rest.size() > 11 && rest[rest.size() - 11] == '-' &&
        is_iso_date(rest.substr(rest.size() - 10)))
        return {rest.substr(0, rest.size() - 11), rest.substr(rest.size() - 10)};
    if (rest.size() > 8 && rest[rest.size() - 8] == '-' &&
        is_iso_month(rest.substr(rest.size() - 7)))
        return {rest.substr(0, rest.size() - 8), rest.substr(rest.size() - 7)};
    return {rest, {}};
}

std::expected<market_index, std::string>
make(index_family family, std::string_view subject, fields fs = {}) {
    return market_index::make(family, std::string(subject), std::move(fs));
}

fields with_optional(fields fs, std::string_view name, std::string_view text) {
    if (!text.empty())
        fs.push_back({std::string(name), std::string(text)});
    return fs;
}

std::expected<market_index, std::string> refuse(std::string_view why) {
    return std::unexpected(std::string(why));
}

// POWER-NAME[-YYYY-MM-DD[-START-END[-DST]]], the name without a hyphen (:883).
std::expected<market_index, std::string> read_power(std::string_view rest) {
    const auto t = split(rest);
    if (t.size() == 1)
        return refuse("a power index with no delivery date takes ORE's evaluation date, which the "
                      "name cannot keep");
    if (t.size() != 4 && t.size() != 6)
        return refuse("a power index is POWER-NAME-YYYY-MM-DD[-START-END]");
    const auto delivery = rest.substr(t[0].size() + 1, 10);
    if (!is_iso_date(delivery))
        return refuse("a power index's delivery is a date");
    fields fs{{"delivery", std::string(delivery)}};
    if (t.size() == 6) {
        if (!all_digits(t[4]) || !all_digits(t[5]))
            return refuse("a power index's delivery start and end are whole seconds");
        fs.push_back({"start", std::string(t[4])});
        fs.push_back({"end", std::string(t[5])});
    }
    return make(f::power, t[0], std::move(fs));
}

// CCY-NAME[-TENOR] (:258) or CCY-CMS[-TAG]-TENOR (:519).
std::expected<market_index, std::string> read_rate(std::string_view name) {
    const auto t = split(name);
    if (!is_currency(t[0]))
        return refuse("a rate index starts with a three-letter currency");
    if (t.size() >= 2 && t[1] == "CMS") {
        if (t.size() != 3 && t.size() != 4)
            return refuse("a CMS index is CCY-CMS[-TAG]-TENOR");
        if (!is_period(t.back()))
            return refuse("a CMS index ends in a tenor");
        fields fs;
        if (t.size() == 4)
            fs.push_back({"tag", std::string(t[2])});
        fs.push_back({"tenor", std::string(t.back())});
        return make(f::swap, t[0], std::move(fs));
    }
    if (t.size() != 2 && t.size() != 3)
        return refuse("an IBOR index is CCY-NAME[-TENOR]");
    if (t.size() == 3 && !is_period(t[2]))
        return refuse("an IBOR index's tenor is a period");
    return make(f::ibor,
                t[0],
                with_optional({{"name", std::string(t[1])}}, "tenor", t.size() == 3 ? t[2] : ""));
}

// ORE's own inflation indices and their spaced aliases (:622); a convention may
// define any other name.
std::string_view unspaced(std::string_view name) {
    static constexpr std::array<std::pair<std::string_view, std::string_view>, 10> aliases{{
        {"AU CPI", "AUCPI"},
        {"BE HICP", "BEHICP"},
        {"EU HICP", "EUHICP"},
        {"EU HICPXT", "EUHICPXT"},
        {"FR HICP", "FRHICP"},
        {"FR CPI", "FRCPI"},
        {"UK RPI", "UKRPI"},
        {"US CPI", "USCPI"},
        {"ZA CPI", "ZACPI"},
        {"DE CPI", "DECPI"},
    }};
    for (const auto& [spaced, plain] : aliases) {
        if (spaced == name)
            return plain;
    }
    return name;
}

bool starts(std::string_view s, std::string_view prefix) {
    return s.starts_with(prefix);
}

}

std::expected<market_index, std::string> ore_index_codec::read(std::string_view name) {
    auto result = [&]() -> std::expected<market_index, std::string> {
        if (starts(name, "EQ-"))
            return make(f::equity, name.substr(3));
        if (starts(name, "BOND-")) {
            const auto [security, expiry] = split_expiry(name.substr(5));
            return make(f::bond, security, with_optional({}, "expiry", expiry));
        }
        if (starts(name, "BOND_FUTURE-"))
            return make(f::bond_future, name.substr(12));
        if (starts(name, "COMM-")) {
            const auto [commodity, expiry] = split_expiry(name.substr(5));
            return make(f::commodity, commodity, with_optional({}, "expiry", expiry));
        }
        if (starts(name, "POWER-"))
            return read_power(name.substr(6));
        if (starts(name, "FX-")) {
            const auto t = split(name);
            if (t.size() != 4 || !is_currency(t[2]) || !is_currency(t[3]) || t[1].empty())
                return refuse("an FX index is FX-SOURCE-CCY1-CCY2");
            return make(f::fx, t[2], {{"source", std::string(t[1])}, {"ccy", std::string(t[3])}});
        }
        if (starts(name, "GENERIC-"))
            return make(f::generic, name.substr(8));
        if (starts(name, "CMB-")) {
            const auto t = split(name);
            if (t.size() < 3 || !is_period(t.back()))
                return refuse("a constant maturity bond index is CMB-FAMILY-TENOR");
            const auto family = name.substr(4, name.size() - 4 - t.back().size() - 1);
            return make(f::cmb, family, {{"tenor", std::string(t.back())}});
        }
        if (name.contains('-'))
            return read_rate(name);
        return make(f::inflation, unspaced(name));
    }();
    if (!result)
        return std::unexpected(std::format("'{}': {}", name, result.error()));
    return result;
}

std::string ore_index_codec::write(const market_index& index) {
    const auto opt = [&](std::string_view field) {
        const auto* text = index.get(field);
        return text ? "-" + *text : std::string();
    };
    const auto& subject = index.subject();
    switch (index.family()) {
        case f::ibor:
            return subject + "-" + *index.get("name") + opt("tenor");
        case f::swap:
            return subject + "-CMS" + opt("tag") + "-" + *index.get("tenor");
        case f::inflation:
            return subject;
        case f::fx:
            return "FX-" + *index.get("source") + "-" + subject + "-" + *index.get("ccy");
        case f::equity:
            return "EQ-" + subject;
        case f::commodity:
            return "COMM-" + subject + opt("expiry");
        case f::power:
            return "POWER-" + subject + opt("delivery") + opt("start") + opt("end");
        case f::bond:
            return "BOND-" + subject + opt("expiry");
        case f::bond_future:
            return "BOND_FUTURE-" + subject;
        case f::cmb:
            return "CMB-" + subject + "-" + *index.get("tenor");
        case f::generic:
            return "GENERIC-" + subject;
    }
    return {};
}

}
