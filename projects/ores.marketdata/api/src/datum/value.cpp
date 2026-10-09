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
#include "ores.marketdata.api/datum/value.hpp"
#include "ores.platform/numeric/floating_point.hpp"
#include <algorithm>
#include <array>
#include <chrono>
#include <format>
#include <vector>

namespace ores::marketdata::datum {

namespace {

bool is_digit(char c) {
    return c >= '0' && c <= '9';
}

bool all_digits(std::string_view s) {
    return !s.empty() && std::ranges::all_of(s, is_digit);
}

// ASCII only, so the result does not depend on the program's locale.
std::string upper(std::string_view s) {
    std::string out(s);
    std::ranges::transform(
        out, out.begin(), [](char c) { return c >= 'a' && c <= 'z' ? char(c - 'a' + 'A') : c; });
    return out;
}

int to_int(std::string_view s) {
    int result = 0;
    for (const char c : s)
        result = result * 10 + (c - '0');
    return result;
}

bool is_period(std::string_view s) {
    if (s.empty())
        return false;
    std::size_t i = 0;
    while (i < s.size()) {
        const auto start = i;
        while (i < s.size() && is_digit(s[i]))
            ++i;
        if (i == start || i == s.size())
            return false;
        switch (s[i]) {
            case 'D':
            case 'd':
            case 'W':
            case 'w':
            case 'M':
            case 'm':
            case 'Y':
            case 'y':
                ++i;
                break;
            default:
                return false;
        }
    }
    return true;
}

bool is_valid_date(std::string_view y, std::string_view m, std::string_view d) {
    if (!all_digits(y) || !all_digits(m) || !all_digits(d))
        return false;
    const std::chrono::year_month_day ymd{std::chrono::year{to_int(y)},
                                          std::chrono::month{static_cast<unsigned>(to_int(m))},
                                          std::chrono::day{static_cast<unsigned>(to_int(d))}};
    return ymd.ok();
}

bool is_date(std::string_view s) {
    if (s.size() == 10 && s[4] == '-' && s[7] == '-')
        return is_valid_date(s.substr(0, 4), s.substr(5, 2), s.substr(8, 2));
    if (s.size() == 8)
        return is_valid_date(s.substr(0, 4), s.substr(4, 2), s.substr(6, 2));
    return false;
}

std::vector<std::string_view> split(std::string_view s) {
    std::vector<std::string_view> parts;
    std::size_t start = 0;
    while (true) {
        const auto slash = s.find('/', start);
        parts.push_back(s.substr(start, slash - start));
        if (slash == std::string_view::npos)
            break;
        start = slash + 1;
    }
    return parts;
}

template <std::size_t N>
bool is_one_of(std::string_view s, const std::array<std::string_view, N>& vocabulary) {
    return std::ranges::find(vocabulary, s) != vocabulary.end();
}

// ORE's DeltaVolQuote vocabularies, as parseAtmType and parseDeltaType read them.
constexpr std::array<std::string_view, 7> atm_types{
    "AtmNull", "AtmSpot", "AtmFwd", "AtmDeltaNeutral", "AtmVegaMax", "AtmGammaMax", "AtmPutCall50"};
constexpr std::array<std::string_view, 4> delta_types{"Spot", "Fwd", "PaSpot", "PaFwd"};
constexpr std::array<std::string_view, 2> option_types{"Call", "Put"};
constexpr std::array<std::string_view, 2> moneyness_types{"Spot", "Fwd"};

std::unexpected<std::string> refuse(std::string_view what, std::string_view text) {
    return std::unexpected(std::format("'{}' is not {}", text, what));
}

// A label number with its redundant spelling removed: a leading + goes, and so
// does a fraction of zeros. The digits are otherwise kept as written, because
// rewriting a number changes the value and not only its spelling.
std::string canonical_number(std::string_view text) {
    if (!text.empty() && text.front() == '+')
        text.remove_prefix(1);
    const auto dot = text.find('.');
    if (dot != std::string_view::npos &&
        std::ranges::all_of(text.substr(dot + 1), [](char c) { return c == '0'; }))
        text = text.substr(0, dot);
    return std::string(text);
}

}

term::term(kind k, std::string text)
    : kind_(k)
    , text_(std::move(text)) {}

std::expected<term, std::string> term::parse(std::string_view text) {
    if (text == "ON" || text == "TN" || text == "SN")
        return term(kind::fx_tenor, std::string(text));
    if (text.size() > 1 && text.front() == 'c' && all_digits(text.substr(1)))
        return term(kind::continuation, std::string(text));
    if (is_date(text))
        return term(kind::date, std::string(text));
    if (is_period(text))
        return term(kind::period, std::string(text));
    return refuse("a period, a date, ON, TN, SN or a continuation", text);
}

decimal::decimal(std::string text)
    : text_(std::move(text)) {}

std::expected<decimal, std::string> decimal::parse(std::string_view text) {
    // The estate's one definition of a number: locale-free, and no
    // hexadecimal, inf or nan. The text is kept; the value is only checked.
    if (!ores::platform::numeric::parse_double(text))
        return refuse("a number", text);
    return decimal(std::string(text));
}

code::code(std::string text)
    : text_(std::move(text)) {}

std::expected<code, std::string> code::parse(std::string_view text) {
    if (text.empty() || text.find('/') != std::string_view::npos)
        return refuse("a code", text);
    return code(std::string(text));
}

strike::strike(form f)
    : form_(std::move(f)) {}

std::expected<strike, std::string> strike::parse(std::string_view text) {
    const auto parts = split(text);
    const auto& head = parts.front();

    if (parts.size() == 1) {
        if (head == "ATM")
            return strike(atm_strike{"AtmSpot", std::nullopt, true});
        if (head == "ATMF")
            return strike(atm_strike{"AtmFwd", std::nullopt, true});
        auto level = decimal::parse(head);
        if (!level)
            return refuse("a strike", text);
        return strike(absolute_strike{std::move(*level)});
    }

    if (head == "DEL" && parts.size() == 4) {
        auto delta = decimal::parse(parts[3]);
        if (!is_one_of(parts[1], delta_types) || !is_one_of(parts[2], option_types) || !delta)
            return refuse("a delta strike", text);
        return strike(
            delta_strike{std::string(parts[1]), std::string(parts[2]), std::move(*delta)});
    }

    if (head == "ATM" && (parts.size() == 2 || parts.size() == 4)) {
        if (!is_one_of(parts[1], atm_types))
            return refuse("an ATM strike", text);
        if (parts.size() == 2)
            return strike(atm_strike{std::string(parts[1]), std::nullopt, false});
        if (parts[2] != "DEL" || !is_one_of(parts[3], delta_types))
            return refuse("an ATM strike", text);
        return strike(atm_strike{std::string(parts[1]), std::string(parts[3]), false});
    }

    if (head == "MNY" && parts.size() == 3) {
        auto moneyness = decimal::parse(parts[2]);
        if (!is_one_of(parts[1], moneyness_types) || !moneyness)
            return refuse("a moneyness strike", text);
        return strike(moneyness_strike{std::string(parts[1]), std::move(*moneyness)});
    }

    return refuse("a strike", text);
}

std::string strike::text() const {
    struct writer {
        std::string operator()(const absolute_strike& s) const {
            return s.level.text();
        }
        std::string operator()(const atm_strike& s) const {
            if (s.shorthand)
                return s.atm_type == "AtmFwd" ? "ATMF" : "ATM";
            std::string out = "ATM/" + s.atm_type;
            if (s.delta_type)
                out += "/DEL/" + *s.delta_type;
            return out;
        }
        std::string operator()(const delta_strike& s) const {
            return "DEL/" + s.delta_type + "/" + s.option_type + "/" + s.delta.text();
        }
        std::string operator()(const moneyness_strike& s) const {
            return "MNY/" + s.moneyness_type + "/" + s.moneyness.text();
        }
    };
    return std::visit(writer{}, form_);
}

strike_label::strike_label(form f, std::string number)
    : form_(f)
    , number_(std::move(number)) {}

std::expected<strike_label, std::string> strike_label::parse(std::string_view text) {
    if (text.empty())
        return refuse("a strike label", text);
    if (upper(text) == "ATM")
        return strike_label(form::at_the_money, {});

    // The suffix names the wing, and ORE reads it in either case. The two-letter
    // suffixes come first, so RR is never read as a put on an 'R' delta.
    const struct {
        std::string_view token;
        form kind;
    } suffixes[] = {{"RR", form::risk_reversal},
                    {"BF", form::butterfly},
                    {"C", form::delta_call},
                    {"P", form::delta_put}};
    for (const auto& suffix : suffixes) {
        if (text.size() <= suffix.token.size())
            continue;
        if (upper(text.substr(text.size() - suffix.token.size())) != suffix.token)
            continue;
        const auto number = decimal::parse(text.substr(0, text.size() - suffix.token.size()));
        if (!number)
            return refuse("a strike label", text);
        return strike_label(suffix.kind, canonical_number(number->text()));
    }

    const auto level = decimal::parse(text);
    if (!level)
        return refuse("a strike label", text);
    return strike_label(form::level, canonical_number(level->text()));
}

std::string strike_label::text() const {
    switch (form_) {
        case form::at_the_money:
            return "ATM";
        case form::delta_call:
            return number_ + "C";
        case form::delta_put:
            return number_ + "P";
        case form::risk_reversal:
            return number_ + "RR";
        case form::butterfly:
            return number_ + "BF";
        case form::level:
            return number_;
    }
    return {};
}

std::string text_of(const value& v) {
    struct writer {
        std::string operator()(none_t) const {
            return {};
        }
        std::string operator()(const std::string& s) const {
            return s;
        }
        std::string operator()(const term& t) const {
            return t.text();
        }
        std::string operator()(const decimal& d) const {
            return d.text();
        }
        std::string operator()(const strike& s) const {
            return s.text();
        }
        std::string operator()(const code& c) const {
            return c.text();
        }
        std::string operator()(const strike_label& l) const {
            return l.text();
        }
    };
    return std::visit(writer{}, v);
}

}
