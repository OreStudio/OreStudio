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
#include "ores.marketdata.core/datum/ore_key_codec.hpp"
#include "ore_key_reading.hpp"
#include <algorithm>
#include <array>
#include <format>
#include <optional>
#include <utility>

// GCC and Clang report a missing case in the dispatch switch through -Wall's
// -Wswitch and -Werror; MSVC's equivalent, C4062, is off by default.
#if defined(_MSC_VER)
#pragma warning(error : 4062)
#endif

namespace ores::marketdata::datum {

namespace detail {

void refuse(const std::string& why) { throw refusal(why); }

datum_builder::datum_builder(instrument_type type, quote_type quote)
    : type_(type)
    , quote_(quote) {
    for (const auto& spec : schema_of(type).fields)
        fields_.push_back({spec.name, none});
}

datum_builder& datum_builder::set(field f, value v) {
    for (auto& fv : fields_) {
        if (fv.name == f) {
            fv.held = std::move(v);
            return *this;
        }
    }
    refuse(std::format("{} has no field {}", ore_name(type_), name_of(f)));
}

market_datum datum_builder::build() {
    auto datum = market_datum::make(type_, quote_, std::move(fields_));
    if (!datum)
        refuse(datum.error());
    return std::move(*datum);
}

std::string text(std::string_view t) {
    if (t.empty())
        refuse("a token is empty");
    return std::string(t);
}

term term_of(std::string_view t, std::initializer_list<term::kind> allowed) {
    auto parsed = term::parse(t);
    if (!parsed)
        refuse(parsed.error());
    if (std::ranges::find(allowed, parsed->which()) == allowed.end())
        refuse(std::format("'{}' is not a term this slot takes", t));
    return std::move(*parsed);
}

term period(std::string_view t) { return term_of(t, {term::kind::period}); }

term period_or_date(std::string_view t) {
    return term_of(t, {term::kind::period, term::kind::date});
}

term expiry(std::string_view t) {
    return term_of(t, {term::kind::period, term::kind::date, term::kind::continuation});
}

decimal number(std::string_view t) {
    auto parsed = decimal::parse(t);
    if (!parsed)
        refuse(parsed.error());
    return std::move(*parsed);
}

decimal integer(std::string_view t) {
    // Nine digits at most, so a caller can convert it without overflow.
    if (t.empty() || t.size() > 9 ||
        !std::ranges::all_of(t, [](char c) { return c >= '0' && c <= '9'; }))
        refuse(std::format("'{}' is not an integer", t));
    return number(t);
}

code token(std::string_view t) {
    auto parsed = code::parse(t);
    if (!parsed)
        refuse(parsed.error());
    return std::move(*parsed);
}

strike base_strike(std::string_view t) {
    auto parsed = strike::parse(t);
    if (!parsed)
        refuse(parsed.error());
    if (const auto* atm = std::get_if<atm_strike>(&parsed->which()); atm && atm->shorthand)
        refuse(std::format("'{}' is the equity option's shorthand, not a strike here", t));
    return std::move(*parsed);
}

std::string join(tokens parts) {
    std::string out;
    for (std::size_t i = 0; i < parts.size(); ++i) {
        if (i > 0)
            out += '/';
        out += parts[i];
    }
    return out;
}

void require_size(tokens rest, std::initializer_list<std::size_t> allowed) {
    if (std::ranges::find(allowed, rest.size()) == allowed.end())
        refuse(std::format("{} tokens after the quote type is not a form of this type", rest.size()));
}

void require_quote(quote_type q, std::initializer_list<quote_type> allowed) {
    if (std::ranges::find(allowed, q) == allowed.end())
        refuse(std::format("{} is not a quote type of this instrument", ore_name(q)));
}

bool is_one_period(std::string_view t) {
    if (t.size() < 2)
        return false;
    switch (t.back()) {
    case 'D':
    case 'd':
    case 'W':
    case 'w':
    case 'M':
    case 'm':
    case 'Y':
    case 'y':
        break;
    default:
        return false;
    }
    return std::ranges::all_of(t.substr(0, t.size() - 1), [](char c) { return c >= '0' && c <= '9'; });
}

bool is_doc_clause(std::string_view t) {
    constexpr std::array<std::string_view, 8> clauses{
        "CR", "MM", "MR", "XR", "CR14", "MM14", "MR14", "XR14"};
    return std::ranges::find(clauses, t) != clauses.end();
}

bool is_number(std::string_view t) { return decimal::parse(t).has_value(); }

}

namespace {

/// The first token of each type's key, as its writer spells it.
constexpr std::array<std::string_view, instrument_type_count> type_tokens{
    "ZERO",
    "DISCOUNT",
    "MM",
    "MM_FUTURE",
    "OI_FUTURE",
    "FRA",
    "IMM_FRA",
    "IR_SWAP",
    "BASIS_SWAP",
    "BMA_SWAP",
    "CC_BASIS_SWAP",
    "CC_FIX_FLOAT_SWAP",
    "CDS",
    "CDS_INDEX",
    "FX",
    "FXFWD",
    "HAZARD_RATE",
    "RECOVERY_RATE",
    "ASSUMED_RECOVERY_RATE",
    "SWAPTION",
    "CAPFLOOR",
    "FX_OPTION",
    "ZC_INFLATIONSWAP",
    "ZC_INFLATIONCAPFLOOR",
    "YY_INFLATIONSWAP",
    "YY_INFLATIONCAPFLOOR",
    "SEASONALITY",
    "EQUITY",
    "EQUITY_FWD",
    "EQUITY_DIVIDEND",
    "EQUITY_OPTION",
    "BOND",
    "BOND_FUTURE",
    "BOND_OPTION",
    "BOND_FUTURE_OPTION",
    "INDEX_CDS_OPTION",
    "INDEX_CDS_TRANCHE",
    "COMMODITY",
    "COMMODITY_FWD",
    "CORRELATION",
    "COMMODITY_OPTION",
    "COMMODITY_CALENDAR_SPREAD_OPTION",
    "SHAPE_PROFILE",
    "CPR",
    "RATING"};

std::optional<instrument_type> type_from_token(std::string_view t) {
    // The spellings ORE's parseInstrumentType also reads.
    if (t == "FX_SPOT")
        return instrument_type::fx_spot;
    if (t == "FX_FWD")
        return instrument_type::fx_fwd;
    for (std::size_t i = 0; i < type_tokens.size(); ++i) {
        if (type_tokens[i] == t)
            return static_cast<instrument_type>(i);
    }
    return std::nullopt;
}

std::optional<quote_type> quote_from_token(std::string_view t) {
    // ORE's parseQuoteType reads the deprecated RATE_GVOL as RATE_LNVOL.
    if (t == "RATE_GVOL")
        return quote_type::rate_lnvol;
    return quote_type_named(t);
}

std::vector<std::string_view> split(std::string_view key) {
    std::vector<std::string_view> parts;
    std::size_t start = 0;
    while (true) {
        const auto slash = key.find('/', start);
        parts.push_back(key.substr(start, slash - start));
        if (slash == std::string_view::npos)
            break;
        start = slash + 1;
    }
    return parts;
}

market_datum dispatch(instrument_type t, quote_type q, detail::tokens rest) {
    using it = instrument_type;
    switch (t) {
    case it::zero:
    case it::discount:
    case it::mm:
    case it::mm_future:
    case it::oi_future:
    case it::fra:
    case it::imm_fra:
    case it::ir_swap:
    case it::basis_swap:
    case it::bma_swap:
    case it::cc_basis_swap:
    case it::cc_fix_float_swap:
        return detail::read_rates(t, q, rest);
    case it::cds:
    case it::cds_index:
    case it::hazard_rate:
    case it::recovery_rate:
    case it::assumed_recovery_rate:
    case it::index_cds_option:
    case it::index_cds_tranche:
        return detail::read_credit(t, q, rest);
    case it::swaption:
    case it::capfloor:
    case it::bond_option:
        return detail::read_volatility(t, q, rest);
    case it::fx_spot:
    case it::fx_fwd:
    case it::fx_option:
        return detail::read_fx(t, q, rest);
    case it::zc_inflation_swap:
    case it::zc_inflation_capfloor:
    case it::yy_inflation_swap:
    case it::yy_inflation_capfloor:
    case it::seasonality:
        return detail::read_inflation(t, q, rest);
    case it::equity_spot:
    case it::equity_fwd:
    case it::equity_dividend:
    case it::equity_option:
        return detail::read_equity(t, q, rest);
    case it::commodity_spot:
    case it::commodity_fwd:
    case it::commodity_option:
    case it::commodity_calendar_spread_option:
        return detail::read_commodity(t, q, rest);
    case it::bond:
    case it::bond_future:
    case it::bond_future_option:
    case it::correlation:
    case it::cpr:
    case it::rating:
    case it::shape_profile:
        return detail::read_securities(t, q, rest);
    }
    detail::refuse("unknown instrument type");
}

}

std::expected<market_datum, std::string> ore_key_codec::read(std::string_view key) {
    const auto parts = split(key);
    if (parts.size() < 3)
        return std::unexpected(std::format("'{}' has fewer than three tokens", key));
    const auto type = type_from_token(parts[0]);
    if (!type)
        return std::unexpected(std::format("'{}' is not an ORE instrument type", parts[0]));
    const auto quote = quote_from_token(parts[1]);
    if (!quote)
        return std::unexpected(std::format("'{}' is not an ORE quote type", parts[1]));
    try {
        return dispatch(*type, *quote, detail::tokens(parts).subspan(2));
    } catch (const detail::refusal& r) {
        return std::unexpected(std::format("'{}': {}", key, r.what()));
    }
}

std::string ore_key_codec::write(const market_datum& datum) {
    std::string key(token_of(datum.type()));
    key += '/';
    key += ore_name(datum.quote());
    for (const auto& fv : datum.fields()) {
        if (std::holds_alternative<none_t>(fv.held))
            continue;
        key += '/';
        key += text_of(fv.held);
    }
    return key;
}

std::string_view ore_key_codec::token_of(instrument_type t) {
    return type_tokens[static_cast<std::size_t>(t)];
}

}
