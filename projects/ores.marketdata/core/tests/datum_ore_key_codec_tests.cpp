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
#include "datum_catalogue.hpp"
#include "ores.platform/numeric/floating_point.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cmath>
#include <format>
#include <map>
#include <string>
#include <vector>

/**
 * @file datum_ore_key_codec_tests.cpp
 * @brief The ORE key codec against ORE's own parser.
 *
 * The catalogue under external/ore/catalogue records, for every key form ORE's
 * parser documents and every key the example corpus carries, the fields ORE's
 * parseMarketDatum assigns, or ORE's error. The codec must accept what ORE
 * accepts and refuse what ORE refuses, write each key back as it read it, and
 * hold each token in the field ORE puts it in.
 *
 * The last is checked by translating a datum into what the catalogue's harness
 * prints for ORE's datum: ORE's field names, ORE's defaults for a token the key
 * leaves out, ORE's printing of periods, and the dates ORE derives from a
 * tenor. Every field ORE prints must be produced by the translation, so a field
 * the translation does not know fails rather than goes unchecked.
 */

namespace {

const std::string tags("[marketdata][datum][ore_key_codec]");

using namespace ores::marketdata::datum;
using ores::marketdata::test::accepted_by_ore;
using ores::marketdata::test::canonical_key;
using ores::marketdata::test::catalogue_corpus;
using ores::marketdata::test::catalogue_forms;
using ores::marketdata::test::catalogue_line;

// --- What ORE prints ------------------------------------------------------

/// A period as ORE's to_string prints it: days of a week or more as weeks and
/// days, months of a year or more as years and months, after QuantLib has summed
/// a compound period such as 1Y6M into one unit.
std::string ore_period(const std::string& text) {
    long days = 0, months = 0, weeks = 0, years = 0;
    bool day_units = false, month_units = false;
    std::size_t i = 0;
    int parts = 0;
    char last_unit = 0;
    while (i < text.size()) {
        long n = 0;
        while (i < text.size() && std::isdigit(static_cast<unsigned char>(text[i])))
            n = n * 10 + (text[i++] - '0');
        const char unit = static_cast<char>(std::toupper(static_cast<unsigned char>(text[i++])));
        ++parts;
        last_unit = unit;
        switch (unit) {
        case 'D':
            days += n;
            day_units = true;
            break;
        case 'W':
            weeks += n;
            day_units = true;
            break;
        case 'M':
            months += n;
            month_units = true;
            break;
        default:
            years += n;
            month_units = true;
            break;
        }
    }
    if (parts == 1 && last_unit == 'W')
        return std::format("{}W", weeks);
    if (parts == 1 && last_unit == 'Y')
        return std::format("{}Y", years);
    if (day_units && !month_units) {
        long n = days + 7 * weeks;
        std::string out;
        long w = 0;
        if (n >= 7) {
            w = n / 7;
            out += std::format("{}W", w);
            n %= 7;
        }
        if (n != 0 || w == 0)
            out += std::format("{}D", n);
        return out;
    }
    long n = months + 12 * years;
    std::string out;
    long y = 0;
    if (n >= 12) {
        y = n / 12;
        out += std::format("{}Y", y);
        n %= 12;
    }
    if (n != 0 || y == 0)
        out += std::format("{}M", n);
    return out;
}

/// A date as ORE prints it: YYYY-MM-DD.
std::string ore_date(const std::string& text) {
    if (text.size() == 8)
        return text.substr(0, 4) + "-" + text.substr(4, 2) + "-" + text.substr(6, 2);
    return text;
}

/// The date ORE derives from a period against the catalogue's as-of date,
/// QuantLib's earliest: 1901-01-01 plus the period, moved past a weekend to the
/// Monday after it, as ORE's weekends-only calendar does.
std::string ore_date_from(const std::string& period_text) {
    using namespace std::chrono;
    year_month_day d{year{1901}, January, day{1}};
    sys_days result{d};
    std::size_t i = 0;
    while (i < period_text.size()) {
        int n = 0;
        while (i < period_text.size() && std::isdigit(static_cast<unsigned char>(period_text[i])))
            n = n * 10 + (period_text[i++] - '0');
        const char unit =
            static_cast<char>(std::toupper(static_cast<unsigned char>(period_text[i++])));
        const year_month_day current{result};
        switch (unit) {
        case 'D':
            result += days{n};
            break;
        case 'W':
            result += days{7 * n};
            break;
        case 'M':
            result = sys_days{current.year() / (current.month() + months{n}) / current.day()};
            break;
        default:
            result = sys_days{(current.year() + years{n}) / current.month() / current.day()};
            break;
        }
    }
    const weekday wd{result};
    if (wd == Saturday)
        result += days{2};
    else if (wd == Sunday)
        result += days{1};
    const year_month_day out{result};
    return std::format("{:04}-{:02}-{:02}",
                       static_cast<int>(out.year()),
                       static_cast<unsigned>(out.month()),
                       static_cast<unsigned>(out.day()));
}

/// One field ORE prints, as the datum says it should read.
struct expected {
    std::string text;
    /// Compare as numbers, after dividing the datum's by this: ORE reads a CDS
    /// running spread in basis points.
    bool numeric = false;
    double scale = 1.0;
};

expected exact(std::string s) { return {std::move(s)}; }
expected number_of(const std::string& s, double scale = 1.0) { return {s, true, scale}; }

bool same(const expected& e, const std::string& ore) {
    if (!e.numeric)
        return e.text == ore;
    const auto ours = ores::platform::numeric::parse_double(e.text);
    const auto theirs = ores::platform::numeric::parse_double(ore);
    if (!ours || !theirs)
        return false;
    const double a = *ours / e.scale;
    return std::fabs(a - *theirs) <= 1e-12 * std::max(1.0, std::fabs(a));
}

std::string str(const market_datum& d, field f) {
    return text_of(d.at(f));
}

bool none_in(const market_datum& d, field f) {
    return std::holds_alternative<none_t>(d.at(f));
}

std::string period_of(const market_datum& d, field f) { return ore_period(str(d, f)); }

std::string bool_of(const std::string& token) {
    const bool truth = token == "Y" || token == "YES" || token == "TRUE" || token == "True" ||
                       token == "true" || token == "1";
    return truth ? "true" : "false";
}

/// A number as ORE's harness prints a real: six significant digits.
std::string ore_real(const decimal& n) {
    return std::format("{:.6g}", *ores::platform::numeric::parse_double(n.text()));
}

/// A strike as the harness prints ORE's: its class and its text.
expected strike_of(const market_datum& d, field f) {
    if (none_in(d, f))
        return exact("none");
    const auto* s = std::get_if<strike>(&d.at(f));
    struct printer {
        std::string operator()(const absolute_strike& a) const {
            return "ore::data::AbsoluteStrike:" + ore_real(a.level);
        }
        std::string operator()(const atm_strike& a) const {
            std::string out = "ore::data::AtmStrike:ATM/" + a.atm_type;
            if (a.delta_type)
                out += "/DEL/" + *a.delta_type;
            return out;
        }
        std::string operator()(const delta_strike& a) const {
            return "ore::data::DeltaStrike:DEL/" + a.delta_type + "/" + a.option_type + "/" +
                   ore_real(a.delta);
        }
        std::string operator()(const moneyness_strike& a) const {
            return "ore::data::MoneynessStrike:MNY/" + a.moneyness_type + "/" + ore_real(a.moneyness);
        }
    };
    return exact(std::visit(printer{}, s->which()));
}

/// An expiry as the harness prints ORE's Expiry object.
std::string expiry_object_of(const market_datum& d, field f) {
    const auto* t = std::get_if<term>(&d.at(f));
    switch (t->which()) {
    case term::kind::date:
        return "ore::data::ExpiryDate:" + ore_date(t->text());
    case term::kind::continuation:
        return "ore::data::FutureContinuationExpiry:" + t->text();
    default:
        return "ore::data::ExpiryPeriod:" + ore_period(t->text());
    }
}

std::string day_counter_of(const std::string& token) {
    static const std::map<std::string, std::string> names{{"A365", "Actual/365 (Fixed)"},
                                                          {"A365F", "Actual/365 (Fixed)"},
                                                          {"A360", "Actual/360"},
                                                          {"ACT/ACT", "Actual/Actual (ISDA)"}};
    const auto it = names.find(token);
    return it == names.end() ? "?" + token : it->second;
}

std::string time_unit_of(std::string token) {
    for (auto& c : token)
        c = static_cast<char>(std::toupper(static_cast<unsigned char>(c)));
    if (token == "HOUR" || token == "HOURS" || token == "H" || token == "HR" || token == "HRS")
        return "HOUR";
    return "SECOND";
}

/// A term ORE keeps as a date or a period, printed as ORE prints whichever it is.
std::string term_text(const market_datum& d, field f) {
    const auto* t = std::get_if<term>(&d.at(f));
    return t->which() == term::kind::date ? ore_date(t->text()) : ore_period(t->text());
}

bool is_date(const market_datum& d, field f) {
    const auto* t = std::get_if<term>(&d.at(f));
    return t && t->which() == term::kind::date;
}

/// What the harness prints for ORE's datum, field by field, from ours.
std::map<std::string, expected> ore_view(const market_datum& d) {
    using it = instrument_type;
    using fl = field;
    std::map<std::string, expected> v;
    const auto ccy_or_empty = [&](fl f) { return exact(none_in(d, f) ? "" : str(d, f)); };

    switch (d.type()) {
    case it::zero:
        v["ccy"] = exact(str(d, fl::ccy));
        v["dayCounter"] = exact(day_counter_of(str(d, fl::day_counter)));
        v["date"] = exact(is_date(d, fl::term) ? ore_date(str(d, fl::term)) : "null");
        v["tenor"] = exact(is_date(d, fl::term) ? "0D" : period_of(d, fl::term));
        v["tenorBased"] = exact(is_date(d, fl::term) ? "false" : "true");
        break;
    case it::discount:
        v["ccy"] = exact(str(d, fl::ccy));
        v["date"] = exact(is_date(d, fl::term) ? ore_date(str(d, fl::term)) : "null");
        v["tenor"] = exact(is_date(d, fl::term) ? "0D" : period_of(d, fl::term));
        break;
    case it::mm:
        v["ccy"] = exact(str(d, fl::ccy));
        v["indexName"] = ccy_or_empty(fl::index_name);
        v["fwdStart"] = exact(period_of(d, fl::fwd_start));
        v["term"] = exact(period_of(d, fl::term));
        break;
    case it::mm_future:
        v["ccy"] = exact(str(d, fl::ccy));
        v["expiry"] = exact(str(d, fl::contract_month));
        v["contract"] = exact(str(d, fl::contract));
        v["tenor"] = exact(period_of(d, fl::tenor));
        break;
    case it::oi_future:
        v["ccy"] = exact(str(d, fl::ccy));
        v["contractMonth"] = exact(str(d, fl::contract_month));
        v["contract"] = exact(str(d, fl::contract));
        v["tenor"] = exact(period_of(d, fl::tenor));
        break;
    case it::fra:
        v["ccy"] = exact(str(d, fl::ccy));
        v["fwdStart"] = exact(period_of(d, fl::fwd_start));
        v["term"] = exact(period_of(d, fl::term));
        break;
    case it::imm_fra:
        v["ccy"] = exact(str(d, fl::ccy));
        v["imm1"] = exact(str(d, fl::imm1));
        v["imm2"] = exact(str(d, fl::imm2));
        break;
    case it::ir_swap: {
        const bool dated = is_date(d, fl::fwd_start);
        v["ccy"] = exact(str(d, fl::ccy));
        v["indeName"] = ccy_or_empty(fl::index_name);
        v["tenor"] = exact(period_of(d, fl::tenor));
        v["fwdStart"] = exact(dated ? "0D" : period_of(d, fl::fwd_start));
        v["term"] = exact(dated ? "0D" : period_of(d, fl::term));
        v["startDate"] = exact(dated ? ore_date(str(d, fl::fwd_start)) : "null");
        v["maturityDate"] = exact(dated ? ore_date(str(d, fl::term)) : "null");
        break;
    }
    case it::basis_swap:
        v["flatTerm"] = exact(period_of(d, fl::flat_term));
        v["term"] = exact(period_of(d, fl::term));
        v["ccy"] = exact(str(d, fl::ccy));
        v["maturity"] = exact(period_of(d, fl::maturity));
        break;
    case it::bma_swap:
        v["term"] = exact(period_of(d, fl::term));
        v["ccy"] = exact(str(d, fl::ccy));
        v["maturity"] = exact(period_of(d, fl::maturity));
        break;
    case it::cc_basis_swap:
        v["flatCcy"] = exact(str(d, fl::flat_ccy));
        v["flatTerm"] = exact(period_of(d, fl::flat_term));
        v["ccy"] = exact(str(d, fl::ccy));
        v["term"] = exact(period_of(d, fl::term));
        v["maturity"] = exact(period_of(d, fl::maturity));
        break;
    case it::cc_fix_float_swap:
        v["floatCurrency"] = exact(str(d, fl::float_ccy));
        v["floatTenor"] = exact(period_of(d, fl::float_tenor));
        v["fixedCurrency"] = exact(str(d, fl::fixed_ccy));
        v["fixedTenor"] = exact(period_of(d, fl::fixed_tenor));
        v["maturity"] = exact(period_of(d, fl::maturity));
        break;
    case it::cds:
        v["underlyingName"] = exact(str(d, fl::underlying_name));
        v["seniority"] = ccy_or_empty(fl::seniority);
        v["ccy"] = exact(str(d, fl::ccy));
        v["docClause"] = ccy_or_empty(fl::doc_clause);
        v["term"] = exact(none_in(d, fl::term) ? "0D" : period_of(d, fl::term));
        v["runningSpread"] = none_in(d, fl::running_spread)
                                 ? exact("null")
                                 : number_of(str(d, fl::running_spread), 10000.0);
        break;
    case it::hazard_rate:
        v["underlyingName"] = exact(str(d, fl::underlying_name));
        v["seniority"] = exact(str(d, fl::seniority));
        v["ccy"] = exact(str(d, fl::ccy));
        v["docClause"] = ccy_or_empty(fl::doc_clause);
        v["term"] = exact(period_of(d, fl::term));
        break;
    case it::recovery_rate:
    case it::assumed_recovery_rate:
        v["underlyingName"] = exact(str(d, fl::underlying_name));
        v["seniority"] = ccy_or_empty(fl::seniority);
        v["ccy"] = ccy_or_empty(fl::ccy);
        v["docClause"] = ccy_or_empty(fl::doc_clause);
        break;
    case it::cds_index:
    case it::index_cds_tranche:
        v["cdsIndexName"] = exact(str(d, fl::cds_index_name));
        v["term"] = exact(period_of(d, fl::term));
        v["attachmentPoint"] = d.type() == it::cds_index || none_in(d, fl::attachment_point)
                                   ? number_of("0")
                                   : number_of(str(d, fl::attachment_point));
        v["detachmentPoint"] = number_of(str(d, fl::detachment_point));
        break;
    case it::fx_spot:
        v["unitCcy"] = exact(str(d, fl::unit_ccy));
        v["ccy"] = exact(str(d, fl::ccy));
        break;
    case it::fx_fwd: {
        v["unitCcy"] = exact(str(d, fl::unit_ccy));
        v["ccy"] = exact(str(d, fl::ccy));
        const auto* t = d.get<fl::term>();
        v["term"] = exact(t->which() == term::kind::fx_tenor ? t->text() : term_text(d, fl::term));
        v["conversionFactor"] = number_of("1");
        break;
    }
    case it::fx_option:
        v["unitCcy"] = exact(str(d, fl::unit_ccy));
        v["ccy"] = exact(str(d, fl::ccy));
        v["expiry"] = exact(period_of(d, fl::expiry));
        v["strike"] = exact(str(d, fl::strike_label));
        break;
    case it::swaption:
        v["ccy"] = exact(str(d, fl::ccy));
        v["quoteTag"] = ccy_or_empty(fl::quote_tag);
        v["term"] = exact(period_of(d, fl::term));
        if (!none_in(d, fl::expiry)) {
            v["expiry"] = exact(period_of(d, fl::expiry));
            v["dimension"] = exact(str(d, fl::dimension));
            v["strike"] = number_of(none_in(d, fl::strike_level) ? "0" : str(d, fl::strike_level));
            v["isPayer"] = exact(str(d, fl::payer_receiver) == "R" ? "false" : "true");
        }
        break;
    case it::capfloor:
        v["ccy"] = exact(str(d, fl::ccy));
        v["indexName"] = ccy_or_empty(fl::index_name);
        if (none_in(d, fl::term)) {
            v["indexTenor"] = exact(period_of(d, fl::index_tenor));
        } else {
            v["term"] = exact(period_of(d, fl::term));
            v["underlying"] = exact(period_of(d, fl::index_tenor));
            v["atm"] = exact(bool_of(str(d, fl::atm)));
            v["relative"] = exact(bool_of(str(d, fl::relative)));
            v["strike"] = number_of(str(d, fl::strike_level));
            v["isCap"] = exact(str(d, fl::cap_floor) == "F" ? "false" : "true");
        }
        break;
    case it::bond_option:
        v["qualifier"] = exact(str(d, fl::qualifier));
        v["term"] = exact(period_of(d, fl::term));
        if (!none_in(d, fl::expiry))
            v["expiry"] = exact(period_of(d, fl::expiry));
        break;
    case it::zc_inflation_swap:
    case it::yy_inflation_swap:
        v["index"] = exact(str(d, fl::index));
        v["term"] = exact(period_of(d, fl::term));
        break;
    case it::zc_inflation_capfloor:
    case it::yy_inflation_capfloor:
        v["index"] = exact(str(d, fl::index));
        v["term"] = exact(period_of(d, fl::term));
        v["isCap"] = exact(str(d, fl::cap_floor) == "C" ? "true" : "false");
        v["strike"] = exact(str(d, fl::strike_level));
        break;
    case it::seasonality:
        v["index"] = exact(str(d, fl::index));
        v["type"] = exact(str(d, fl::seasonality_type));
        v["month"] = exact(str(d, fl::month));
        break;
    case it::equity_spot:
        v["eqName"] = exact(str(d, fl::eq_name));
        v["ccy"] = exact(str(d, fl::ccy));
        break;
    case it::equity_fwd:
    case it::equity_dividend: {
        v["eqName"] = exact(str(d, fl::eq_name));
        v["ccy"] = exact(str(d, fl::ccy));
        const auto date = is_date(d, fl::expiry) ? ore_date(str(d, fl::expiry))
                                                 : ore_date_from(str(d, fl::expiry));
        v[d.type() == it::equity_fwd ? "expiryDate" : "tenorDate"] = exact(date);
        break;
    }
    case it::equity_option:
        v["eqName"] = exact(str(d, fl::eq_name));
        v["ccy"] = exact(str(d, fl::ccy));
        v["expiry"] = exact(str(d, fl::expiry));
        v["strike"] = strike_of(d, fl::strike);
        v["isCall"] = exact(str(d, fl::option_type) == "P" ? "false" : "true");
        break;
    case it::bond:
    case it::cpr:
        v["securityID"] = exact(str(d, fl::security_id));
        break;
    case it::bond_future:
        v["securityID"] = exact(str(d, fl::security_id));
        if (!none_in(d, fl::future_contract))
            v["futureContract"] = exact(str(d, fl::future_contract));
        break;
    case it::bond_future_option:
        v["contractName"] = exact(str(d, fl::contract_name));
        v["expiry"] = exact(str(d, fl::expiry));
        v["strike"] = strike_of(d, fl::strike);
        v["isCall"] = exact(str(d, fl::option_type) == "P" ? "false" : "true");
        break;
    case it::index_cds_option:
        v["indexName"] = exact(str(d, fl::index_name));
        v["indexTerm"] = ccy_or_empty(fl::index_term);
        v["expiry"] = exact(expiry_object_of(d, fl::expiry));
        v["strike"] = strike_of(d, fl::strike);
        v["side"] = exact(str(d, fl::side) == "Seller" || str(d, fl::side) == "S" ||
                                  str(d, fl::side) == "Receiver"
                              ? "Seller"
                              : "Buyer");
        break;
    case it::commodity_spot:
        v["commodityName"] = exact(str(d, fl::commodity_name));
        v["quoteCurrency"] = exact(str(d, fl::ccy));
        break;
    case it::commodity_fwd: {
        v["commodityName"] = exact(str(d, fl::commodity_name));
        v["quoteCurrency"] = exact(str(d, fl::ccy));
        const auto* t = d.get<fl::expiry>();
        if (t->which() == term::kind::fx_tenor) {
            v["expiryDate"] = exact("null");
            v["tenor"] = exact("1D");
            v["startTenor"] =
                exact(t->text() == "ON" ? "0D" : t->text() == "TN" ? "1D" : "none");
            v["tenorBased"] = exact("true");
        } else if (t->which() == term::kind::date) {
            v["expiryDate"] = exact(ore_date(t->text()));
            v["tenor"] = exact("0D");
            v["startTenor"] = exact("none");
            v["tenorBased"] = exact("false");
        } else {
            v["expiryDate"] = exact("null");
            v["tenor"] = exact(ore_period(t->text()));
            v["startTenor"] = exact("none");
            v["tenorBased"] = exact("true");
        }
        break;
    }
    case it::commodity_option:
    case it::commodity_calendar_spread_option:
        v["commodityName"] = exact(str(d, fl::commodity_name));
        if (d.quote() == quote_type::shift)
            break;
        v["quoteCurrency"] = exact(str(d, fl::ccy));
        v["expiry"] = exact(expiry_object_of(d, fl::expiry));
        v["strike"] = strike_of(d, fl::strike);
        if (d.type() == it::commodity_option)
            v["optionType"] = exact(str(d, fl::option_type) == "P" ? "Put" : "Call");
        else {
            v["offset"] = exact(str(d, fl::offset));
            v["optionType"] = exact("Call");
        }
        break;
    case it::correlation:
        v["index1"] = exact(str(d, fl::index1));
        v["index2"] = exact(str(d, fl::index2));
        v["expiry"] = exact(str(d, fl::expiry));
        v["strike"] = exact(str(d, fl::strike_label));
        break;
    case it::rating:
        v["id"] = exact(str(d, fl::rating_name));
        v["fromRating"] = exact(str(d, fl::from_rating));
        v["toRating"] = exact(str(d, fl::to_rating));
        break;
    case it::shape_profile:
        v["quoteName"] = exact(str(d, fl::quote_name));
        v["deliveryDate"] = exact(ore_date(str(d, fl::delivery_date)));
        v["startTimeInSec"] = exact(str(d, fl::start_time_in_sec));
        v["timeUnit"] = exact(time_unit_of(str(d, fl::time_unit)));
        v["isDST"] = exact(none_in(d, fl::dst) ? "false" : "true");
        break;
    }
    return v;
}

/// The instrument type ORE reports for a datum: ORE turns a deprecated CDS_INDEX
/// key into a tranche and a bond future price into a bond price, and its enum
/// printer has no name for BOND_FUTURE_OPTION.
std::string ore_instrument_type(const market_datum& d) {
    if (d.type() == instrument_type::cds_index)
        return "INDEX_CDS_TRANCHE";
    if (d.type() == instrument_type::bond_future && d.quote() == quote_type::price)
        return "BOND";
    if (d.type() == instrument_type::bond_future_option)
        return "?";
    return std::string(ore_name(d.type()));
}

/// Every difference between the datum and ORE's reading of the same key.
std::vector<std::string> differences(const market_datum& d, const catalogue_line& ore) {
    std::vector<std::string> out;
    if (ore.at("instrumentType") != ore_instrument_type(d))
        out.push_back("instrumentType: ORE " + ore.at("instrumentType"));
    if (ore.at("quoteType") != ore_name(d.quote()))
        out.push_back("quoteType: ORE " + ore.at("quoteType"));
    auto view = ore_view(d);
    for (const auto& [name, value] : ore) {
        if (name == "key" || name == "accepted" || name == "class" || name == "instrumentType" ||
            name == "quoteType")
            continue;
        const auto it = view.find(name);
        if (it == view.end()) {
            out.push_back(name + ": not checked; ORE " + value);
            continue;
        }
        if (!same(it->second, value))
            out.push_back(name + ": ours " + it->second.text + ", ORE " + value);
        view.erase(it);
    }
    for (const auto& [name, value] : view)
        out.push_back(name + ": ours " + value.text + ", ORE prints no such field");
    return out;
}

struct tally {
    std::size_t lines = 0;
    std::vector<std::string> failures;

    void fail(const std::string& key, const std::string& why) {
        if (failures.size() < 40)
            failures.push_back(key + " -- " + why);
        else if (failures.size() == 40)
            failures.push_back("...");
    }
};

/// Keys ORE accepts and the codec refuses on purpose, because ORE reads part of
/// a token and drops the rest, so no datum could write the key back.
const std::map<std::string, std::string> deliberately_refused{
    {"INDEX_CDS_OPTION/RATE_LNVOL/CDXIG/5Y/1Y",
     "ORE reads 1Y as a strike of 1, not as the expiry after an index term"}};

/// Checks every catalogue line: refusals, write-back and ORE's fields.
tally check(const std::vector<catalogue_line>& lines) {
    tally t;
    for (const auto& line : lines) {
        ++t.lines;
        const auto& key = line.at("key");
        const auto datum = ore_key_codec::read(key);
        if (deliberately_refused.contains(key)) {
            if (datum)
                t.fail(key, "the codec should refuse it: " + deliberately_refused.at(key));
            continue;
        }
        if (!accepted_by_ore(line)) {
            if (datum)
                t.fail(key, "ORE refuses it (" + line.at("error") + ") and the codec accepts it");
            continue;
        }
        if (!datum) {
            t.fail(key, "ORE accepts it and the codec refuses it: " + datum.error());
            continue;
        }
        const auto written = ore_key_codec::write(*datum);
        if (!written)
            t.fail(key, "does not write back: " + written.error());
        else if (*written != canonical_key(key))
            t.fail(key, "writes back as " + *written);
        for (const auto& d : differences(*datum, line))
            t.fail(key, d);
    }
    return t;
}

}

TEST_CASE("the_codec_reads_every_documented_ore_form_as_ore_does", tags) {
    const auto t = check(catalogue_forms());
    CHECK(t.lines == catalogue_forms().size());
    for (const auto& f : t.failures)
        UNSCOPED_INFO(f);
    CHECK(t.failures.empty());
}

TEST_CASE("the_codec_reads_every_corpus_key_as_ore_does", tags) {
    const auto t = check(catalogue_corpus());
    CHECK(t.lines == catalogue_corpus().size());
    for (const auto& f : t.failures)
        UNSCOPED_INFO(f);
    CHECK(t.failures.empty());
}

TEST_CASE("an_alias_reads_as_its_type_and_writes_canonically", tags) {
    const auto spot = ore_key_codec::read("FX_SPOT/RATE/EUR/USD");
    REQUIRE(spot);
    CHECK(spot->type() == instrument_type::fx_spot);
    CHECK(ore_key_codec::write(*spot) == "FX/RATE/EUR/USD");

    const auto gvol = ore_key_codec::read("SWAPTION/RATE_GVOL/EUR/5Y/2Y/ATM");
    REQUIRE(gvol);
    CHECK(gvol->quote() == quote_type::rate_lnvol);
    CHECK(ore_key_codec::write(*gvol) == "SWAPTION/RATE_LNVOL/EUR/5Y/2Y/ATM");
}

TEST_CASE("the_codec_refuses_tokens_it_could_not_write_back", tags) {
    // ORE reads each of these and ignores a token; the codec cannot keep what it
    // would not write, so it refuses them.
    for (const auto* key : {"CAPFLOOR/SHIFT/EUR/6M/C",
                            "INDEX_CDS_TRANCHE/BASE_CORRELATION/CDXIG/5Y/0.03/0.07",
                            "SHAPE_PROFILE/SHAPE_FACTOR/PJM_WH_RT/2027-02-02/0/HOUR/NODST",
                            "SWAPTION/SHIFT/EUR/2Y/P",
                            "INDEX_CDS_OPTION/RATE_LNVOL/CDXIG/5Y/1Y",
                            "INDEX_CDS_OPTION/RATE_LNVOL/CDXIG/5Y/2025-02-19"}) {
        INFO("key: " << key);
        CHECK_FALSE(ore_key_codec::read(key));
    }
}

TEST_CASE("a_flag_outside_its_vocabulary_is_refused", tags) {
    for (const auto* key : {"ZC_INFLATIONCAPFLOOR/RATE_NVOL/EUHICPXT/5Y/X/0.01",
                            "INDEX_CDS_OPTION/PRICE/CDXIG/5Y/1Y/100/X"}) {
        INFO("key: " << key);
        CHECK_FALSE(ore_key_codec::read(key));
    }
}

TEST_CASE("a_running_spread_and_a_doc_clause_are_told_apart_by_value", tags) {
    const auto spread = ore_key_codec::read("CDS/CREDIT_SPREAD/ACME/SNRFOR/USD/5Y/100");
    REQUIRE(spread);
    CHECK(spread->get<field::term>()->text() == "5Y");
    CHECK(spread->get<field::running_spread>()->text() == "100");
    CHECK(std::holds_alternative<none_t>(spread->at(field::doc_clause)));

    const auto clause = ore_key_codec::read("CDS/CREDIT_SPREAD/ACME/SNRFOR/USD/XR14/5Y");
    REQUIRE(clause);
    CHECK(*clause->get<field::doc_clause>() == "XR14");
    CHECK(std::holds_alternative<none_t>(clause->at(field::running_spread)));
}

TEST_CASE("the_equity_option_shorthand_is_a_strike_only_for_equity_options", tags) {
    const auto equity = ore_key_codec::read("EQUITY_OPTION/RATE_LNVOL/SP5/USD/1Y/ATMF");
    REQUIRE(equity);
    CHECK(ore_key_codec::write(*equity) == "EQUITY_OPTION/RATE_LNVOL/SP5/USD/1Y/ATMF");

    CHECK_FALSE(ore_key_codec::read("COMMODITY_OPTION/RATE_LNVOL/WTI/USD/1Y/ATMF"));
    CHECK(ore_key_codec::read("COMMODITY_OPTION/RATE_LNVOL/WTI/USD/1Y/ATM/AtmFwd"));
}

TEST_CASE("a_period_keeps_its_spelling_through_the_round_trip", tags) {
    // ORE prints 12M as 1Y and 14D as 2W; the key keeps what was written.
    for (const auto* key : {"MM/RATE/EUR/0D/12M", "MM/RATE/EUR/0D/14D", "FRA/RATE/EUR/1y/1Y6M"}) {
        INFO("key: " << key);
        const auto datum = ore_key_codec::read(key);
        REQUIRE(datum);
        CHECK(ore_key_codec::write(*datum) == key);
    }
}

TEST_CASE("an_unknown_type_or_quote_is_refused_with_a_reason", tags) {
    const auto type = ore_key_codec::read("NOT_A_TYPE/RATE/EUR");
    REQUIRE_FALSE(type);
    CHECK_FALSE(type.error().empty());

    const auto quote = ore_key_codec::read("ZERO/NOT_A_QUOTE/EUR/A365/1Y");
    REQUIRE_FALSE(quote);
    CHECK_FALSE(quote.error().empty());

    CHECK_FALSE(ore_key_codec::read(""));
    CHECK_FALSE(ore_key_codec::read("ZERO"));
}

TEST_CASE("a_datum_whose_key_reads_as_another_form_has_no_key", tags) {
    // With no seniority, the doc clause XR14 takes the seniority's place, so
    // the key would read back with XR14 as the seniority.
    const auto read = ore_key_codec::read("CDS/CREDIT_SPREAD/ACME/SNRFOR/USD/XR14/5Y");
    REQUIRE(read);
    std::vector<field_value> fields(read->fields().begin(), read->fields().end());
    for (auto& fv : fields) {
        if (fv.name == field::seniority)
            fv.held = none_t{};
    }
    const auto datum = market_datum::make(read->type(), read->quote(), std::move(fields));
    REQUIRE(datum);

    const auto written = ore_key_codec::write(*datum);
    REQUIRE_FALSE(written);
    CHECK_FALSE(written.error().empty());
}
