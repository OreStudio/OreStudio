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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: oresmd_parser.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.marketdata.core/oresmd/oresmd_parser.hpp"
#include "ores.marketdata.core/oresmd/detail/oresmd_string_utils.hpp"
#include "ores.marketdata.core/oresmd/oresmd_exception.hpp"
#include <boost/throw_exception.hpp>
#include <boost/url.hpp>
#include <algorithm>
#include <cctype>
#include <format>
#include <magic_enum/magic_enum.hpp>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>
#include <variant>
#include <vector>

namespace {

using namespace ores::marketdata::domain;
using ores::marketdata::core::oresmd_exception;
using ores::marketdata::core::detail::is_currency_code;
using ores::marketdata::core::detail::to_lower;
using ores::marketdata::core::detail::to_upper;

// A series' points are the datum keys' remaining segments joined with '/' (the
// decomposition's own `point_id`), so a family whose coordinate spans more than one
// identifier field reads it by splitting on the same character.
std::vector<std::string> split_on_slash(std::string_view point) {
    std::vector<std::string> parts;
    auto rest = point;
    while (true) {
        const auto at = rest.find('/');
        if (at == std::string_view::npos) {
            parts.emplace_back(rest);
            return parts;
        }
        parts.emplace_back(rest.substr(0, at));
        rest.remove_prefix(at + 1);
    }
}

/**
 * @brief Query parameters of an oresmd URI, decoded once and indexed by key -- every
 * asset class reads a subset of these, none needs the raw params_view.
 */
struct query_params final {
    std::optional<std::string> ccy;
    std::optional<std::string> index;
    std::optional<std::string> index_spelling;
    std::optional<std::string> tenor;
    std::optional<std::string> second_tenor;
    std::optional<std::string> second_ccy;
    std::optional<std::string> contract_month;
    std::optional<std::string> contract_code;
    std::optional<std::string> second_factor;
    std::optional<std::string> curve_id;
    std::optional<std::string> day_count;
    std::optional<std::string> settle;
    std::optional<std::string> shift;
    std::optional<std::string> strip;
    std::optional<std::string> role;
    std::optional<std::string> type;
    std::optional<std::string> metric;
    std::optional<std::string> quote;
    std::optional<std::string> model;
    std::optional<std::string> delivery;
    std::optional<std::string> source;
    std::optional<std::string> source_spelling;
    std::optional<std::string> name_spelling;
    // The coordinate dimensions, one query key each. A surface's coordinate used
    // to arrive as one comma-joined `point`; the model now names each dimension,
    // so a reader needs no per-family convention to read a URI.
    std::optional<std::string> maturity;
    std::optional<std::string> expiry;
    std::optional<std::string> strike;
    std::optional<std::string> delta;
    std::optional<std::string> smile;
    std::optional<std::string> call_put;
    std::optional<std::string> premium;
    std::optional<std::string> seniority;
    std::optional<std::string> restructuring;
    std::optional<std::string> from_grade;
    std::optional<std::string> to;
    std::optional<std::string> date;
    std::optional<std::string> second;
    std::optional<std::string> period;
    std::optional<std::string> dst;
    std::optional<std::string> month;

    static query_params from(const boost::urls::url_view& u) {
        query_params qp;
        for (const auto& p : u.params()) {
            if (p.key == "ccy")
                qp.ccy = p.value;
            else if (p.key == "index")
                qp.index = p.value;
            else if (p.key == "index_spelling")
                qp.index_spelling = p.value;
            else if (p.key == "tenor")
                qp.tenor = p.value;
            else if (p.key == "second_tenor")
                qp.second_tenor = p.value;
            else if (p.key == "second_ccy")
                qp.second_ccy = p.value;
            else if (p.key == "contract_month")
                qp.contract_month = p.value;
            else if (p.key == "contract_code")
                qp.contract_code = p.value;
            else if (p.key == "second_factor")
                qp.second_factor = p.value;
            else if (p.key == "curve_id")
                qp.curve_id = p.value;
            else if (p.key == "day_count")
                qp.day_count = p.value;
            else if (p.key == "settle")
                qp.settle = p.value;
            else if (p.key == "shift")
                qp.shift = p.value;
            else if (p.key == "strip")
                qp.strip = p.value;
            else if (p.key == "role")
                qp.role = p.value;
            else if (p.key == "type")
                qp.type = p.value;
            else if (p.key == "metric")
                qp.metric = p.value;
            else if (p.key == "quote")
                qp.quote = p.value;
            else if (p.key == "model")
                qp.model = p.value;
            else if (p.key == "point")
                BOOST_THROW_EXCEPTION(oresmd_exception(
                    "The 'point' query key is no longer part of the oresmd grammar: name each "
                    "coordinate with its own key -- maturity, expiry, tenor, strike, delta, "
                    "seniority, restructuring, date and the rest."));
            else if (p.key == "delivery")
                qp.delivery = p.value;
            else if (p.key == "source")
                qp.source = p.value;
            else if (p.key == "source_spelling")
                qp.source_spelling = p.value;
            else if (p.key == "name_spelling")
                qp.name_spelling = p.value;
            else if (p.key == "maturity")
                qp.maturity = p.value;
            else if (p.key == "expiry")
                qp.expiry = p.value;
            else if (p.key == "strike")
                qp.strike = p.value;
            else if (p.key == "delta")
                qp.delta = p.value;
            else if (p.key == "smile")
                qp.smile = p.value;
            else if (p.key == "call_put")
                qp.call_put = p.value;
            else if (p.key == "premium")
                qp.premium = p.value;
            else if (p.key == "seniority")
                qp.seniority = p.value;
            else if (p.key == "restructuring")
                qp.restructuring = p.value;
            else if (p.key == "from")
                qp.from_grade = p.value;
            else if (p.key == "to")
                qp.to = p.value;
            else if (p.key == "date")
                qp.date = p.value;
            else if (p.key == "second")
                qp.second = p.value;
            else if (p.key == "period")
                qp.period = p.value;
            else if (p.key == "dst")
                qp.dst = p.value;
            else if (p.key == "month")
                qp.month = p.value;
            else
                BOOST_THROW_EXCEPTION(
                    oresmd_exception(std::format("Unrecognised oresmd query key: '{}'.", p.key)));
        }
        return qp;
    }
};

template <typename Enum>
Enum parse_enum(std::string_view field, std::string_view value) {
    const auto e = magic_enum::enum_cast<Enum>(value);
    if (!e)
        BOOST_THROW_EXCEPTION(
            oresmd_exception(std::format("Unrecognised {} value: {}", field, value)));
    return *e;
}

/**
 * @brief Whether a quote type carries a coordinate, as the model declares it.
 *
 * The grammar is the models' declaration: every quote type lists the coordinate
 * keys it has, and a key it does not list is a misplaced one rather than a value
 * to store and forget.
 */
bool fx_carries_coordinate(fx_quote_type qt, std::string_view key) {
    switch (qt) {
        case fx_quote_type::spot:
            return false;
        case fx_quote_type::fwd:
            return key == "maturity";
        case fx_quote_type::option:
            return key == "expiry" || key == "delta" || key == "strike";
    }
    return false;
}

/**
 * @brief Refuses any coordinate when the resolved type has no quote type.
 *
 * A fixing's key is an index name and a curve's is the curve: neither carries a
 * coordinate, so a coordinate key on one names nothing. The identity keys the
 * Fields table declares are not coordinates and are not refused here.
 */
void validate_fx_no_coordinates(const query_params& qp) {
    if (qp.maturity)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://fx/... does not support the 'maturity' query key.")));
    if (qp.expiry)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://fx/... does not support the 'expiry' query key.")));
    if (qp.delta)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://fx/... does not support the 'delta' query key.")));
    if (qp.strike)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://fx/... does not support the 'strike' query key.")));
}

void validate_fx_coordinates(fx_quote_type qt, const query_params& qp) {
    if (qp.maturity && !fx_carries_coordinate(qt, "maturity"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://fx/... does not support the 'maturity' query key.")));
    if (qp.expiry && !fx_carries_coordinate(qt, "expiry"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://fx/... does not support the 'expiry' query key.")));
    if (qp.delta && !fx_carries_coordinate(qt, "delta"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://fx/... does not support the 'delta' query key.")));
    if (qp.strike && !fx_carries_coordinate(qt, "strike"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://fx/... does not support the 'strike' query key.")));
}
/**
 * @brief Whether a quote type carries a coordinate, as the model declares it.
 *
 * The grammar is the models' declaration: every quote type lists the coordinate
 * keys it has, and a key it does not list is a misplaced one rather than a value
 * to store and forget.
 */
bool ir_carries_coordinate(ir_quote_type qt, std::string_view key) {
    switch (qt) {
        case ir_quote_type::ir_swap:
            return key == "maturity";
        case ir_quote_type::discount:
            return key == "tenor";
        case ir_quote_type::mm:
            return key == "maturity";
        case ir_quote_type::fra:
            return key == "maturity";
        case ir_quote_type::imm_fra:
            return key == "maturity";
        case ir_quote_type::basis_swap:
            return key == "maturity";
        case ir_quote_type::bma_swap:
            return key == "maturity";
        case ir_quote_type::cc_basis_swap:
            return key == "maturity";
        case ir_quote_type::cc_fix_float_swap:
            return key == "maturity";
        case ir_quote_type::zero:
            return key == "maturity";
        case ir_quote_type::mm_future:
            return key == "contract_code" || key == "tenor";
        case ir_quote_type::oi_future:
            return key == "contract_month" || key == "contract_code" || key == "tenor";
        case ir_quote_type::capfloor:
            return key == "expiry" || key == "strike";
        case ir_quote_type::bond_option:
            return key == "expiry" || key == "smile" || key == "delta" || key == "strike";
        case ir_quote_type::swaption:
            return key == "expiry" || key == "smile" || key == "delta" || key == "strike";
    }
    return false;
}

/**
 * @brief Refuses any coordinate when the resolved type has no quote type.
 *
 * A fixing's key is an index name and a curve's is the curve: neither carries a
 * coordinate, so a coordinate key on one names nothing. The identity keys the
 * Fields table declares are not coordinates and are not refused here.
 */
void validate_ir_no_coordinates(const query_params& qp) {
    if (qp.maturity)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://ir/... does not support the 'maturity' query key.")));
    if (qp.expiry)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://ir/... does not support the 'expiry' query key.")));
    if (qp.strike)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://ir/... does not support the 'strike' query key.")));
    if (qp.delta)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://ir/... does not support the 'delta' query key.")));
    if (qp.smile)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://ir/... does not support the 'smile' query key.")));
}

void validate_ir_coordinates(ir_quote_type qt, const query_params& qp) {
    if (qp.maturity && !ir_carries_coordinate(qt, "maturity"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://ir/... does not support the 'maturity' query key.")));
    if (qp.expiry && !ir_carries_coordinate(qt, "expiry"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://ir/... does not support the 'expiry' query key.")));
    if (qp.strike && !ir_carries_coordinate(qt, "strike"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://ir/... does not support the 'strike' query key.")));
    if (qp.delta && !ir_carries_coordinate(qt, "delta"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://ir/... does not support the 'delta' query key.")));
    if (qp.smile && !ir_carries_coordinate(qt, "smile"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://ir/... does not support the 'smile' query key.")));
}
/**
 * @brief Whether a quote type carries a coordinate, as the model declares it.
 *
 * The grammar is the models' declaration: every quote type lists the coordinate
 * keys it has, and a key it does not list is a misplaced one rather than a value
 * to store and forget.
 */
bool equity_carries_coordinate(equity_quote_type qt, std::string_view key) {
    switch (qt) {
        case equity_quote_type::spot:
            return false;
        case equity_quote_type::dividend:
            return key == "maturity";
        case equity_quote_type::fwd:
            return key == "maturity";
        case equity_quote_type::option:
            return key == "expiry" || key == "delta" || key == "premium" || key == "call_put" ||
                   key == "strike";
    }
    return false;
}

/**
 * @brief Refuses any coordinate when the resolved type has no quote type.
 *
 * A fixing's key is an index name and a curve's is the curve: neither carries a
 * coordinate, so a coordinate key on one names nothing. The identity keys the
 * Fields table declares are not coordinates and are not refused here.
 */
void validate_equity_no_coordinates(const query_params& qp) {
    if (qp.maturity)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://equity/... does not support the 'maturity' query key.")));
    if (qp.expiry)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://equity/... does not support the 'expiry' query key.")));
    if (qp.delta)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://equity/... does not support the 'delta' query key.")));
    if (qp.premium)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://equity/... does not support the 'premium' query key.")));
    if (qp.call_put)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://equity/... does not support the 'call_put' query key.")));
    if (qp.strike)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://equity/... does not support the 'strike' query key.")));
}

void validate_equity_coordinates(equity_quote_type qt, const query_params& qp) {
    if (qp.maturity && !equity_carries_coordinate(qt, "maturity"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://equity/... does not support the 'maturity' query key.")));
    if (qp.expiry && !equity_carries_coordinate(qt, "expiry"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://equity/... does not support the 'expiry' query key.")));
    if (qp.delta && !equity_carries_coordinate(qt, "delta"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://equity/... does not support the 'delta' query key.")));
    if (qp.premium && !equity_carries_coordinate(qt, "premium"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://equity/... does not support the 'premium' query key.")));
    if (qp.call_put && !equity_carries_coordinate(qt, "call_put"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://equity/... does not support the 'call_put' query key.")));
    if (qp.strike && !equity_carries_coordinate(qt, "strike"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://equity/... does not support the 'strike' query key.")));
}
/**
 * @brief Whether a quote type carries a coordinate, as the model declares it.
 *
 * The grammar is the models' declaration: every quote type lists the coordinate
 * keys it has, and a key it does not list is a misplaced one rather than a value
 * to store and forget.
 */
bool credit_carries_coordinate(credit_quote_type qt, std::string_view key) {
    switch (qt) {
        case credit_quote_type::cds:
            return key == "seniority" || key == "restructuring" || key == "tenor";
        case credit_quote_type::hazard_rate:
            return key == "seniority" || key == "restructuring" || key == "tenor";
        case credit_quote_type::recovery_rate:
            return key == "seniority" || key == "restructuring";
        case credit_quote_type::cds_index:
            return key == "tenor" || key == "strike";
        case credit_quote_type::index_cds_tranche:
            return key == "tenor" || key == "strike";
        case credit_quote_type::index_cds_option:
            return key == "tenor" || key == "expiry" || key == "delta" || key == "strike";
    }
    return false;
}

/**
 * @brief Refuses any coordinate when the resolved type has no quote type.
 *
 * A fixing's key is an index name and a curve's is the curve: neither carries a
 * coordinate, so a coordinate key on one names nothing. The identity keys the
 * Fields table declares are not coordinates and are not refused here.
 */
void validate_credit_no_coordinates(const query_params& qp) {
    if (qp.seniority)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://credit/... does not support the 'seniority' query key.")));
    if (qp.restructuring)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://credit/... does not support the 'restructuring' query key.")));
    if (qp.tenor)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://credit/... does not support the 'tenor' query key.")));
    if (qp.expiry)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://credit/... does not support the 'expiry' query key.")));
    if (qp.strike)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://credit/... does not support the 'strike' query key.")));
    if (qp.delta)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://credit/... does not support the 'delta' query key.")));
}

void validate_credit_coordinates(credit_quote_type qt, const query_params& qp) {
    if (qp.seniority && !credit_carries_coordinate(qt, "seniority"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://credit/... does not support the 'seniority' query key.")));
    if (qp.restructuring && !credit_carries_coordinate(qt, "restructuring"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://credit/... does not support the 'restructuring' query key.")));
    if (qp.tenor && !credit_carries_coordinate(qt, "tenor"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://credit/... does not support the 'tenor' query key.")));
    if (qp.expiry && !credit_carries_coordinate(qt, "expiry"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://credit/... does not support the 'expiry' query key.")));
    if (qp.strike && !credit_carries_coordinate(qt, "strike"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://credit/... does not support the 'strike' query key.")));
    if (qp.delta && !credit_carries_coordinate(qt, "delta"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://credit/... does not support the 'delta' query key.")));
}
/**
 * @brief Whether a quote type carries a coordinate, as the model declares it.
 *
 * The grammar is the models' declaration: every quote type lists the coordinate
 * keys it has, and a key it does not list is a misplaced one rather than a value
 * to store and forget.
 */
bool commodity_carries_coordinate(commodity_quote_type qt, std::string_view key) {
    switch (qt) {
        case commodity_quote_type::spot:
            return false;
        case commodity_quote_type::fwd:
            return key == "maturity";
        case commodity_quote_type::option:
            return key == "expiry" || key == "delta" || key == "premium" || key == "call_put" ||
                   key == "strike";
    }
    return false;
}

/**
 * @brief Refuses any coordinate when the resolved type has no quote type.
 *
 * A fixing's key is an index name and a curve's is the curve: neither carries a
 * coordinate, so a coordinate key on one names nothing. The identity keys the
 * Fields table declares are not coordinates and are not refused here.
 */
void validate_commodity_no_coordinates(const query_params& qp) {
    if (qp.maturity)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://commodity/... does not support the 'maturity' query key.")));
    if (qp.expiry)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://commodity/... does not support the 'expiry' query key.")));
    if (qp.delta)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://commodity/... does not support the 'delta' query key.")));
    if (qp.premium)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://commodity/... does not support the 'premium' query key.")));
    if (qp.call_put)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://commodity/... does not support the 'call_put' query key.")));
    if (qp.strike)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://commodity/... does not support the 'strike' query key.")));
}

void validate_commodity_coordinates(commodity_quote_type qt, const query_params& qp) {
    if (qp.maturity && !commodity_carries_coordinate(qt, "maturity"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://commodity/... does not support the 'maturity' query key.")));
    if (qp.expiry && !commodity_carries_coordinate(qt, "expiry"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://commodity/... does not support the 'expiry' query key.")));
    if (qp.delta && !commodity_carries_coordinate(qt, "delta"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://commodity/... does not support the 'delta' query key.")));
    if (qp.premium && !commodity_carries_coordinate(qt, "premium"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://commodity/... does not support the 'premium' query key.")));
    if (qp.call_put && !commodity_carries_coordinate(qt, "call_put"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://commodity/... does not support the 'call_put' query key.")));
    if (qp.strike && !commodity_carries_coordinate(qt, "strike"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://commodity/... does not support the 'strike' query key.")));
}
/**
 * @brief Whether a quote type carries a coordinate, as the model declares it.
 *
 * The grammar is the models' declaration: every quote type lists the coordinate
 * keys it has, and a key it does not list is a misplaced one rather than a value
 * to store and forget.
 */
bool inflation_carries_coordinate(inflation_quote_type qt, std::string_view key) {
    switch (qt) {
        case inflation_quote_type::zc_swap:
            return key == "maturity";
        case inflation_quote_type::yy_swap:
            return key == "maturity";
        case inflation_quote_type::seasonality:
            return key == "month";
        case inflation_quote_type::zc_capfloor:
            return key == "expiry" || key == "call_put" || key == "strike";
        case inflation_quote_type::yy_capfloor:
            return key == "expiry" || key == "call_put" || key == "strike";
        case inflation_quote_type::cf_price:
            return key == "expiry" || key == "call_put" || key == "strike";
    }
    return false;
}

/**
 * @brief Refuses any coordinate when the resolved type has no quote type.
 *
 * A fixing's key is an index name and a curve's is the curve: neither carries a
 * coordinate, so a coordinate key on one names nothing. The identity keys the
 * Fields table declares are not coordinates and are not refused here.
 */
void validate_inflation_no_coordinates(const query_params& qp) {
    if (qp.maturity)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://inflation/... does not support the 'maturity' query key.")));
    if (qp.month)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://inflation/... does not support the 'month' query key.")));
    if (qp.expiry)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://inflation/... does not support the 'expiry' query key.")));
    if (qp.call_put)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://inflation/... does not support the 'call_put' query key.")));
    if (qp.strike)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://inflation/... does not support the 'strike' query key.")));
}

void validate_inflation_coordinates(inflation_quote_type qt, const query_params& qp) {
    if (qp.maturity && !inflation_carries_coordinate(qt, "maturity"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://inflation/... does not support the 'maturity' query key.")));
    if (qp.month && !inflation_carries_coordinate(qt, "month"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://inflation/... does not support the 'month' query key.")));
    if (qp.expiry && !inflation_carries_coordinate(qt, "expiry"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://inflation/... does not support the 'expiry' query key.")));
    if (qp.call_put && !inflation_carries_coordinate(qt, "call_put"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://inflation/... does not support the 'call_put' query key.")));
    if (qp.strike && !inflation_carries_coordinate(qt, "strike"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://inflation/... does not support the 'strike' query key.")));
}
/**
 * @brief Whether a quote type carries a coordinate, as the model declares it.
 *
 * The grammar is the models' declaration: every quote type lists the coordinate
 * keys it has, and a key it does not list is a misplaced one rather than a value
 * to store and forget.
 */
bool correlation_carries_coordinate(correlation_quote_type qt, std::string_view key) {
    switch (qt) {
        case correlation_quote_type::pairwise:
            return key == "expiry" || key == "delta";
    }
    return false;
}

/**
 * @brief Refuses any coordinate when the resolved type has no quote type.
 *
 * A fixing's key is an index name and a curve's is the curve: neither carries a
 * coordinate, so a coordinate key on one names nothing. The identity keys the
 * Fields table declares are not coordinates and are not refused here.
 */
void validate_correlation_no_coordinates(const query_params& qp) {
    if (qp.expiry)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://correlation/... does not support the 'expiry' query key.")));
    if (qp.delta)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://correlation/... does not support the 'delta' query key.")));
}

void validate_correlation_coordinates(correlation_quote_type qt, const query_params& qp) {
    if (qp.expiry && !correlation_carries_coordinate(qt, "expiry"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://correlation/... does not support the 'expiry' query key.")));
    if (qp.delta && !correlation_carries_coordinate(qt, "delta"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://correlation/... does not support the 'delta' query key.")));
}
/**
 * @brief Whether a quote type carries a coordinate, as the model declares it.
 *
 * The grammar is the models' declaration: every quote type lists the coordinate
 * keys it has, and a key it does not list is a misplaced one rather than a value
 * to store and forget.
 */
bool shape_profile_carries_coordinate(shape_profile_quote_type qt, std::string_view key) {
    switch (qt) {
        case shape_profile_quote_type::shape_factor:
            return key == "date" || key == "second" || key == "period" || key == "dst";
    }
    return false;
}

/**
 * @brief Refuses any coordinate when the resolved type has no quote type.
 *
 * A fixing's key is an index name and a curve's is the curve: neither carries a
 * coordinate, so a coordinate key on one names nothing. The identity keys the
 * Fields table declares are not coordinates and are not refused here.
 */
void validate_shape_profile_no_coordinates(const query_params& qp) {
    if (qp.date)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://shape_profile/... does not support the 'date' query key.")));
    if (qp.second)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://shape_profile/... does not support the 'second' query key.")));
    if (qp.period)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://shape_profile/... does not support the 'period' query key.")));
    if (qp.dst)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://shape_profile/... does not support the 'dst' query key.")));
}

void validate_shape_profile_coordinates(shape_profile_quote_type qt, const query_params& qp) {
    if (qp.date && !shape_profile_carries_coordinate(qt, "date"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://shape_profile/... does not support the 'date' query key.")));
    if (qp.second && !shape_profile_carries_coordinate(qt, "second"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://shape_profile/... does not support the 'second' query key.")));
    if (qp.period && !shape_profile_carries_coordinate(qt, "period"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://shape_profile/... does not support the 'period' query key.")));
    if (qp.dst && !shape_profile_carries_coordinate(qt, "dst"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://shape_profile/... does not support the 'dst' query key.")));
}
/**
 * @brief Whether a quote type carries a coordinate, as the model declares it.
 *
 * The grammar is the models' declaration: every quote type lists the coordinate
 * keys it has, and a key it does not list is a misplaced one rather than a value
 * to store and forget.
 */
bool rating_carries_coordinate(rating_quote_type qt, std::string_view key) {
    switch (qt) {
        case rating_quote_type::transition_probability:
            return key == "from" || key == "to";
    }
    return false;
}

/**
 * @brief Refuses any coordinate when the resolved type has no quote type.
 *
 * A fixing's key is an index name and a curve's is the curve: neither carries a
 * coordinate, so a coordinate key on one names nothing. The identity keys the
 * Fields table declares are not coordinates and are not refused here.
 */
void validate_rating_no_coordinates(const query_params& qp) {
    if (qp.from_grade)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://rating/... does not support the 'from' query key.")));
    if (qp.to)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://rating/... does not support the 'to' query key.")));
}

void validate_rating_coordinates(rating_quote_type qt, const query_params& qp) {
    if (qp.from_grade && !rating_carries_coordinate(qt, "from"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://rating/... does not support the 'from' query key.")));
    if (qp.to && !rating_carries_coordinate(qt, "to"))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://rating/... does not support the 'to' query key.")));
}

instrument_type parse_type(const query_params& qp) {
    if (!qp.type)
        return instrument_type::quote;
    return parse_enum<instrument_type>("type", *qp.type);
}

/**
 * @brief Throws if @p value is present -- used to reject a query key another asset class
 * uses but this one does not (e.g. =tenor= on an FX URI), rather than silently ignoring
 * it. Every asset class's allowed key set is documented in the design doc's "Grammar"
 * section; anything outside it for a given =asset_class= is a genuine input error, not
 * forward-compatible noise.
 */
void reject_if_present(std::string_view asset_class,
                       std::string_view key,
                       const std::optional<std::string>& value) {
    if (value)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://{}/... does not support the '{}' query key.", asset_class, key)));
}

/**
 * @brief Throws if @p qp names a =model= on anything but a volatility surface.
 *
 * =model= is the surface's own subtype, so an identifier has somewhere to keep it
 * only when the URI also asked for =type=vol=. Without this check a URI that names
 * a model on a quote parses successfully and drops the value, so the caller cannot
 * tell that what it asked for was ignored.
 */
void reject_model_unless_vol(std::string_view asset_class,
                             const query_params& qp,
                             instrument_type type) {
    if (qp.model && type != instrument_type::vol)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://{}/... 'model' is only meaningful when type=vol.", asset_class)));
}

void validate_fx(const query_params& qp) {
    reject_if_present("fx", "ccy", qp.ccy);
    reject_if_present("fx", "index", qp.index);
    reject_if_present("fx", "tenor", qp.tenor);
    reject_if_present("fx", "second_tenor", qp.second_tenor);
    reject_if_present("fx", "second_ccy", qp.second_ccy);
    reject_if_present("fx", "second_factor", qp.second_factor);
    reject_if_present("fx", "curve_id", qp.curve_id);
    reject_if_present("fx", "settle", qp.settle);
    reject_if_present("fx", "day_count", qp.day_count);
    reject_if_present("fx", "shift", qp.shift);
    reject_if_present("fx", "strip", qp.strip);
    reject_if_present("fx", "role", qp.role);
    reject_if_present("fx", "metric", qp.metric);
    reject_if_present("fx", "delivery", qp.delivery);
    reject_if_present("fx", "name_spelling", qp.name_spelling);
    if (qp.source && parse_type(qp) != instrument_type::fixing)
        BOOST_THROW_EXCEPTION(
            oresmd_exception("oresmd://fx/... 'source' is only meaningful when type=fixing."));
    if (qp.source_spelling && parse_type(qp) != instrument_type::fixing)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            "oresmd://fx/... 'source_spelling' is only meaningful when type=fixing."));
}

void validate_ir(const query_params& qp) {
    reject_if_present("ir", "ccy", qp.ccy);
    reject_if_present("ir", "source", qp.source);
    reject_if_present("ir", "source_spelling", qp.source_spelling);
    reject_if_present("ir", "delivery", qp.delivery);
    reject_if_present("ir", "name_spelling", qp.name_spelling);
    if (qp.metric && parse_type(qp) != instrument_type::quote)
        BOOST_THROW_EXCEPTION(
            oresmd_exception("oresmd://ir/... 'metric' is only meaningful when type=quote."));
}

/*
 * The query keys none of the classes that delegate here models. index, tenor,
 * role and metric belong to another class's grammar, source and source_spelling
 * to an FX fixing, delivery to a commodity or power fixing, and name_spelling to
 * a generic index. equity and credit generate no reject list of their own, so for
 * those two this is the whole of their query validation and a key refused
 * everywhere but its owner has to be named here.
 */
void validate_no_foreign_keys(std::string_view asset_class, const query_params& qp) {
    reject_if_present(asset_class, "index", qp.index);
    reject_if_present(asset_class, "tenor", qp.tenor);
    reject_if_present(asset_class, "role", qp.role);
    reject_if_present(asset_class, "metric", qp.metric);
    reject_if_present(asset_class, "source", qp.source);
    reject_if_present(asset_class, "source_spelling", qp.source_spelling);
    reject_if_present(asset_class, "delivery", qp.delivery);
    reject_if_present(asset_class, "name_spelling", qp.name_spelling);
}

void validate_equity(const query_params& qp) {
    validate_no_foreign_keys("equity", qp);
}

std::string first_segment(const boost::urls::url_view& u) {
    for (const auto seg : u.segments()) {
        if (!seg.empty())
            return std::string(seg);
    }
    BOOST_THROW_EXCEPTION(oresmd_exception("oresmd URI is missing its entity path segment."));
}

/*
 * The inflation index codes the model carries. A fixing's name is the code alone,
 * so a fixing whose code this class does not carry has no index name to write back:
 * it is refused here rather than accepted and dropped. A quote's key names its code
 * in the middle of the key, and every code is accepted for one.
 */
bool inflation_index_code_is_known(std::string_view name) {
    static const std::string_view codes[] = {
        "AUCPI",
        "EUHICP",
        "EUHICPXT",
        "FRHICP",
        "UKRPI",
        "USCPI",
        "ZACPI",
    };
    return std::ranges::any_of(codes, [name](std::string_view known) { return known == name; });
}

market_data_identifier parse_fx(const boost::urls::url_view& u, const query_params& qp) {
    validate_fx(qp);
    fx_market_data_identifier id;
    id.pair = to_upper(first_segment(u));
    if (id.pair.size() != 6 ||
        !std::ranges::all_of(id.pair, [](unsigned char c) { return std::isalpha(c); }))
        BOOST_THROW_EXCEPTION(oresmd_exception(std::format(
            "oresmd://fx/... entity must be a 6-letter currency pair, got: '{}'.", id.pair)));
    id.type = parse_type(qp);
    reject_model_unless_vol("fx", qp, id.type);
    if (qp.quote) {
        // A quote type names a volatility surface as well as a quote: CAPFLOOR
        // arrives as type=vol with quote=capfloor. Anything else that carries a
        // quote key is a genuine input error.
        if (id.type != instrument_type::quote && id.type != instrument_type::vol)
            BOOST_THROW_EXCEPTION(
                oresmd_exception("oresmd://fx/... 'quote' is only meaningful when type=quote."));
        id.quote_type = parse_enum<fx_quote_type>("quote", *qp.quote);
    }
    // The silence the old grammar read as a default: a quote URI that names no
    // quote type takes the asset class's own. Only a quote has one -- a fixing's
    // key is an index name and names no quote type at all.
    if (!id.quote_type && id.type == instrument_type::quote)
        id.quote_type = fx_quote_type::spot;
    if (!id.quote_type && id.type == instrument_type::vol)
        id.quote_type = fx_quote_type::option;
    if (qp.maturity)
        id.maturity = to_lower(*qp.maturity);
    if (qp.expiry) {
        if (!id.vol)
            id.vol.emplace();
        id.vol->expiry = to_upper(*qp.expiry);
    }
    if (qp.delta) {
        if (!id.vol)
            id.vol.emplace();
        id.vol->delta_type = to_upper(*qp.delta);
    }
    if (qp.strike) {
        if (!id.vol)
            id.vol.emplace();
        id.vol->strike = to_upper(*qp.strike);
    }
    // The resolved quote type decides which coordinates exist: a key the model
    // does not give that type is misplaced, not a value to store and forget.
    if (id.quote_type)
        validate_fx_coordinates(*id.quote_type, qp);
    else if (id.type != instrument_type::quote && id.type != instrument_type::vol)
        validate_fx_no_coordinates(qp);
    // A fixing carries the source that published it, lower-cased the way the
    // index families are, with the token as ORE wrote it kept beside it when the
    // two differ. A quote carries neither; validate_fx() refuses them.
    if (qp.source)
        id.source = to_lower(*qp.source);
    if (qp.source_spelling)
        id.source_spelling = *qp.source_spelling;
    if (qp.model && id.type == instrument_type::vol && id.vol)
        id.vol->model_subtype = parse_enum<volatility_model_subtype>("model", *qp.model);
    return id;
}

market_data_identifier parse_ir(const boost::urls::url_view& u, const query_params& qp) {
    validate_ir(qp);
    ir_market_data_identifier id;
    id.ccy = to_upper(first_segment(u));
    id.type = parse_type(qp);
    reject_model_unless_vol("ir", qp, id.type);
    if (qp.index)
        id.index = parse_enum<index_family>("index", *qp.index);
    if (qp.index_spelling)
        id.index_spelling = *qp.index_spelling;
    if (qp.tenor)
        id.tenor = to_lower(*qp.tenor);
    if (qp.second_tenor)
        id.second_tenor = to_lower(*qp.second_tenor);
    if (qp.second_ccy)
        id.second_ccy = to_upper(*qp.second_ccy);
    if (qp.contract_month)
        id.contract_month = *qp.contract_month;
    if (qp.contract_code)
        id.contract_code = *qp.contract_code;
    if (qp.curve_id)
        id.curve_id = *qp.curve_id;
    if (qp.day_count)
        id.day_count = *qp.day_count;
    if (qp.settle)
        id.settle = *qp.settle;
    // A cap/floor surface's displacement and strip flags used to arrive inside
    // the point; they are identity keys and are written in their own right.
    if (qp.shift)
        id.shift = to_lower(*qp.shift);
    if (qp.strip)
        id.strip = to_lower(*qp.strip);
    if (qp.role)
        id.role = parse_enum<curve_role>("role", *qp.role);
    if (qp.metric)
        id.metric = parse_enum<metric>("metric", *qp.metric);
    if (qp.quote) {
        // A quote type names a volatility surface as well as a quote: CAPFLOOR
        // arrives as type=vol with quote=capfloor. Anything else that carries a
        // quote key is a genuine input error, so the gate lives here rather than
        // in the type-gated key table, which admits only type=quote.
        if (id.type != instrument_type::quote && id.type != instrument_type::vol)
            BOOST_THROW_EXCEPTION(
                oresmd_exception("oresmd://ir/... 'quote' is only meaningful when type=quote."));
        id.quote_type = parse_enum<ir_quote_type>("quote", *qp.quote);
    }
    // The silence the old grammar read as a default: a volatility surface that
    // names no quote type is the swaption, and the writer emits the key like any
    // other so a reader never has to know it.
    if (!id.quote_type && id.type == instrument_type::vol)
        id.quote_type = ir_quote_type::swaption;
    // The entity path segment is the currency for every IR identifier except a
    // bond option, whose third key segment is the underlying bond curve's own
    // name -- EUR_GENERIC in the corpus -- rather than a currency.
    const auto entity_is_underlying =
        id.type == instrument_type::vol && id.quote_type == ir_quote_type::bond_option;
    if (!entity_is_underlying && !is_currency_code(id.ccy))
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://ir/... entity must be a currency, got: '{}'.", id.ccy)));
    if (id.quote_type)
        validate_ir_coordinates(*id.quote_type, qp);
    else if (id.type != instrument_type::quote && id.type != instrument_type::vol)
        validate_ir_no_coordinates(qp);
    if (qp.model && id.type == instrument_type::vol) {
        // A shift quote names its surface with no coordinate of its own:
        // CAPFLOOR/SHIFT/CCY/TENOR arrives as
        // type=vol&quote=capfloor&model=shift&tenor=6m. The surface is built
        // from the model alone, so it is created here when no coordinate did.
        if (!id.vol)
            id.vol.emplace();
        id.vol->model_subtype = parse_enum<volatility_model_subtype>("model", *qp.model);
    }
    // The declared coordinates, one query key each. The surface's own live in
    // the vol block; the rest are the identifier's own members.
    if (qp.maturity)
        id.maturity = to_lower(*qp.maturity);
    if (qp.expiry || qp.strike || qp.delta || qp.smile) {
        if (!id.vol)
            id.vol.emplace();
        if (qp.expiry)
            id.vol->expiry = to_upper(*qp.expiry);
        if (qp.strike)
            id.vol->strike = to_upper(*qp.strike);
        if (qp.delta)
            id.vol->delta_type = to_upper(*qp.delta);
        // The smile convention's marker is a name whose case the key has to read
        // back, so it is carried as it arrived.
        if (qp.smile)
            id.vol->smile = *qp.smile;
    }
    if (id.type == instrument_type::fixing && id.index && requires_tenor(*id.index) && !id.tenor)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://ir/... a term index ('{}') fixing requires a tenor.",
                        magic_enum::enum_name(*id.index))));
    return id;
}

market_data_identifier parse_equity(const boost::urls::url_view& u, const query_params& qp) {
    validate_equity(qp);
    equity_market_data_identifier id;
    id.ticker = to_upper(first_segment(u));
    // A fixing's key is an index name, and an index name does not carry the
    // currency the index is quoted in -- EQ-SP5 is the level of an index, not
    // a price in a currency. Every other type still needs one.
    id.type = parse_type(qp);
    if (!qp.ccy && id.type != instrument_type::fixing)
        BOOST_THROW_EXCEPTION(
            oresmd_exception("oresmd://equity/... requires a ccy query key unless type=fixing."));
    if (qp.ccy)
        id.ccy = to_upper(*qp.ccy);
    reject_model_unless_vol("equity", qp, id.type);
    if (qp.quote) {
        // A quote type names a volatility surface as well as a quote: CAPFLOOR
        // arrives as type=vol with quote=capfloor. Anything else that carries a
        // quote key is a genuine input error.
        if (id.type != instrument_type::quote && id.type != instrument_type::vol)
            BOOST_THROW_EXCEPTION(oresmd_exception(
                "oresmd://equity/... 'quote' is only meaningful when type=quote."));
        id.quote_type = parse_enum<equity_quote_type>("quote", *qp.quote);
    }
    // The silence the old grammar read as a default: a quote URI that names no
    // quote type takes the asset class's own. Only a quote has one -- a fixing's
    // key is an index name and names no quote type at all.
    if (!id.quote_type && id.type == instrument_type::quote)
        id.quote_type = equity_quote_type::spot;
    if (!id.quote_type && id.type == instrument_type::vol)
        id.quote_type = equity_quote_type::option;
    if (qp.maturity)
        id.maturity = to_lower(*qp.maturity);
    if (qp.expiry) {
        if (!id.vol)
            id.vol.emplace();
        id.vol->expiry = to_upper(*qp.expiry);
    }
    if (qp.delta) {
        if (!id.vol)
            id.vol.emplace();
        id.vol->delta_type = to_upper(*qp.delta);
    }
    if (qp.premium) {
        if (!id.vol)
            id.vol.emplace();
        id.vol->premium_type = to_upper(*qp.premium);
    }
    if (qp.call_put) {
        if (!id.vol)
            id.vol.emplace();
        id.vol->call_put = to_upper(*qp.call_put);
    }
    if (qp.strike) {
        if (!id.vol)
            id.vol.emplace();
        id.vol->strike = to_upper(*qp.strike);
    }
    // The resolved quote type decides which coordinates exist: a key the model
    // does not give that type is misplaced, not a value to store and forget.
    if (id.quote_type)
        validate_equity_coordinates(*id.quote_type, qp);
    else if (id.type != instrument_type::quote && id.type != instrument_type::vol)
        validate_equity_no_coordinates(qp);
    if (qp.model && id.type == instrument_type::vol && id.vol)
        id.vol->model_subtype = parse_enum<volatility_model_subtype>("model", *qp.model);
    return id;
}

market_data_identifier parse_credit(const boost::urls::url_view& u, const query_params& qp) {
    reject_if_present("credit", "index", qp.index);
    reject_if_present("credit", "second_tenor", qp.second_tenor);
    reject_if_present("credit", "second_ccy", qp.second_ccy);
    reject_if_present("credit", "second_factor", qp.second_factor);
    reject_if_present("credit", "curve_id", qp.curve_id);
    reject_if_present("credit", "settle", qp.settle);
    reject_if_present("credit", "day_count", qp.day_count);
    reject_if_present("credit", "shift", qp.shift);
    reject_if_present("credit", "strip", qp.strip);
    reject_if_present("credit", "role", qp.role);
    reject_if_present("credit", "metric", qp.metric);
    reject_if_present("credit", "source", qp.source);
    reject_if_present("credit", "source_spelling", qp.source_spelling);
    reject_if_present("credit", "delivery", qp.delivery);
    reject_if_present("credit", "name_spelling", qp.name_spelling);
    credit_market_data_identifier id;
    id.reference_entity = to_upper(first_segment(u));
    if (!qp.ccy)
        BOOST_THROW_EXCEPTION(oresmd_exception("oresmd://credit/... requires a ccy query key."));
    id.ccy = to_upper(*qp.ccy);
    id.type = parse_type(qp);
    reject_model_unless_vol("credit", qp, id.type);
    if (qp.quote) {
        // A quote type names a volatility surface as well as a quote: CAPFLOOR
        // arrives as type=vol with quote=capfloor. Anything else that carries a
        // quote key is a genuine input error.
        if (id.type != instrument_type::quote && id.type != instrument_type::vol)
            BOOST_THROW_EXCEPTION(oresmd_exception(
                "oresmd://credit/... 'quote' is only meaningful when type=quote."));
        id.quote_type = parse_enum<credit_quote_type>("quote", *qp.quote);
    }
    // The silence the old grammar read as a default: a quote URI that names no
    // quote type takes the asset class's own. Only a quote has one -- a fixing's
    // key is an index name and names no quote type at all.
    if (!id.quote_type && id.type == instrument_type::quote)
        id.quote_type = credit_quote_type::cds;
    if (qp.seniority)
        id.seniority = to_lower(*qp.seniority);
    if (qp.restructuring)
        id.restructuring = to_lower(*qp.restructuring);
    if (qp.tenor)
        id.tenor = to_lower(*qp.tenor);
    if (qp.expiry) {
        if (!id.vol)
            id.vol.emplace();
        id.vol->expiry = to_upper(*qp.expiry);
    }
    if (qp.strike) {
        if (!id.vol)
            id.vol.emplace();
        id.vol->strike = to_upper(*qp.strike);
    }
    if (qp.delta) {
        if (!id.vol)
            id.vol.emplace();
        id.vol->delta_type = to_upper(*qp.delta);
    }
    // The resolved quote type decides which coordinates exist: a key the model
    // does not give that type is misplaced, not a value to store and forget.
    if (id.quote_type)
        validate_credit_coordinates(*id.quote_type, qp);
    else if (id.type != instrument_type::quote && id.type != instrument_type::vol)
        validate_credit_no_coordinates(qp);
    if (qp.model && id.type == instrument_type::vol && id.vol)
        id.vol->model_subtype = parse_enum<volatility_model_subtype>("model", *qp.model);
    return id;
}

market_data_identifier parse_correlation(const boost::urls::url_view& u, const query_params& qp) {
    reject_if_present("correlation", "ccy", qp.ccy);
    reject_if_present("correlation", "index", qp.index);
    reject_if_present("correlation", "tenor", qp.tenor);
    reject_if_present("correlation", "second_tenor", qp.second_tenor);
    reject_if_present("correlation", "second_ccy", qp.second_ccy);
    reject_if_present("correlation", "curve_id", qp.curve_id);
    reject_if_present("correlation", "settle", qp.settle);
    reject_if_present("correlation", "day_count", qp.day_count);
    reject_if_present("correlation", "shift", qp.shift);
    reject_if_present("correlation", "strip", qp.strip);
    reject_if_present("correlation", "role", qp.role);
    reject_if_present("correlation", "metric", qp.metric);
    reject_if_present("correlation", "source", qp.source);
    reject_if_present("correlation", "source_spelling", qp.source_spelling);
    reject_if_present("correlation", "delivery", qp.delivery);
    reject_if_present("correlation", "name_spelling", qp.name_spelling);
    correlation_market_data_identifier id;
    id.factor_pair = to_upper(first_segment(u));
    id.type = parse_type(qp);
    reject_if_present("correlation", "model", qp.model);
    if (qp.quote) {
        // A quote type names a volatility surface as well as a quote: CAPFLOOR
        // arrives as type=vol with quote=capfloor. Anything else that carries a
        // quote key is a genuine input error.
        if (id.type != instrument_type::quote && id.type != instrument_type::vol)
            BOOST_THROW_EXCEPTION(oresmd_exception(
                "oresmd://correlation/... 'quote' is only meaningful when type=quote."));
        id.quote_type = parse_enum<correlation_quote_type>("quote", *qp.quote);
    }
    if (qp.expiry)
        id.expiry = to_lower(*qp.expiry);
    if (qp.delta)
        id.delta = to_lower(*qp.delta);
    // The resolved quote type decides which coordinates exist: a key the model
    // does not give that type is misplaced, not a value to store and forget.
    if (id.quote_type)
        validate_correlation_coordinates(*id.quote_type, qp);
    else if (id.type != instrument_type::quote && id.type != instrument_type::vol)
        validate_correlation_no_coordinates(qp);
    // A pairwise correlation names a second operand as well as the entity, and
    // the entity carries the first: the two take fixed positions in the key.
    if (qp.second_factor)
        id.second_factor = to_upper(*qp.second_factor);
    return id;
}

market_data_identifier parse_inflation(const boost::urls::url_view& u, const query_params& qp) {
    // Validation: only quote and type are meaningful for inflation.
    reject_if_present("inflation", "ccy", qp.ccy);
    reject_if_present("inflation", "index", qp.index);
    reject_if_present("inflation", "tenor", qp.tenor);
    reject_if_present("inflation", "second_tenor", qp.second_tenor);
    reject_if_present("inflation", "second_ccy", qp.second_ccy);
    reject_if_present("inflation", "second_factor", qp.second_factor);
    reject_if_present("inflation", "curve_id", qp.curve_id);
    reject_if_present("inflation", "settle", qp.settle);
    reject_if_present("inflation", "day_count", qp.day_count);
    reject_if_present("inflation", "shift", qp.shift);
    reject_if_present("inflation", "strip", qp.strip);
    reject_if_present("inflation", "role", qp.role);
    reject_if_present("inflation", "metric", qp.metric);
    reject_if_present("inflation", "source", qp.source);
    reject_if_present("inflation", "source_spelling", qp.source_spelling);
    reject_if_present("inflation", "delivery", qp.delivery);
    reject_if_present("inflation", "name_spelling", qp.name_spelling);
    inflation_market_data_identifier id;
    id.index_code = to_upper(first_segment(u));
    id.type = parse_type(qp);
    reject_model_unless_vol("inflation", qp, id.type);
    if (qp.quote) {
        // A quote type names a volatility surface as well as a quote: CAPFLOOR
        // arrives as type=vol with quote=capfloor. Anything else that carries a
        // quote key is a genuine input error.
        if (id.type != instrument_type::quote && id.type != instrument_type::vol)
            BOOST_THROW_EXCEPTION(oresmd_exception(
                "oresmd://inflation/... 'quote' is only meaningful when type=quote."));
        id.quote_type = parse_enum<inflation_quote_type>("quote", *qp.quote);
    }
    if (qp.maturity)
        id.maturity = to_lower(*qp.maturity);
    if (qp.month)
        id.month = to_lower(*qp.month);
    if (qp.expiry) {
        if (!id.vol)
            id.vol.emplace();
        id.vol->expiry = to_upper(*qp.expiry);
    }
    if (qp.call_put) {
        if (!id.vol)
            id.vol.emplace();
        id.vol->call_put = to_upper(*qp.call_put);
    }
    if (qp.strike) {
        if (!id.vol)
            id.vol.emplace();
        id.vol->strike = to_upper(*qp.strike);
    }
    // The resolved quote type decides which coordinates exist: a key the model
    // does not give that type is misplaced, not a value to store and forget.
    if (id.quote_type)
        validate_inflation_coordinates(*id.quote_type, qp);
    else if (id.type != instrument_type::quote && id.type != instrument_type::vol)
        validate_inflation_no_coordinates(qp);
    if (id.type == instrument_type::fixing && !inflation_index_code_is_known(id.index_code))
        BOOST_THROW_EXCEPTION(oresmd_exception(std::format(
            "oresmd://inflation/... '{}' is not an index code the model carries, so it has no "
            "index name to write back.",
            id.index_code)));
    if (qp.model && id.type == instrument_type::vol && id.vol)
        id.vol->model_subtype = parse_enum<volatility_model_subtype>("model", *qp.model);
    return id;
}

market_data_identifier parse_commodity(const boost::urls::url_view& u, const query_params& qp) {
    reject_if_present("commodity", "index", qp.index);
    reject_if_present("commodity", "tenor", qp.tenor);
    reject_if_present("commodity", "second_tenor", qp.second_tenor);
    reject_if_present("commodity", "second_ccy", qp.second_ccy);
    reject_if_present("commodity", "second_factor", qp.second_factor);
    reject_if_present("commodity", "curve_id", qp.curve_id);
    reject_if_present("commodity", "settle", qp.settle);
    reject_if_present("commodity", "day_count", qp.day_count);
    reject_if_present("commodity", "shift", qp.shift);
    reject_if_present("commodity", "strip", qp.strip);
    reject_if_present("commodity", "role", qp.role);
    reject_if_present("commodity", "metric", qp.metric);
    reject_if_present("commodity", "source", qp.source);
    reject_if_present("commodity", "source_spelling", qp.source_spelling);
    reject_if_present("commodity", "name_spelling", qp.name_spelling);
    commodity_market_data_identifier id;
    id.commodity_code = to_upper(first_segment(u));
    // A fixing's key is an index name, and an index name does not carry the
    // currency the index is quoted in -- EQ-SP5 is the level of an index, not
    // a price in a currency. Every other type still needs one.
    id.type = parse_type(qp);
    if (!qp.ccy && id.type != instrument_type::fixing)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            "oresmd://commodity/... requires a ccy query key unless type=fixing."));
    if (qp.ccy)
        id.ccy = to_upper(*qp.ccy);
    reject_model_unless_vol("commodity", qp, id.type);
    if (qp.quote) {
        // A quote type names a volatility surface as well as a quote: CAPFLOOR
        // arrives as type=vol with quote=capfloor. Anything else that carries a
        // quote key is a genuine input error.
        if (id.type != instrument_type::quote && id.type != instrument_type::vol)
            BOOST_THROW_EXCEPTION(oresmd_exception(
                "oresmd://commodity/... 'quote' is only meaningful when type=quote."));
        id.quote_type = parse_enum<commodity_quote_type>("quote", *qp.quote);
    }
    // The silence the old grammar read as a default: a quote URI that names no
    // quote type takes the asset class's own. Only a quote has one -- a fixing's
    // key is an index name and names no quote type at all.
    if (!id.quote_type && id.type == instrument_type::quote)
        id.quote_type = commodity_quote_type::spot;
    if (!id.quote_type && id.type == instrument_type::vol)
        id.quote_type = commodity_quote_type::option;
    if (qp.maturity)
        id.maturity = to_lower(*qp.maturity);
    if (qp.expiry) {
        if (!id.vol)
            id.vol.emplace();
        id.vol->expiry = to_upper(*qp.expiry);
    }
    if (qp.delta) {
        if (!id.vol)
            id.vol.emplace();
        id.vol->delta_type = to_upper(*qp.delta);
    }
    if (qp.premium) {
        if (!id.vol)
            id.vol.emplace();
        id.vol->premium_type = to_upper(*qp.premium);
    }
    if (qp.call_put) {
        if (!id.vol)
            id.vol.emplace();
        id.vol->call_put = to_upper(*qp.call_put);
    }
    if (qp.strike) {
        if (!id.vol)
            id.vol.emplace();
        id.vol->strike = to_upper(*qp.strike);
    }
    // The resolved quote type decides which coordinates exist: a key the model
    // does not give that type is misplaced, not a value to store and forget.
    if (id.quote_type)
        validate_commodity_coordinates(*id.quote_type, qp);
    else if (id.type != instrument_type::quote && id.type != instrument_type::vol)
        validate_commodity_no_coordinates(qp);
    if (qp.delivery) {
        // A delivery coordinate is a fixing's: a commodity future's or a bond
        // future's contract month, or an intraday power index's window. A quote
        // carries that period in the name it is quoted under, so a delivery on one
        // names nothing and is refused rather than accepted and dropped.
        if (id.type != instrument_type::fixing)
            BOOST_THROW_EXCEPTION(oresmd_exception(
                "oresmd://commodity/... 'delivery' is only meaningful when type=fixing."));
        id.delivery = to_lower(*qp.delivery);
    }
    if (qp.model && id.type == instrument_type::vol && id.vol)
        id.vol->model_subtype = parse_enum<volatility_model_subtype>("model", *qp.model);
    return id;
}

market_data_identifier parse_security(const boost::urls::url_view& u, const query_params& qp) {
    reject_if_present("security", "ccy", qp.ccy);
    reject_if_present("security", "index", qp.index);
    reject_if_present("security", "tenor", qp.tenor);
    reject_if_present("security", "second_tenor", qp.second_tenor);
    reject_if_present("security", "second_ccy", qp.second_ccy);
    reject_if_present("security", "second_factor", qp.second_factor);
    reject_if_present("security", "curve_id", qp.curve_id);
    reject_if_present("security", "settle", qp.settle);
    reject_if_present("security", "day_count", qp.day_count);
    reject_if_present("security", "shift", qp.shift);
    reject_if_present("security", "strip", qp.strip);
    reject_if_present("security", "role", qp.role);
    reject_if_present("security", "metric", qp.metric);
    reject_if_present("security", "source", qp.source);
    reject_if_present("security", "source_spelling", qp.source_spelling);
    reject_if_present("security", "name_spelling", qp.name_spelling);
    security_market_data_identifier id;
    id.security_id = to_upper(first_segment(u));
    id.type = parse_type(qp);
    reject_if_present("security", "model", qp.model);
    if (qp.quote) {
        // A quote type names a volatility surface as well as a quote: CAPFLOOR
        // arrives as type=vol with quote=capfloor. Anything else that carries a
        // quote key is a genuine input error.
        if (id.type != instrument_type::quote && id.type != instrument_type::vol)
            BOOST_THROW_EXCEPTION(oresmd_exception(
                "oresmd://security/... 'quote' is only meaningful when type=quote."));
        id.quote_type = parse_enum<security_quote_type>("quote", *qp.quote);
    }
    if (qp.delivery) {
        // A delivery coordinate is a fixing's: a commodity future's or a bond
        // future's contract month, or an intraday power index's window. A quote
        // carries that period in the name it is quoted under, so a delivery on one
        // names nothing and is refused rather than accepted and dropped.
        if (id.type != instrument_type::fixing)
            BOOST_THROW_EXCEPTION(oresmd_exception(
                "oresmd://security/... 'delivery' is only meaningful when type=fixing."));
        id.delivery = to_lower(*qp.delivery);
    }
    return id;
}

market_data_identifier parse_shape_profile(const boost::urls::url_view& u, const query_params& qp) {
    reject_if_present("shape_profile", "ccy", qp.ccy);
    reject_if_present("shape_profile", "index", qp.index);
    reject_if_present("shape_profile", "tenor", qp.tenor);
    reject_if_present("shape_profile", "second_tenor", qp.second_tenor);
    reject_if_present("shape_profile", "second_ccy", qp.second_ccy);
    reject_if_present("shape_profile", "second_factor", qp.second_factor);
    reject_if_present("shape_profile", "curve_id", qp.curve_id);
    reject_if_present("shape_profile", "settle", qp.settle);
    reject_if_present("shape_profile", "day_count", qp.day_count);
    reject_if_present("shape_profile", "shift", qp.shift);
    reject_if_present("shape_profile", "strip", qp.strip);
    reject_if_present("shape_profile", "role", qp.role);
    reject_if_present("shape_profile", "metric", qp.metric);
    reject_if_present("shape_profile", "source", qp.source);
    reject_if_present("shape_profile", "source_spelling", qp.source_spelling);
    reject_if_present("shape_profile", "delivery", qp.delivery);
    reject_if_present("shape_profile", "name_spelling", qp.name_spelling);
    shape_profile_market_data_identifier id;
    id.profile_id = to_upper(first_segment(u));
    id.type = parse_type(qp);
    reject_if_present("shape_profile", "model", qp.model);
    if (qp.quote) {
        // A quote type names a volatility surface as well as a quote: CAPFLOOR
        // arrives as type=vol with quote=capfloor. Anything else that carries a
        // quote key is a genuine input error.
        if (id.type != instrument_type::quote && id.type != instrument_type::vol)
            BOOST_THROW_EXCEPTION(oresmd_exception(
                "oresmd://shape_profile/... 'quote' is only meaningful when type=quote."));
        id.quote_type = parse_enum<shape_profile_quote_type>("quote", *qp.quote);
    }
    if (qp.date)
        id.date = to_lower(*qp.date);
    if (qp.second)
        id.second = to_lower(*qp.second);
    if (qp.period)
        id.period = to_lower(*qp.period);
    if (qp.dst)
        id.dst = to_lower(*qp.dst);
    // The resolved quote type decides which coordinates exist: a key the model
    // does not give that type is misplaced, not a value to store and forget.
    if (id.quote_type)
        validate_shape_profile_coordinates(*id.quote_type, qp);
    else if (id.type != instrument_type::quote && id.type != instrument_type::vol)
        validate_shape_profile_no_coordinates(qp);
    return id;
}

market_data_identifier parse_rating(const boost::urls::url_view& u, const query_params& qp) {
    reject_if_present("rating", "ccy", qp.ccy);
    reject_if_present("rating", "index", qp.index);
    reject_if_present("rating", "tenor", qp.tenor);
    reject_if_present("rating", "second_tenor", qp.second_tenor);
    reject_if_present("rating", "second_ccy", qp.second_ccy);
    reject_if_present("rating", "second_factor", qp.second_factor);
    reject_if_present("rating", "curve_id", qp.curve_id);
    reject_if_present("rating", "settle", qp.settle);
    reject_if_present("rating", "day_count", qp.day_count);
    reject_if_present("rating", "shift", qp.shift);
    reject_if_present("rating", "strip", qp.strip);
    reject_if_present("rating", "role", qp.role);
    reject_if_present("rating", "metric", qp.metric);
    reject_if_present("rating", "source", qp.source);
    reject_if_present("rating", "source_spelling", qp.source_spelling);
    reject_if_present("rating", "delivery", qp.delivery);
    reject_if_present("rating", "name_spelling", qp.name_spelling);
    rating_market_data_identifier id;
    id.provider_id = to_upper(first_segment(u));
    id.type = parse_type(qp);
    reject_if_present("rating", "model", qp.model);
    if (qp.quote) {
        // A quote type names a volatility surface as well as a quote: CAPFLOOR
        // arrives as type=vol with quote=capfloor. Anything else that carries a
        // quote key is a genuine input error.
        if (id.type != instrument_type::quote && id.type != instrument_type::vol)
            BOOST_THROW_EXCEPTION(oresmd_exception(
                "oresmd://rating/... 'quote' is only meaningful when type=quote."));
        id.quote_type = parse_enum<rating_quote_type>("quote", *qp.quote);
    }
    if (qp.from_grade)
        id.from = to_lower(*qp.from_grade);
    if (qp.to)
        id.to = to_lower(*qp.to);
    // The resolved quote type decides which coordinates exist: a key the model
    // does not give that type is misplaced, not a value to store and forget.
    if (id.quote_type)
        validate_rating_coordinates(*id.quote_type, qp);
    else if (id.type != instrument_type::quote && id.type != instrument_type::vol)
        validate_rating_no_coordinates(qp);
    return id;
}

market_data_identifier parse_power(const boost::urls::url_view& u, const query_params& qp) {
    reject_if_present("power", "ccy", qp.ccy);
    reject_if_present("power", "index", qp.index);
    reject_if_present("power", "index_spelling", qp.index_spelling);
    reject_if_present("power", "tenor", qp.tenor);
    reject_if_present("power", "second_tenor", qp.second_tenor);
    reject_if_present("power", "second_ccy", qp.second_ccy);
    reject_if_present("power", "contract_month", qp.contract_month);
    reject_if_present("power", "contract_code", qp.contract_code);
    reject_if_present("power", "second_factor", qp.second_factor);
    reject_if_present("power", "curve_id", qp.curve_id);
    reject_if_present("power", "day_count", qp.day_count);
    reject_if_present("power", "settle", qp.settle);
    reject_if_present("power", "shift", qp.shift);
    reject_if_present("power", "strip", qp.strip);
    reject_if_present("power", "role", qp.role);
    reject_if_present("power", "metric", qp.metric);
    reject_if_present("power", "quote", qp.quote);
    reject_if_present("power", "source", qp.source);
    reject_if_present("power", "source_spelling", qp.source_spelling);
    reject_if_present("power", "name_spelling", qp.name_spelling);
    power_market_data_identifier id;
    id.commodity_code = to_upper(first_segment(u));
    id.type = parse_type(qp);
    reject_if_present("power", "model", qp.model);
    if (qp.delivery) {
        // A delivery coordinate is a fixing's: a commodity future's or a bond
        // future's contract month, or an intraday power index's window. A quote
        // carries that period in the name it is quoted under, so a delivery on one
        // names nothing and is refused rather than accepted and dropped.
        if (id.type != instrument_type::fixing)
            BOOST_THROW_EXCEPTION(oresmd_exception(
                "oresmd://power/... 'delivery' is only meaningful when type=fixing."));
        id.delivery = to_lower(*qp.delivery);
    }
    return id;
}

market_data_identifier parse_generic(const boost::urls::url_view& u, const query_params& qp) {
    reject_if_present("generic", "ccy", qp.ccy);
    reject_if_present("generic", "index", qp.index);
    reject_if_present("generic", "index_spelling", qp.index_spelling);
    reject_if_present("generic", "tenor", qp.tenor);
    reject_if_present("generic", "second_tenor", qp.second_tenor);
    reject_if_present("generic", "second_ccy", qp.second_ccy);
    reject_if_present("generic", "contract_month", qp.contract_month);
    reject_if_present("generic", "contract_code", qp.contract_code);
    reject_if_present("generic", "second_factor", qp.second_factor);
    reject_if_present("generic", "curve_id", qp.curve_id);
    reject_if_present("generic", "day_count", qp.day_count);
    reject_if_present("generic", "settle", qp.settle);
    reject_if_present("generic", "shift", qp.shift);
    reject_if_present("generic", "strip", qp.strip);
    reject_if_present("generic", "role", qp.role);
    reject_if_present("generic", "metric", qp.metric);
    reject_if_present("generic", "quote", qp.quote);
    reject_if_present("generic", "source", qp.source);
    reject_if_present("generic", "source_spelling", qp.source_spelling);
    reject_if_present("generic", "delivery", qp.delivery);
    generic_market_data_identifier id;
    id.name = to_upper(first_segment(u));
    id.type = parse_type(qp);
    reject_if_present("generic", "model", qp.model);
    // The spelling is the token as ORE wrote it, so it is carried raw rather than
    // case-normalised with the name it sits beside. It has to be that name in
    // another case: a spelling that is not would leave the identifier holding two
    // identities, the one its entity names and the one its index name reads back
    // as.
    if (qp.name_spelling) {
        if (to_upper(*qp.name_spelling) != id.name)
            BOOST_THROW_EXCEPTION(oresmd_exception(
                std::format("oresmd://generic/... name_spelling '{}' is not the name '{}' in "
                            "another case.",
                            *qp.name_spelling,
                            id.name)));
        id.name_spelling = *qp.name_spelling;
    }
    return id;
}

void append_if(boost::urls::url& u, std::string_view key, const std::optional<std::string>& v) {
    if (v && !v->empty())
        u.params().append({key, *v});
}

// A surface's own coordinates are plain strings rather than optionals, and an
// unset one is the empty string: the writer emits the keys the grammar defines
// for the type, not an empty key for every coordinate the model declares.
void append_if(boost::urls::url& u, std::string_view key, const std::string& v) {
    if (!v.empty())
        u.params().append({key, v});
}

template <typename Enum>
void append_enum_if(boost::urls::url& u, std::string_view key, const std::optional<Enum>& v) {
    if (v)
        u.params().append({key, std::string(magic_enum::enum_name(*v))});
}

}

namespace ores::marketdata::core {

namespace {

// Which identifier fields the canonical container governs: the surface and
// coordinate spellings oresmd keeps no repository of. A coordinate whose value
// is not in the supplied set is refused, so a caller can pin the spellings its
// own reference data knows.
void validate_canonical(const domain::market_data_identifier& identifier,
                        const canonical_values& canonical) {
    const auto check_tenor = [&canonical](const std::optional<std::string>& v) {
        if (v && !canonical.tenor.contains(*v))
            BOOST_THROW_EXCEPTION(oresmd_exception(std::format(
                "Unknown tenor spelling '{}': not in the supplied canonical values.", *v)));
    };
    const auto check_coordinate = [&canonical](std::string_view key, const std::string& v) {
        if (!v.empty() && !canonical.coordinate.contains(v))
            BOOST_THROW_EXCEPTION(oresmd_exception(std::format(
                "Unknown {} spelling '{}': not in the supplied canonical values.", key, v)));
    };
    const auto check_optional = [&check_coordinate](std::string_view key,
                                                    const std::optional<std::string>& v) {
        if (v)
            check_coordinate(key, *v);
    };

    std::visit(
        [&](const auto& id) {
            using T = std::decay_t<decltype(id)>;
            if constexpr (std::is_same_v<T, ir_market_data_identifier>) {
                check_tenor(id.tenor);
            }
            if constexpr (requires { id.maturity; })
                check_optional("maturity", id.maturity);
            if constexpr (requires { id.seniority; })
                check_optional("seniority", id.seniority);
            if constexpr (requires { id.restructuring; })
                check_optional("restructuring", id.restructuring);
            if constexpr (requires { id.month; })
                check_optional("month", id.month);
            if constexpr (std::is_same_v<T, credit_market_data_identifier>)
                check_optional("tenor", id.tenor);
            if constexpr (requires { id.from; })
                check_optional("from", id.from);
            if constexpr (requires { id.to; })
                check_optional("to", id.to);
            if constexpr (requires { id.date; })
                check_optional("date", id.date);
            if constexpr (requires { id.second; })
                check_optional("second", id.second);
            if constexpr (requires { id.period; })
                check_optional("period", id.period);
            if constexpr (requires { id.dst; })
                check_optional("dst", id.dst);
            if constexpr (requires { id.expiry; })
                check_optional("expiry", id.expiry);
            if constexpr (requires { id.delta; })
                check_optional("delta", id.delta);
            if constexpr (requires { id.vol; }) {
                if (id.vol) {
                    check_coordinate("expiry", id.vol->expiry);
                    check_coordinate("strike", id.vol->strike);
                    check_optional("delta", id.vol->delta_type);
                    check_optional("call_put", id.vol->call_put);
                    check_optional("premium", id.vol->premium_type);
                    // The smile marker is a name, not a canonical spelling.
                }
            }
        },
        identifier);
}

}

market_data_identifier oresmd_parser::parse(const domain::oresmd_uri& uri) {
    const auto r = boost::urls::parse_uri(uri.value);
    if (r.has_error())
        BOOST_THROW_EXCEPTION(
            oresmd_exception(std::format("Failed to parse oresmd URI: {}", uri.value)));

    const auto& u = r.value();
    if (u.scheme() != domain::oresmd_scheme)
        BOOST_THROW_EXCEPTION(oresmd_exception(std::format(
            "Unrecognised URI scheme: {} (expected {})", u.scheme(), domain::oresmd_scheme)));

    const auto asset_token = to_lower(u.host());
    const auto qp = query_params::from(u);

    if (asset_token == "fx")
        return parse_fx(u, qp);
    if (asset_token == "ir")
        return parse_ir(u, qp);
    if (asset_token == "equity")
        return parse_equity(u, qp);
    if (asset_token == "credit")
        return parse_credit(u, qp);
    if (asset_token == "commodity")
        return parse_commodity(u, qp);
    if (asset_token == "inflation")
        return parse_inflation(u, qp);
    if (asset_token == "correlation")
        return parse_correlation(u, qp);
    if (asset_token == "security")
        return parse_security(u, qp);
    if (asset_token == "shape_profile")
        return parse_shape_profile(u, qp);
    if (asset_token == "rating")
        return parse_rating(u, qp);
    if (asset_token == "power")
        return parse_power(u, qp);
    if (asset_token == "generic")
        return parse_generic(u, qp);

    BOOST_THROW_EXCEPTION(
        oresmd_exception(std::format("Unrecognised oresmd asset class: {}", asset_token)));
}

domain::oresmd_uri oresmd_parser::to_uri(const domain::market_data_identifier& identifier) {
    boost::urls::url u;
    u.set_scheme(domain::oresmd_scheme);

    std::visit(
        [&u](const auto& id) {
            using T = std::decay_t<decltype(id)>;
            if constexpr (std::is_same_v<T, fx_market_data_identifier>) {
                u.set_host("fx");
                u.segments().push_back(to_lower(id.pair));
                u.params().append({"type", std::string(magic_enum::enum_name(id.type))});
                append_enum_if(u, "quote", id.quote_type);
                append_if(u, "source", id.source);
                append_if(u, "source_spelling", id.source_spelling);
                append_if(u, "maturity", id.maturity);
                if (id.vol)
                    append_if(u, "expiry", id.vol->expiry);
                if (id.vol)
                    append_if(u, "delta", id.vol->delta_type);
                if (id.vol)
                    append_if(u, "strike", id.vol->strike);
                // The model is written whether or not it holds the default, and
                // only where it means something: a reader must not have to know
                // that an absent model on a surface means lognormal.
                if (id.type == instrument_type::vol)
                    u.params().append({"model",
                                       std::string(magic_enum::enum_name(
                                           id.vol ? id.vol->model_subtype :
                                                    volatility_model_subtype::rate_lnvol))});
            } else if constexpr (std::is_same_v<T, ir_market_data_identifier>) {
                u.set_host("ir");
                u.segments().push_back(to_lower(id.ccy));
                append_enum_if(u, "index", id.index);
                append_if(u, "index_spelling", id.index_spelling);
                append_if(u, "tenor", id.tenor);
                append_if(u, "second_tenor", id.second_tenor);
                append_if(u, "second_ccy", id.second_ccy);
                append_if(u, "contract_month", id.contract_month);
                append_if(u, "contract_code", id.contract_code);
                append_if(u, "shift", id.shift);
                append_if(u, "strip", id.strip);
                append_if(u, "curve_id", id.curve_id);
                append_if(u, "day_count", id.day_count);
                append_if(u, "settle", id.settle);
                append_enum_if(u, "role", id.role);
                u.params().append({"type", std::string(magic_enum::enum_name(id.type))});
                append_enum_if(u, "metric", id.metric);
                append_enum_if(u, "quote", id.quote_type);
                append_if(u, "maturity", id.maturity);
                if (id.vol)
                    append_if(u, "expiry", id.vol->expiry);
                if (id.vol)
                    append_if(u, "strike", id.vol->strike);
                if (id.vol)
                    append_if(u, "delta", id.vol->delta_type);
                if (id.vol)
                    append_if(u, "smile", id.vol->smile);
                // The model is written whether or not it holds the default, and
                // only where it means something: a reader must not have to know
                // that an absent model on a surface means lognormal.
                if (id.type == instrument_type::vol)
                    u.params().append({"model",
                                       std::string(magic_enum::enum_name(
                                           id.vol ? id.vol->model_subtype :
                                                    volatility_model_subtype::rate_lnvol))});
            } else if constexpr (std::is_same_v<T, equity_market_data_identifier>) {
                u.set_host("equity");
                u.segments().push_back(to_lower(id.ticker));
                if (id.ccy)
                    u.params().append({"ccy", to_lower(*id.ccy)});
                u.params().append({"type", std::string(magic_enum::enum_name(id.type))});
                append_enum_if(u, "quote", id.quote_type);
                append_if(u, "maturity", id.maturity);
                if (id.vol)
                    append_if(u, "expiry", id.vol->expiry);
                if (id.vol)
                    append_if(u, "delta", id.vol->delta_type);
                if (id.vol)
                    append_if(u, "premium", id.vol->premium_type);
                if (id.vol)
                    append_if(u, "call_put", id.vol->call_put);
                if (id.vol)
                    append_if(u, "strike", id.vol->strike);
                // The model is written whether or not it holds the default, and
                // only where it means something: a reader must not have to know
                // that an absent model on a surface means lognormal.
                if (id.type == instrument_type::vol)
                    u.params().append({"model",
                                       std::string(magic_enum::enum_name(
                                           id.vol ? id.vol->model_subtype :
                                                    volatility_model_subtype::rate_lnvol))});
            } else if constexpr (std::is_same_v<T, credit_market_data_identifier>) {
                u.set_host("credit");
                u.segments().push_back(to_lower(id.reference_entity));
                // Mandatory for the class, but a quote type the model marks
                // no_ccy carries none, and an empty value is not a value.
                if (!id.ccy.empty())
                    u.params().append({"ccy", to_lower(id.ccy)});
                u.params().append({"type", std::string(magic_enum::enum_name(id.type))});
                append_enum_if(u, "quote", id.quote_type);
                append_if(u, "seniority", id.seniority);
                append_if(u, "restructuring", id.restructuring);
                append_if(u, "tenor", id.tenor);
                if (id.vol)
                    append_if(u, "expiry", id.vol->expiry);
                if (id.vol)
                    append_if(u, "strike", id.vol->strike);
                if (id.vol)
                    append_if(u, "delta", id.vol->delta_type);
                // The model is written whether or not it holds the default, and
                // only where it means something: a reader must not have to know
                // that an absent model on a surface means lognormal.
                if (id.type == instrument_type::vol)
                    u.params().append({"model",
                                       std::string(magic_enum::enum_name(
                                           id.vol ? id.vol->model_subtype :
                                                    volatility_model_subtype::rate_lnvol))});
            } else if constexpr (std::is_same_v<T, commodity_market_data_identifier>) {
                u.set_host("commodity");
                u.segments().push_back(to_lower(id.commodity_code));
                if (id.ccy)
                    u.params().append({"ccy", to_lower(*id.ccy)});
                u.params().append({"type", std::string(magic_enum::enum_name(id.type))});
                append_enum_if(u, "quote", id.quote_type);
                append_if(u, "delivery", id.delivery);
                append_if(u, "maturity", id.maturity);
                if (id.vol)
                    append_if(u, "expiry", id.vol->expiry);
                if (id.vol)
                    append_if(u, "delta", id.vol->delta_type);
                if (id.vol)
                    append_if(u, "premium", id.vol->premium_type);
                if (id.vol)
                    append_if(u, "call_put", id.vol->call_put);
                if (id.vol)
                    append_if(u, "strike", id.vol->strike);
                // The model is written whether or not it holds the default, and
                // only where it means something: a reader must not have to know
                // that an absent model on a surface means lognormal.
                if (id.type == instrument_type::vol)
                    u.params().append({"model",
                                       std::string(magic_enum::enum_name(
                                           id.vol ? id.vol->model_subtype :
                                                    volatility_model_subtype::rate_lnvol))});
            } else if constexpr (std::is_same_v<T, inflation_market_data_identifier>) {
                u.set_host("inflation");
                u.segments().push_back(to_lower(id.index_code));
                u.params().append({"type", std::string(magic_enum::enum_name(id.type))});
                append_enum_if(u, "quote", id.quote_type);
                append_if(u, "maturity", id.maturity);
                append_if(u, "month", id.month);
                if (id.vol)
                    append_if(u, "expiry", id.vol->expiry);
                if (id.vol)
                    append_if(u, "call_put", id.vol->call_put);
                if (id.vol)
                    append_if(u, "strike", id.vol->strike);
                // The model is written whether or not it holds the default, and
                // only where it means something: a reader must not have to know
                // that an absent model on a surface means lognormal.
                if (id.type == instrument_type::vol)
                    u.params().append({"model",
                                       std::string(magic_enum::enum_name(
                                           id.vol ? id.vol->model_subtype :
                                                    volatility_model_subtype::rate_lnvol))});
            } else if constexpr (std::is_same_v<T, correlation_market_data_identifier>) {
                u.set_host("correlation");
                u.segments().push_back(to_lower(id.factor_pair));
                u.params().append({"type", std::string(magic_enum::enum_name(id.type))});
                append_enum_if(u, "quote", id.quote_type);
                append_if(u, "second_factor", id.second_factor);
                append_if(u, "expiry", id.expiry);
                append_if(u, "delta", id.delta);
            } else if constexpr (std::is_same_v<T, security_market_data_identifier>) {
                u.set_host("security");
                u.segments().push_back(to_lower(id.security_id));
                u.params().append({"type", std::string(magic_enum::enum_name(id.type))});
                append_enum_if(u, "quote", id.quote_type);
                append_if(u, "delivery", id.delivery);
            } else if constexpr (std::is_same_v<T, shape_profile_market_data_identifier>) {
                u.set_host("shape_profile");
                u.segments().push_back(to_lower(id.profile_id));
                u.params().append({"type", std::string(magic_enum::enum_name(id.type))});
                append_enum_if(u, "quote", id.quote_type);
                append_if(u, "date", id.date);
                append_if(u, "second", id.second);
                append_if(u, "period", id.period);
                append_if(u, "dst", id.dst);
            } else if constexpr (std::is_same_v<T, rating_market_data_identifier>) {
                u.set_host("rating");
                u.segments().push_back(to_lower(id.provider_id));
                u.params().append({"type", std::string(magic_enum::enum_name(id.type))});
                append_enum_if(u, "quote", id.quote_type);
                append_if(u, "from", id.from);
                append_if(u, "to", id.to);
            } else if constexpr (std::is_same_v<T, power_market_data_identifier>) {
                u.set_host("power");
                u.segments().push_back(to_lower(id.commodity_code));
                u.params().append({"type", std::string(magic_enum::enum_name(id.type))});
                append_if(u, "delivery", id.delivery);
            } else if constexpr (std::is_same_v<T, generic_market_data_identifier>) {
                u.set_host("generic");
                u.segments().push_back(to_lower(id.name));
                u.params().append({"type", std::string(magic_enum::enum_name(id.type))});
                append_if(u, "name_spelling", id.name_spelling);
            }
        },
        identifier);

    return domain::oresmd_uri{u.buffer()};
}

domain::oresmd_uri oresmd_parser::to_uri(const domain::market_data_identifier& identifier,
                                         const canonical_values& canonical) {
    validate_canonical(identifier, canonical);
    return to_uri(identifier);
}

domain::oresmd_uri oresmd_parser::to_series_uri(const domain::market_data_identifier& identifier) {
    // The point is the observation's coordinate and the surface is its vol point, so
    // a series is the identifier with both dropped. The visitor asks each variant
    // whether it has the fields at all: the security, power and generic classes carry
    // neither, and a fixing's are unset, so those identifiers come back unchanged.
    auto series = identifier;
    std::visit(
        [](auto& id) {
            // The declared coordinates go, one member each. The surface stays,
            // because its model is identity: a lognormal and a normal surface of
            // one underlying are two series rather than one, which is what the
            // grammar's own declaration says and no hand-written special case
            // can be trusted to remember.
            if constexpr (requires { id.maturity; })
                id.maturity.reset();
            if constexpr (requires { id.seniority; })
                id.seniority.reset();
            if constexpr (requires { id.restructuring; })
                id.restructuring.reset();
            if constexpr (requires { id.month; })
                id.month.reset();
            if constexpr (requires { id.from; })
                id.from.reset();
            if constexpr (requires { id.to; })
                id.to.reset();
            if constexpr (requires { id.date; })
                id.date.reset();
            if constexpr (requires { id.second; })
                id.second.reset();
            if constexpr (requires { id.period; })
                id.period.reset();
            if constexpr (requires { id.dst; })
                id.dst.reset();
            // Correlation keeps its surface coordinates at the top level, because
            // its identifier has no vol member for them to live in.
            if constexpr (requires { id.expiry; })
                id.expiry.reset();
            if constexpr (requires { id.delta; })
                id.delta.reset();
            if constexpr (requires { id.vol; }) {
                if (id.vol) {
                    id.vol->expiry.clear();
                    id.vol->strike.clear();
                    id.vol->delta_type.reset();
                    id.vol->call_put.reset();
                    id.vol->premium_type.reset();
                    id.vol->smile.reset();
                }
            }
            // Three IR families keep their coordinate in fields the surface drop
            // above does not reach, and the decomposition's own split says which
            // fields: a discount curve's series is the currency and the curve, so
            // its maturity is the tenor; a money-market future's series is the
            // currency and the contract month, so its coordinate is the contract
            // code and the underlying tenor; and an overnight-index future's
            // series is the currency alone, so the contract month is part of its
            // coordinate too. A key that is a coordinate for one family and
            // identity for another is decided by the family, not by the name.
            if constexpr (std::is_same_v<std::decay_t<decltype(id)>, ir_market_data_identifier>) {
                if (id.quote_type == ir_quote_type::discount) {
                    id.tenor.reset();
                } else if (id.quote_type == ir_quote_type::mm_future) {
                    id.contract_code.reset();
                    id.tenor.reset();
                } else if (id.quote_type == ir_quote_type::oi_future) {
                    id.contract_month.reset();
                    id.contract_code.reset();
                    id.tenor.reset();
                }
            }
            if constexpr (std::is_same_v<std::decay_t<decltype(id)>, credit_market_data_identifier>)
                id.tenor.reset();
        },
        series);
    return to_uri(series);
}

std::optional<domain::market_data_identifier>
oresmd_parser::with_point(const domain::market_data_identifier& identifier,
                          const std::string& point) {
    // The inverse of to_series_uri()'s per-family drop, and the same fact read back:
    // the families whose coordinate the drop reaches are the ones that take a point,
    // and the three IR families that keep it elsewhere split it into those fields.
    // A surface is not a point -- to_series_uri() drops the vol point rather than a
    // field this could refill -- so a vol identifier is refused, as is a fixing's
    // key, which is an index name and carries no coordinate of its own.
    auto datum = identifier;
    const auto placed = std::visit(
        [&point](auto& id) -> bool {
            using T = std::decay_t<decltype(id)>;
            // Only a quote's key is a series and a point: a fixing's key is an index
            // name, a curve's is the curve itself, and a vol's is its surface point.
            if (id.type != instrument_type::quote)
                return false;
            // A spot quote's key is its series key, so a point the decomposition
            // hands it is accepted and dropped: the reader writes no coordinate
            // for a family that has none.
            if constexpr (std::is_same_v<T, fx_market_data_identifier>) {
                if (id.quote_type.value_or(fx_quote_type::spot) == fx_quote_type::spot)
                    return true;
            }
            if constexpr (std::is_same_v<T, equity_market_data_identifier>) {
                if (id.quote_type.value_or(equity_quote_type::spot) == equity_quote_type::spot)
                    return true;
            }
            if constexpr (std::is_same_v<T, commodity_market_data_identifier>) {
                if (id.quote_type.value_or(commodity_quote_type::spot) ==
                    commodity_quote_type::spot)
                    return true;
            }
            if constexpr (std::is_same_v<T, ir_market_data_identifier>) {
                // The coordinate-bearing families, named one by one: a quote type
                // this does not know keeps its coordinate somewhere else, and a
                // silently wrong key is worse than no key, so the default refuses.
                switch (id.quote_type.value_or(ir_quote_type::ir_swap)) {
                    case ir_quote_type::discount:
                        id.tenor = point;
                        return true;
                    case ir_quote_type::mm_future:
                    case ir_quote_type::oi_future: {
                        // A future's series has already dropped the contract fields,
                        // so the point carries them: contract_code/tenor for a money
                        // market future and contract_month/contract_code/tenor for an
                        // overnight-index one.
                        const auto parts = split_on_slash(point);
                        const auto is_overnight = id.quote_type == ir_quote_type::oi_future;
                        if (parts.size() != (is_overnight ? 3u : 2u))
                            return false;
                        if (is_overnight) {
                            id.contract_month = parts[0];
                            id.contract_code = parts[1];
                            id.tenor = parts[2];
                            return true;
                        }
                        id.contract_code = parts[0];
                        id.tenor = parts[1];
                        return true;
                    }
                    case ir_quote_type::mm:
                    case ir_quote_type::fra:
                    case ir_quote_type::imm_fra:
                    case ir_quote_type::ir_swap:
                    case ir_quote_type::basis_swap:
                    case ir_quote_type::bma_swap:
                    case ir_quote_type::cc_basis_swap:
                    case ir_quote_type::cc_fix_float_swap:
                    case ir_quote_type::zero:
                        id.maturity = point;
                        return true;
                    default:
                        // A capfloor, a swaption and a bond option keep their
                        // coordinate on a surface, which is not a point on a
                        // series; a silently wrong key is worse than no key.
                        return false;
                }
            }
            if constexpr (std::is_same_v<T, fx_market_data_identifier>) {
                if (id.quote_type.value_or(fx_quote_type::spot) == fx_quote_type::fwd) {
                    id.maturity = point;
                    return true;
                }
                return false;
            }
            if constexpr (std::is_same_v<T, equity_market_data_identifier>) {
                const auto qt = id.quote_type.value_or(equity_quote_type::spot);
                if (qt == equity_quote_type::dividend || qt == equity_quote_type::fwd) {
                    id.maturity = point;
                    return true;
                }
                return false;
            }
            if constexpr (std::is_same_v<T, commodity_market_data_identifier>) {
                if (id.quote_type.value_or(commodity_quote_type::spot) ==
                    commodity_quote_type::fwd) {
                    id.maturity = point;
                    return true;
                }
                return false;
            }
            if constexpr (std::is_same_v<T, inflation_market_data_identifier>) {
                const auto qt = id.quote_type.value_or(inflation_quote_type::zc_swap);
                if (qt == inflation_quote_type::seasonality) {
                    id.month = point;
                    return true;
                }
                if (qt == inflation_quote_type::zc_swap || qt == inflation_quote_type::yy_swap) {
                    id.maturity = point;
                    return true;
                }
                return false;
            }
            if constexpr (std::is_same_v<T, credit_market_data_identifier>) {
                const auto parts = split_on_slash(point);
                switch (id.quote_type.value_or(credit_quote_type::cds)) {
                    case credit_quote_type::cds_index:
                    case credit_quote_type::index_cds_tranche:
                        if (parts.size() != 2)
                            return false;
                        id.tenor = parts[0];
                        if (!id.vol)
                            id.vol.emplace();
                        id.vol->strike = parts[1];
                        return true;
                    case credit_quote_type::cds:
                    case credit_quote_type::hazard_rate:
                        // seniority/tenor, or seniority/restructuring/tenor.
                        if (parts.size() == 2) {
                            id.seniority = parts[0];
                            id.tenor = parts[1];
                            return true;
                        }
                        if (parts.size() == 3) {
                            id.seniority = parts[0];
                            id.restructuring = parts[1];
                            id.tenor = parts[2];
                            return true;
                        }
                        return false;
                    case credit_quote_type::recovery_rate:
                        if (parts.empty())
                            return false;
                        id.seniority = parts[0];
                        if (parts.size() == 2)
                            id.restructuring = parts[1];
                        return parts.size() <= 2;
                    case credit_quote_type::index_cds_option:
                        return false;
                }
            }
            if constexpr (std::is_same_v<T, correlation_market_data_identifier>) {
                const auto parts = split_on_slash(point);
                if (parts.size() != 2)
                    return false;
                id.expiry = parts[0];
                id.delta = parts[1];
                return true;
            }
            if constexpr (std::is_same_v<T, shape_profile_market_data_identifier>) {
                const auto parts = split_on_slash(point);
                if (parts.size() != 3 && parts.size() != 4)
                    return false;
                id.date = parts[0];
                id.second = parts[1];
                id.period = parts[2];
                if (parts.size() == 4)
                    id.dst = parts[3];
                return true;
            }
            if constexpr (std::is_same_v<T, rating_market_data_identifier>) {
                // The degenerate form names the provider and no grades; the
                // series it belongs to is the one with no grades at all.
                if (point.empty())
                    return true;
                const auto parts = split_on_slash(point);
                if (parts.size() != 2)
                    return false;
                id.from = parts[0];
                id.to = parts[1];
                return true;
            }
            return false;
        },
        datum);
    if (!placed)
        return std::nullopt;
    return datum;
}

}
