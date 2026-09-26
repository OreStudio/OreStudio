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
#include "ores.marketdata.core/oresmd/detail/oresmd_index_family_utils.hpp"
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
#include <vector>

namespace {

using namespace ores::marketdata::domain;
using ores::marketdata::core::oresmd_exception;
using ores::marketdata::core::detail::is_overnight;
using ores::marketdata::core::detail::to_lower;
using ores::marketdata::core::detail::to_upper;

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
    std::optional<std::string> point;

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
                qp.point = p.value;
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
}

void validate_ir(const query_params& qp) {
    reject_if_present("ir", "ccy", qp.ccy);
    if (qp.metric && parse_type(qp) != instrument_type::quote)
        BOOST_THROW_EXCEPTION(
            oresmd_exception("oresmd://ir/... 'metric' is only meaningful when type=quote."));
}

void validate_no_ir_only_keys(std::string_view asset_class, const query_params& qp) {
    reject_if_present(asset_class, "index", qp.index);
    reject_if_present(asset_class, "tenor", qp.tenor);
    reject_if_present(asset_class, "role", qp.role);
    reject_if_present(asset_class, "metric", qp.metric);
}

void validate_equity(const query_params& qp) {
    validate_no_ir_only_keys("equity", qp);
}

std::string first_segment(const boost::urls::url_view& u) {
    for (const auto seg : u.segments()) {
        if (!seg.empty())
            return std::string(seg);
    }
    BOOST_THROW_EXCEPTION(oresmd_exception("oresmd URI is missing its entity path segment."));
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
    if (qp.quote) {
        // A quote type names a volatility surface as well as a quote: CAPFLOOR
        // arrives as type=vol with quote=capfloor. Anything else that carries a
        // quote key is a genuine input error.
        if (id.type != instrument_type::quote && id.type != instrument_type::vol)
            BOOST_THROW_EXCEPTION(
                oresmd_exception("oresmd://fx/... 'quote' is only meaningful when type=quote."));
        id.quote_type = parse_enum<fx_quote_type>("quote", *qp.quote);
    }
    if (qp.point)
        id.point = to_lower(*qp.point);
    // A vol surface point carries the FX option's expiry and strike, and the
    // pair already carries the two currencies: type=vol&point=10y,atm.
    if (qp.point && id.type == instrument_type::vol) {
        std::vector<std::string> parts;
        std::stringstream ss(*id.point);
        std::string part;
        while (std::getline(ss, part, ','))
            parts.push_back(to_upper(part));
        if (parts.size() != 2)
            BOOST_THROW_EXCEPTION(oresmd_exception(std::format(
                "oresmd://fx/... a vol surface point is expiry,strike, got: '{}'.", *id.point)));
        volatility_surface_point v;
        v.expiry = parts[0];
        v.strike = parts[1];
        id.vol = std::move(v);
    }
    if (qp.model && id.type == instrument_type::vol && id.vol)
        id.vol->model_subtype = parse_enum<volatility_model_subtype>("model", *qp.model);
    return id;
}

market_data_identifier parse_ir(const boost::urls::url_view& u, const query_params& qp) {
    validate_ir(qp);
    ir_market_data_identifier id;
    id.ccy = to_upper(first_segment(u));
    id.type = parse_type(qp);
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
    if (qp.point) {
        id.point = to_lower(*qp.point);
        if (id.type == instrument_type::vol) {
            // Split the point as it was written, not as it is stored: a swaption
            // surface may carry the smile convention's marker, and that marker is
            // a name whose case the key has to read back.
            std::vector<std::string> parts;
            std::stringstream ss(*qp.point);
            std::string part;
            while (std::getline(ss, part, ','))
                parts.push_back(part);
            if (id.quote_type == ir_quote_type::capfloor) {
                // The cap/floor surface carries five coordinates, not three:
                // maturity, float tenor, and the shift and strip flags the
                // market data catalogue names, then the strike.
                if (parts.size() != 5)
                    BOOST_THROW_EXCEPTION(oresmd_exception(
                        std::format("oresmd://ir/... a capfloor vol point is "
                                    "maturity,float_tenor,shift,strip,strike, got: '{}'.",
                                    *id.point)));
                volatility_surface_point v;
                v.expiry = to_upper(parts[0]);
                id.tenor = to_lower(parts[1]);
                id.shift = to_lower(parts[2]);
                id.strip = to_lower(parts[3]);
                v.strike = to_upper(parts[4]);
                id.vol = std::move(v);
            } else if (parts.size() == 3 || parts.size() == 4) {
                volatility_surface_point v;
                v.expiry = to_upper(parts[0]);
                if (!id.tenor)
                    id.tenor = to_lower(parts[1]);
                if (parts.size() == 3) {
                    v.strike = to_upper(parts[2]);
                } else {
                    // expiry,tenor,marker,value -- the smile convention. The
                    // marker is carried and re-serialised as it arrived.
                    v.delta_type = parts[2];
                    v.strike = to_upper(parts[3]);
                    id.point = to_lower(parts[0]) + "," + to_lower(parts[1]) + "," + *v.delta_type +
                               "," + to_lower(parts[3]);
                }
                id.vol = std::move(v);
            }
        }
    }
    if (qp.model && id.type == instrument_type::vol) {
        // A shift quote names its surface with no coordinate of its own:
        // CAPFLOOR/SHIFT/CCY/TENOR arrives as
        // type=vol&quote=capfloor&model=shift&tenor=6m. The surface is built
        // from the model alone, so it is created here when the point block
        // above did not.
        if (!id.vol)
            id.vol.emplace();
        id.vol->model_subtype = parse_enum<volatility_model_subtype>("model", *qp.model);
    }
    if (id.type == instrument_type::fixing && id.index && !is_overnight(*id.index) && !id.tenor)
        BOOST_THROW_EXCEPTION(oresmd_exception(
            std::format("oresmd://ir/... a term index ('{}') fixing requires a tenor.",
                        magic_enum::enum_name(*id.index))));
    return id;
}

market_data_identifier parse_equity(const boost::urls::url_view& u, const query_params& qp) {
    validate_equity(qp);
    equity_market_data_identifier id;
    id.ticker = to_upper(first_segment(u));
    if (!qp.ccy)
        BOOST_THROW_EXCEPTION(oresmd_exception("oresmd://equity/... requires a ccy query key."));
    id.ccy = to_upper(*qp.ccy);
    id.type = parse_type(qp);
    if (qp.quote) {
        // A quote type names a volatility surface as well as a quote: CAPFLOOR
        // arrives as type=vol with quote=capfloor. Anything else that carries a
        // quote key is a genuine input error.
        if (id.type != instrument_type::quote && id.type != instrument_type::vol)
            BOOST_THROW_EXCEPTION(oresmd_exception(
                "oresmd://equity/... 'quote' is only meaningful when type=quote."));
        id.quote_type = parse_enum<equity_quote_type>("quote", *qp.quote);
    }
    if (qp.point)
        id.point = to_lower(*qp.point);
    // An equity option's surface point carries up to five coordinates, and the
    // count says which of the three shapes it is: expiry,strike;
    // expiry,strike,call_put; or expiry,delta,premium,call_put,strike.
    if (qp.point && id.type == instrument_type::vol) {
        std::vector<std::string> parts;
        std::stringstream ss(*id.point);
        std::string part;
        while (std::getline(ss, part, ','))
            parts.push_back(to_upper(part));
        if (parts.size() != 2 && parts.size() != 3 && parts.size() != 5)
            BOOST_THROW_EXCEPTION(oresmd_exception(std::format(
                "oresmd://equity/... a vol surface point is expiry,strike; "
                "expiry,strike,call_put; or expiry,delta,premium,call_put,strike; got: '{}'.",
                *id.point)));
        volatility_surface_point v;
        if (parts.size() == 5) {
            v.expiry = parts[0];
            v.delta_type = parts[1];
            v.premium_type = parts[2];
            v.call_put = parts[3];
            v.strike = parts[4];
        } else {
            v.expiry = parts[0];
            v.strike = parts[1];
            if (parts.size() == 3)
                v.call_put = parts[2];
        }
        id.vol = std::move(v);
    }
    if (qp.model && id.type == instrument_type::vol && id.vol)
        id.vol->model_subtype = parse_enum<volatility_model_subtype>("model", *qp.model);
    return id;
}

market_data_identifier parse_credit(const boost::urls::url_view& u, const query_params& qp) {
    validate_no_ir_only_keys("credit", qp);
    credit_market_data_identifier id;
    id.reference_entity = to_upper(first_segment(u));
    if (!qp.ccy)
        BOOST_THROW_EXCEPTION(oresmd_exception("oresmd://credit/... requires a ccy query key."));
    id.ccy = to_upper(*qp.ccy);
    id.type = parse_type(qp);
    if (qp.quote)
        id.quote_type = parse_enum<credit_quote_type>("quote", *qp.quote);
    if (qp.point)
        id.point = to_lower(*qp.point);
    // An index CDS option's surface point carries the index tenor ahead of the
    // expiry and the strike: type=vol&point=5y,2025-02-19,107.5. The corpus also
    // writes a term vol with the tenor alone and neither of the other two, so the
    // count decides which form this is.
    if (qp.point && id.type == instrument_type::vol) {
        std::vector<std::string> parts;
        std::stringstream ss(*id.point);
        std::string part;
        while (std::getline(ss, part, ','))
            parts.push_back(part);
        if (parts.size() != 1 && parts.size() != 3)
            BOOST_THROW_EXCEPTION(
                oresmd_exception(std::format("oresmd://credit/... a vol surface point is tenor or "
                                             "tenor,expiry,strike, got: '{}'.",
                                             *id.point)));
        volatility_surface_point v;
        if (parts.size() == 3) {
            v.expiry = to_upper(parts[1]);
            v.strike = to_upper(parts[2]);
            id.point = to_lower(parts[0]) + "," + to_lower(parts[1]) + "," + to_lower(parts[2]);
        } else {
            id.point = to_lower(parts[0]);
        }
        id.vol = std::move(v);
    }
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
    correlation_market_data_identifier id;
    id.factor_pair = to_upper(first_segment(u));
    id.type = parse_type(qp);
    if (qp.quote) {
        // A quote type names a volatility surface as well as a quote: CAPFLOOR
        // arrives as type=vol with quote=capfloor. Anything else that carries a
        // quote key is a genuine input error.
        if (id.type != instrument_type::quote && id.type != instrument_type::vol)
            BOOST_THROW_EXCEPTION(oresmd_exception(
                "oresmd://correlation/... 'quote' is only meaningful when type=quote."));
        id.quote_type = parse_enum<correlation_quote_type>("quote", *qp.quote);
    }
    if (qp.point)
        id.point = to_lower(*qp.point);
    // A pairwise correlation names a second operand as well as the entity, and
    // the entity carries the first: the two take fixed positions in the key.
    if (qp.second_factor)
        id.second_factor = to_upper(*qp.second_factor);
    return id;
}

market_data_identifier parse_inflation(const boost::urls::url_view& u, const query_params& qp) {
    // Validation: only quote, type, and point are meaningful for inflation.
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
    inflation_market_data_identifier id;
    id.index_code = to_upper(first_segment(u));
    id.type = parse_type(qp);
    if (qp.quote) {
        // A quote type names a volatility surface as well as a quote: CAPFLOOR
        // arrives as type=vol with quote=capfloor. Anything else that carries a
        // quote key is a genuine input error.
        if (id.type != instrument_type::quote && id.type != instrument_type::vol)
            BOOST_THROW_EXCEPTION(oresmd_exception(
                "oresmd://inflation/... 'quote' is only meaningful when type=quote."));
        id.quote_type = parse_enum<inflation_quote_type>("quote", *qp.quote);
    }
    if (qp.point)
        id.point = to_lower(*qp.point);
    // An inflation cap/floor's surface point is the maturity, the cap-or-floor
    // flag and the strike. Which surface it is -- a price or a normal vol --
    // arrives as the model, matching the metric segment the projection emits.
    if (qp.point && id.type == instrument_type::vol) {
        std::vector<std::string> parts;
        std::stringstream ss(*id.point);
        std::string part;
        while (std::getline(ss, part, ','))
            parts.push_back(to_upper(part));
        if (parts.size() != 3)
            BOOST_THROW_EXCEPTION(
                oresmd_exception(std::format("oresmd://inflation/... a capfloor vol point is "
                                             "maturity,cap_or_floor,strike, got: '{}'.",
                                             *id.point)));
        volatility_surface_point v;
        v.expiry = parts[0];
        v.call_put = parts[1];
        v.strike = parts[2];
        id.vol = std::move(v);
    }
    if (qp.model && id.type == instrument_type::vol && id.vol)
        id.vol->model_subtype = parse_enum<volatility_model_subtype>("model", *qp.model);
    return id;
}

market_data_identifier parse_commodity(const boost::urls::url_view& u, const query_params& qp) {
    validate_no_ir_only_keys("commodity", qp);
    commodity_market_data_identifier id;
    id.commodity_code = to_upper(first_segment(u));
    if (!qp.ccy)
        BOOST_THROW_EXCEPTION(oresmd_exception("oresmd://commodity/... requires a ccy query key."));
    id.ccy = to_upper(*qp.ccy);
    id.type = parse_type(qp);
    if (qp.quote) {
        // A quote type names a volatility surface as well as a quote: CAPFLOOR
        // arrives as type=vol with quote=capfloor. Anything else that carries a
        // quote key is a genuine input error.
        if (id.type != instrument_type::quote && id.type != instrument_type::vol)
            BOOST_THROW_EXCEPTION(oresmd_exception(
                "oresmd://commodity/... 'quote' is only meaningful when type=quote."));
        id.quote_type = parse_enum<commodity_quote_type>("quote", *qp.quote);
    }
    if (qp.point)
        id.point = to_lower(*qp.point);
    // A commodity option's surface point carries the same coordinates the equity
    // option's does, and the count says which of the three shapes it is:
    // expiry,strike; expiry,strike,call_put; or expiry,delta,premium,call_put,strike.
    if (qp.point && id.type == instrument_type::vol) {
        std::vector<std::string> parts;
        std::stringstream ss(*id.point);
        std::string part;
        while (std::getline(ss, part, ','))
            parts.push_back(to_upper(part));
        if (parts.size() != 2 && parts.size() != 3 && parts.size() != 5)
            BOOST_THROW_EXCEPTION(oresmd_exception(std::format(
                "oresmd://commodity/... a vol surface point is expiry,strike; "
                "expiry,strike,call_put; or expiry,delta,premium,call_put,strike; got: '{}'.",
                *id.point)));
        volatility_surface_point v;
        if (parts.size() == 5) {
            v.expiry = parts[0];
            v.delta_type = parts[1];
            v.premium_type = parts[2];
            v.call_put = parts[3];
            v.strike = parts[4];
        } else {
            v.expiry = parts[0];
            v.strike = parts[1];
            if (parts.size() == 3)
                v.call_put = parts[2];
        }
        id.vol = std::move(v);
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
    security_market_data_identifier id;
    id.security_id = to_upper(first_segment(u));
    id.type = parse_type(qp);
    if (qp.quote) {
        // A quote type names a volatility surface as well as a quote: CAPFLOOR
        // arrives as type=vol with quote=capfloor. Anything else that carries a
        // quote key is a genuine input error.
        if (id.type != instrument_type::quote && id.type != instrument_type::vol)
            BOOST_THROW_EXCEPTION(oresmd_exception(
                "oresmd://security/... 'quote' is only meaningful when type=quote."));
        id.quote_type = parse_enum<security_quote_type>("quote", *qp.quote);
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
    shape_profile_market_data_identifier id;
    id.profile_id = to_upper(first_segment(u));
    id.type = parse_type(qp);
    if (qp.quote) {
        // A quote type names a volatility surface as well as a quote: CAPFLOOR
        // arrives as type=vol with quote=capfloor. Anything else that carries a
        // quote key is a genuine input error.
        if (id.type != instrument_type::quote && id.type != instrument_type::vol)
            BOOST_THROW_EXCEPTION(oresmd_exception(
                "oresmd://shape_profile/... 'quote' is only meaningful when type=quote."));
        id.quote_type = parse_enum<shape_profile_quote_type>("quote", *qp.quote);
    }
    if (qp.point)
        id.point = to_lower(*qp.point);
    return id;
}

void append_if(boost::urls::url& u, std::string_view key, const std::optional<std::string>& v) {
    if (v)
        u.params().append({key, *v});
}

template <typename Enum>
void append_enum_if(boost::urls::url& u, std::string_view key, const std::optional<Enum>& v) {
    if (v)
        u.params().append({key, std::string(magic_enum::enum_name(*v))});
}

}

namespace ores::marketdata::core {

namespace {

// Which identifier fields the canonical container governs, per asset class: the
// ir identifier carries tenor and point; fx, equity, credit, commodity, and
// inflation carry point; correlation carries neither.
void validate_canonical(const domain::market_data_identifier& identifier,
                        const canonical_values& canonical) {
    const auto check_tenor = [&canonical](const std::optional<std::string>& v) {
        if (v && !canonical.tenor.contains(*v))
            BOOST_THROW_EXCEPTION(oresmd_exception(std::format(
                "Unknown tenor spelling '{}': not in the supplied canonical values.", *v)));
    };
    const auto check_point = [&canonical](const std::optional<std::string>& v) {
        if (v && !canonical.point.contains(*v))
            BOOST_THROW_EXCEPTION(oresmd_exception(std::format(
                "Unknown point spelling '{}': not in the supplied canonical values.", *v)));
    };

    std::visit(
        [&](const auto& id) {
            using T = std::decay_t<decltype(id)>;
            if constexpr (std::is_same_v<T, ir_market_data_identifier>) {
                check_tenor(id.tenor);
                check_point(id.point);
            } else if constexpr (std::is_same_v<T, fx_market_data_identifier> ||
                                 std::is_same_v<T, equity_market_data_identifier> ||
                                 std::is_same_v<T, credit_market_data_identifier> ||
                                 std::is_same_v<T, commodity_market_data_identifier> ||
                                 std::is_same_v<T, inflation_market_data_identifier>) {
                check_point(id.point);
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
                append_if(u, "point", id.point);
                // The surface's model is the ORE metric of the projected key, and no
                // field carries it, so the uri_order loop above cannot emit it. A
                // non-default model is emitted here or the round trip loses which
                // surface this was -- the price one rather than a log-normal vol.
                if (id.vol && id.vol->model_subtype != volatility_model_subtype::rate_lnvol)
                    u.params().append(
                        {"model", std::string(magic_enum::enum_name(id.vol->model_subtype))});
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
                append_if(u, "point", id.point);
                // The surface's model is the ORE metric of the projected key, and no
                // field carries it, so the uri_order loop above cannot emit it. A
                // non-default model is emitted here or the round trip loses which
                // surface this was -- the price one rather than a log-normal vol.
                if (id.vol && id.vol->model_subtype != volatility_model_subtype::rate_lnvol)
                    u.params().append(
                        {"model", std::string(magic_enum::enum_name(id.vol->model_subtype))});
            } else if constexpr (std::is_same_v<T, equity_market_data_identifier>) {
                u.set_host("equity");
                u.segments().push_back(to_lower(id.ticker));
                u.params().append({"ccy", to_lower(id.ccy)});
                u.params().append({"type", std::string(magic_enum::enum_name(id.type))});
                append_enum_if(u, "quote", id.quote_type);
                append_if(u, "point", id.point);
                // The surface's model is the ORE metric of the projected key, and no
                // field carries it, so the uri_order loop above cannot emit it. A
                // non-default model is emitted here or the round trip loses which
                // surface this was -- the price one rather than a log-normal vol.
                if (id.vol && id.vol->model_subtype != volatility_model_subtype::rate_lnvol)
                    u.params().append(
                        {"model", std::string(magic_enum::enum_name(id.vol->model_subtype))});
            } else if constexpr (std::is_same_v<T, credit_market_data_identifier>) {
                u.set_host("credit");
                u.segments().push_back(to_lower(id.reference_entity));
                u.params().append({"ccy", to_lower(id.ccy)});
                u.params().append({"type", std::string(magic_enum::enum_name(id.type))});
                append_enum_if(u, "quote", id.quote_type);
                append_if(u, "point", id.point);
                // The surface's model is the ORE metric of the projected key, and no
                // field carries it, so the uri_order loop above cannot emit it. A
                // non-default model is emitted here or the round trip loses which
                // surface this was -- the price one rather than a log-normal vol.
                if (id.vol && id.vol->model_subtype != volatility_model_subtype::rate_lnvol)
                    u.params().append(
                        {"model", std::string(magic_enum::enum_name(id.vol->model_subtype))});
            } else if constexpr (std::is_same_v<T, commodity_market_data_identifier>) {
                u.set_host("commodity");
                u.segments().push_back(to_lower(id.commodity_code));
                u.params().append({"ccy", to_lower(id.ccy)});
                u.params().append({"type", std::string(magic_enum::enum_name(id.type))});
                append_enum_if(u, "quote", id.quote_type);
                append_if(u, "point", id.point);
                // The surface's model is the ORE metric of the projected key, and no
                // field carries it, so the uri_order loop above cannot emit it. A
                // non-default model is emitted here or the round trip loses which
                // surface this was -- the price one rather than a log-normal vol.
                if (id.vol && id.vol->model_subtype != volatility_model_subtype::rate_lnvol)
                    u.params().append(
                        {"model", std::string(magic_enum::enum_name(id.vol->model_subtype))});
            } else if constexpr (std::is_same_v<T, inflation_market_data_identifier>) {
                u.set_host("inflation");
                u.segments().push_back(to_lower(id.index_code));
                u.params().append({"type", std::string(magic_enum::enum_name(id.type))});
                append_enum_if(u, "quote", id.quote_type);
                append_if(u, "point", id.point);
                // The surface's model is the ORE metric of the projected key, and no
                // field carries it, so the uri_order loop above cannot emit it. A
                // non-default model is emitted here or the round trip loses which
                // surface this was -- the price one rather than a log-normal vol.
                if (id.vol && id.vol->model_subtype != volatility_model_subtype::rate_lnvol)
                    u.params().append(
                        {"model", std::string(magic_enum::enum_name(id.vol->model_subtype))});
            } else if constexpr (std::is_same_v<T, correlation_market_data_identifier>) {
                u.set_host("correlation");
                u.segments().push_back(to_lower(id.factor_pair));
                u.params().append({"type", std::string(magic_enum::enum_name(id.type))});
                append_enum_if(u, "quote", id.quote_type);
                append_if(u, "second_factor", id.second_factor);
                append_if(u, "point", id.point);
            } else if constexpr (std::is_same_v<T, security_market_data_identifier>) {
                u.set_host("security");
                u.segments().push_back(to_lower(id.security_id));
                u.params().append({"type", std::string(magic_enum::enum_name(id.type))});
                append_enum_if(u, "quote", id.quote_type);
            } else if constexpr (std::is_same_v<T, shape_profile_market_data_identifier>) {
                u.set_host("shape_profile");
                u.segments().push_back(to_lower(id.profile_id));
                u.params().append({"type", std::string(magic_enum::enum_name(id.type))});
                append_enum_if(u, "quote", id.quote_type);
                append_if(u, "point", id.point);
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

}
