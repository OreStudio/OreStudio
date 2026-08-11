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
    std::optional<std::string> tenor;
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
            else if (p.key == "tenor")
                qp.tenor = p.value;
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
    reject_if_present("fx", "role", qp.role);
    reject_if_present("fx", "metric", qp.metric);
}

void validate_ir(const query_params& qp) {
    reject_if_present("ir", "ccy", qp.ccy);
    if (qp.metric && parse_type(qp) != instrument_type::quote)
        BOOST_THROW_EXCEPTION(
            oresmd_exception("oresmd://ir/... 'metric' is only meaningful when type=quote."));
    if (qp.quote && parse_type(qp) != instrument_type::quote)
        BOOST_THROW_EXCEPTION(
            oresmd_exception("oresmd://ir/... 'quote' is only meaningful when type=quote."));
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
        if (id.type != instrument_type::quote)
            BOOST_THROW_EXCEPTION(
                oresmd_exception("oresmd://fx/... 'quote' is only meaningful when type=quote."));
        id.quote_type = parse_enum<fx_quote_type>("quote", *qp.quote);
    }
    if (qp.point)
        id.point = to_lower(*qp.point);
    return id;
}

market_data_identifier parse_ir(const boost::urls::url_view& u, const query_params& qp) {
    validate_ir(qp);
    ir_market_data_identifier id;
    id.ccy = to_upper(first_segment(u));
    id.type = parse_type(qp);
    if (qp.index)
        id.index = parse_enum<index_family>("index", *qp.index);
    if (qp.tenor)
        id.tenor = to_lower(*qp.tenor);
    if (qp.role)
        id.role = parse_enum<curve_role>("role", *qp.role);
    if (qp.metric)
        id.metric = parse_enum<metric>("metric", *qp.metric);
    if (qp.quote)
        id.quote_type = parse_enum<ir_quote_type>("quote", *qp.quote);
    if (qp.point) {
        id.point = to_lower(*qp.point);
        if (id.type == instrument_type::vol) {
            std::vector<std::string> parts;
            std::stringstream ss(*id.point);
            std::string part;
            while (std::getline(ss, part, ','))
                parts.push_back(to_upper(part));
            if (parts.size() == 3) {
                volatility_surface_point v;
                v.expiry = parts[0];
                if (!id.tenor)
                    id.tenor = to_lower(parts[1]);
                v.strike = parts[2];
                id.vol = std::move(v);
            }
        }
    }
    if (qp.model && id.type == instrument_type::vol && id.vol)
        id.vol->model_subtype = parse_enum<volatility_model_subtype>("model", *qp.model);
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
        if (id.type != instrument_type::quote)
            BOOST_THROW_EXCEPTION(oresmd_exception(
                "oresmd://equity/... 'quote' is only meaningful when type=quote."));
        id.quote_type = parse_enum<equity_quote_type>("quote", *qp.quote);
    }
    if (qp.point)
        id.point = to_lower(*qp.point);
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
    return id;
}

market_data_identifier parse_correlation(const boost::urls::url_view& u, const query_params& qp) {
    reject_if_present("correlation", "ccy", qp.ccy);
    reject_if_present("correlation", "index", qp.index);
    reject_if_present("correlation", "tenor", qp.tenor);
    reject_if_present("correlation", "role", qp.role);
    reject_if_present("correlation", "metric", qp.metric);
    reject_if_present("correlation", "point", qp.point);
    correlation_market_data_identifier id;
    id.factor_pair = to_upper(first_segment(u));
    id.type = parse_type(qp);
    if (qp.quote) {
        if (id.type != instrument_type::quote)
            BOOST_THROW_EXCEPTION(oresmd_exception(
                "oresmd://correlation/... 'quote' is only meaningful when type=quote."));
        id.quote_type = parse_enum<correlation_quote_type>("quote", *qp.quote);
    }
    return id;
}

market_data_identifier parse_inflation(const boost::urls::url_view& u, const query_params& qp) {
    // Validation: only quote, type, and point are meaningful for inflation.
    reject_if_present("inflation", "ccy", qp.ccy);
    reject_if_present("inflation", "index", qp.index);
    reject_if_present("inflation", "tenor", qp.tenor);
    reject_if_present("inflation", "role", qp.role);
    reject_if_present("inflation", "metric", qp.metric);
    inflation_market_data_identifier id;
    id.index_code = to_upper(first_segment(u));
    id.type = parse_type(qp);
    if (qp.quote) {
        if (id.type != instrument_type::quote)
            BOOST_THROW_EXCEPTION(oresmd_exception(
                "oresmd://inflation/... 'quote' is only meaningful when type=quote."));
        id.quote_type = parse_enum<inflation_quote_type>("quote", *qp.quote);
    }
    if (qp.point)
        id.point = to_lower(*qp.point);
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
        if (id.type != instrument_type::quote)
            BOOST_THROW_EXCEPTION(oresmd_exception(
                "oresmd://commodity/... 'quote' is only meaningful when type=quote."));
        id.quote_type = parse_enum<commodity_quote_type>("quote", *qp.quote);
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
            } else if constexpr (std::is_same_v<T, ir_market_data_identifier>) {
                u.set_host("ir");
                u.segments().push_back(to_lower(id.ccy));
                append_enum_if(u, "index", id.index);
                append_if(u, "tenor", id.tenor);
                append_enum_if(u, "role", id.role);
                u.params().append({"type", std::string(magic_enum::enum_name(id.type))});
                append_enum_if(u, "metric", id.metric);
                append_enum_if(u, "quote", id.quote_type);
                append_if(u, "point", id.point);
            } else if constexpr (std::is_same_v<T, equity_market_data_identifier>) {
                u.set_host("equity");
                u.segments().push_back(to_lower(id.ticker));
                u.params().append({"ccy", to_lower(id.ccy)});
                u.params().append({"type", std::string(magic_enum::enum_name(id.type))});
                append_enum_if(u, "quote", id.quote_type);
                append_if(u, "point", id.point);
            } else if constexpr (std::is_same_v<T, credit_market_data_identifier>) {
                u.set_host("credit");
                u.segments().push_back(to_lower(id.reference_entity));
                u.params().append({"ccy", to_lower(id.ccy)});
                u.params().append({"type", std::string(magic_enum::enum_name(id.type))});
                append_enum_if(u, "quote", id.quote_type);
                append_if(u, "point", id.point);
            } else if constexpr (std::is_same_v<T, commodity_market_data_identifier>) {
                u.set_host("commodity");
                u.segments().push_back(to_lower(id.commodity_code));
                u.params().append({"ccy", to_lower(id.ccy)});
                u.params().append({"type", std::string(magic_enum::enum_name(id.type))});
                append_enum_if(u, "quote", id.quote_type);
                append_if(u, "point", id.point);
            } else if constexpr (std::is_same_v<T, inflation_market_data_identifier>) {
                u.set_host("inflation");
                u.segments().push_back(to_lower(id.index_code));
                u.params().append({"type", std::string(magic_enum::enum_name(id.type))});
                append_enum_if(u, "quote", id.quote_type);
                append_if(u, "point", id.point);
            } else if constexpr (std::is_same_v<T, correlation_market_data_identifier>) {
                u.set_host("correlation");
                u.segments().push_back(to_lower(id.factor_pair));
                u.params().append({"type", std::string(magic_enum::enum_name(id.type))});
                append_enum_if(u, "quote", id.quote_type);
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
