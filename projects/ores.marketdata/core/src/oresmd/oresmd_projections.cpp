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
#include "ores.marketdata.core/oresmd/oresmd_projections.hpp"
#include "ores.marketdata.core/oresmd/detail/oresmd_index_family_utils.hpp"
#include "ores.marketdata.core/oresmd/detail/oresmd_string_utils.hpp"
#include <format>
#include <magic_enum/magic_enum.hpp>
#include <sstream>
#include <vector>

namespace {

using namespace ores::marketdata::domain;
using ores::marketdata::core::detail::is_overnight;
using ores::marketdata::core::detail::to_upper;

std::vector<std::string> split_point(const std::string& point) {
    std::vector<std::string> parts;
    std::stringstream ss(point);
    std::string part;
    while (std::getline(ss, part, ','))
        parts.push_back(to_upper(part));
    return parts;
}

std::string curve_id(std::string_view ccy, std::string_view tenor) {
    return std::format("{}{}", ccy, to_upper(tenor));
}

/*
 * Both index name and curve key are produced together, gated on `type=fixing` -- the
 * design doc's own worked-examples table shows a single `type=fixing` URI producing
 * BOTH an index name and a curve key (the underlying curve construct has both facets),
 * and reads "--" for both columns on every `type=quote` row. See
 * id:C3E053CA-0D4B-480B-9119-E11530160EC1, "Worked examples" > "Interest rates".
 */
std::optional<std::string> index_name_ir(const ir_market_data_identifier& id) {
    if (id.type != instrument_type::fixing || !id.index)
        return std::nullopt;
    const auto family = to_upper(std::string(magic_enum::enum_name(*id.index)));
    if (is_overnight(*id.index) || !id.tenor)
        return std::format("{}-{}", id.ccy, family);
    return std::format("{}-{}-{}", id.ccy, family, to_upper(*id.tenor));
}

std::optional<std::string> curve_key_ir(const ir_market_data_identifier& id) {
    if (id.type != instrument_type::fixing || !id.tenor)
        return std::nullopt;
    return std::format("Yield/{}/{}", id.ccy, curve_id(id.ccy, *id.tenor));
}

// Default metric from ir_quote_type when metric is absent (e.g. quote=mm implies metric=rate).
metric default_metric(ir_quote_type qt) {
    switch (qt) {
        case ir_quote_type::ir_swap:
        case ir_quote_type::discount:
        case ir_quote_type::mm:
        case ir_quote_type::fra:
        case ir_quote_type::imm_fra:
        case ir_quote_type::cc_fix_float_swap:
            return metric::rate;
        case ir_quote_type::basis_swap:
        case ir_quote_type::cc_basis_swap:
            return metric::basis_spread;
        case ir_quote_type::bma_swap:
            return metric::ratio;
        case ir_quote_type::zero:
            return metric::rate;
        case ir_quote_type::mm_future:
        case ir_quote_type::oi_future:
            return metric::price;
    }
    return metric::rate;
}

// ORE TYPE string for each ir_quote_type.
std::string_view ore_type(ir_quote_type qt) {
    switch (qt) {
        case ir_quote_type::ir_swap:
            return "IR_SWAP";
        case ir_quote_type::discount:
            return "DISCOUNT";
        case ir_quote_type::mm:
            return "MM";
        case ir_quote_type::fra:
            return "FRA";
        case ir_quote_type::imm_fra:
            return "IMM_FRA";
        case ir_quote_type::basis_swap:
            return "BASIS_SWAP";
        case ir_quote_type::bma_swap:
            return "BMA_SWAP";
        case ir_quote_type::cc_basis_swap:
            return "CC_BASIS_SWAP";
        case ir_quote_type::cc_fix_float_swap:
            return "CC_FIX_FLOAT_SWAP";
        case ir_quote_type::zero:
            return "ZERO";
        case ir_quote_type::mm_future:
            return "MM_FUTURE";
        case ir_quote_type::oi_future:
            return "OI_FUTURE";
    }
    return "IR_SWAP";
}

// ORE METRIC string for each metric.
std::string_view ore_metric(metric m) {
    switch (m) {
        case metric::rate:
            return "RATE";
        case metric::price:
            return "PRICE";
        case metric::basis_spread:
            return "BASIS_SPREAD";
        case metric::ratio:
            return "RATIO";
        case metric::yield_spread:
            return "YIELD_SPREAD";
    }
    return "RATE";
}

// Whether this ir_quote_type includes the index in the qualifier (as opposed to just
// ccy/tenor for xccy/BMA types).
bool qualifier_includes_index(ir_quote_type qt) {
    switch (qt) {
        case ir_quote_type::cc_basis_swap:
        case ir_quote_type::cc_fix_float_swap:
        case ir_quote_type::bma_swap:
            return false;
        default:
            return true;
    }
}

std::optional<std::string> quote_key_ir(const ir_market_data_identifier& id) {
    if (id.type == instrument_type::vol) {
        if (!id.point)
            return std::nullopt;
        const auto parts = split_point(*id.point);
        if (parts.size() != 3)
            return std::nullopt;
        return std::format("SWAPTION/RATE_LNVOL/{}/{}/{}/{}", id.ccy, parts[0], parts[1], parts[2]);
    }
    if (id.type != instrument_type::quote || !id.quote_type || !id.point || !id.tenor)
        return std::nullopt;

    const auto qt = *id.quote_type;
    const auto m = id.metric ? *id.metric : default_metric(qt);
    const auto point = to_upper(*id.point);
    const auto t = to_upper(*id.tenor);

    if (qualifier_includes_index(qt)) {
        if (qt == ir_quote_type::ir_swap)
            return std::format("{}/{}/{}/2D/{}/{}", ore_type(qt), ore_metric(m), id.ccy, t, point);
        if (qt == ir_quote_type::discount)
            return std::format("{}/{}/{}/{}/{}",
                               ore_type(qt),
                               ore_metric(m),
                               id.ccy,
                               curve_id(id.ccy, *id.tenor),
                               point);
        if (!id.index)
            return std::nullopt;
        const auto idx = to_upper(std::string(magic_enum::enum_name(*id.index)));
        return std::format("{}/{}/{}/{}/{}/{}", ore_type(qt), ore_metric(m), id.ccy, idx, t, point);
    }
    // No-index types: CC_BASIS_SWAP, CC_FIX_FLOAT_SWAP, BMA_SWAP
    return std::format("{}/{}/{}/{}/{}", ore_type(qt), ore_metric(m), id.ccy, t, point);
}

std::string_view ore_type(fx_quote_type qt) {
    switch (qt) {
    case fx_quote_type::spot: return "FX";
    case fx_quote_type::fwd:  return "FXFWD";
    }
    return "FX";
}

std::string_view ore_fx_metric(fx_quote_type qt) {
    switch (qt) {
    case fx_quote_type::spot:
    case fx_quote_type::fwd: return "RATE";
    }
    return "RATE";
}

std::optional<std::string> quote_key_fx(const fx_market_data_identifier& id) {
    if (id.type != instrument_type::quote || id.pair.size() != 6)
        return std::nullopt;
    const auto qt = id.quote_type.value_or(fx_quote_type::spot);
    // spot: TYPE/METRIC/CCY1/CCY2 (scalar).
    if (qt == fx_quote_type::spot)
        return std::format("{}/{}/{}/{}", ore_type(qt), ore_fx_metric(qt),
                           id.pair.substr(0, 3), id.pair.substr(3, 3));
    // fwd: TYPE/METRIC/CCY1/CCY2/TENOR — forward curve, needs point for tenor.
    if (!id.point)
        return std::nullopt;
    return std::format("{}/{}/{}/{}/{}", ore_type(qt), ore_fx_metric(qt),
                       id.pair.substr(0, 3), id.pair.substr(3, 3), to_upper(*id.point));
}

std::string_view ore_type(equity_quote_type qt) {
    switch (qt) {
        case equity_quote_type::spot:
            return "EQUITY";
        case equity_quote_type::dividend:
            return "EQUITY_DIVIDEND";
        case equity_quote_type::fwd:
            return "EQUITY_FWD";
    }
    return "EQUITY";
}

std::string_view ore_equity_metric(equity_quote_type qt) {
    switch (qt) {
        case equity_quote_type::spot:
        case equity_quote_type::fwd:
            return "PRICE";
        case equity_quote_type::dividend:
            return "RATE";
    }
    return "PRICE";
}

std::optional<std::string> quote_key_equity(const equity_market_data_identifier& id) {
    if (id.type != instrument_type::quote)
        return std::nullopt;
    const auto qt = id.quote_type.value_or(equity_quote_type::spot);
    // spot: EQUITY/PRICE/TICKER/CCY (scalar, no tenor).
    if (qt == equity_quote_type::spot)
        return std::format("{}/{}/{}/{}", ore_type(qt), ore_equity_metric(qt), id.ticker, id.ccy);
    // dividend/fwd: TYPE/METRIC/TICKER/CCY/TENOR — curves, need point for the tenor dimension.
    if (!id.point)
        return std::nullopt;
    return std::format("{}/{}/{}/{}/{}",
                       ore_type(qt),
                       ore_equity_metric(qt),
                       id.ticker,
                       id.ccy,
                       to_upper(*id.point));
}

std::string_view ore_type(credit_quote_type qt) {
    switch (qt) {
        case credit_quote_type::cds:
            return "CDS";
        case credit_quote_type::hazard_rate:
            return "HAZARD_RATE";
        case credit_quote_type::recovery_rate:
            return "RECOVERY_RATE";
        case credit_quote_type::cds_index:
            return "CDS_INDEX";
        case credit_quote_type::index_cds_tranche:
            return "INDEX_CDS_TRANCHE";
    }
    return "CDS";
}

std::string_view ore_credit_metric(credit_quote_type qt) {
    switch (qt) {
        case credit_quote_type::cds:
            return "CREDIT_SPREAD";
        case credit_quote_type::hazard_rate:
        case credit_quote_type::recovery_rate:
            return "RATE";
        case credit_quote_type::cds_index:
        case credit_quote_type::index_cds_tranche:
            return "BASE_CORRELATION";
    }
    return "CREDIT_SPREAD";
}

std::optional<std::string> quote_key_credit(const credit_market_data_identifier& id) {
    if (id.type != instrument_type::quote)
        return std::nullopt;

    const auto qt = id.quote_type.value_or(credit_quote_type::cds);
    const auto qm = ore_credit_metric(qt);

    if (!id.point)
        return std::nullopt;

    const auto parts = split_point(*id.point);

    // RECOVERY_RATE/RATE/ENTITY/SENIORITY/CCY — point is just the seniority.
    if (qt == credit_quote_type::recovery_rate) {
        if (parts.size() != 1)
            return std::nullopt;
        return std::format(
            "{}/{}/{}/{}/{}", ore_type(qt), qm, id.reference_entity, parts[0], id.ccy);
    }

    // CDS_INDEX/BASE_CORRELATION/INDEX/TENOR/DETACHMENT — no ccy dimension.
    // INDEX_CDS_TRANCHE/BASE_CORRELATION/INDEX/SERIES_TENOR/DETACHMENT — no ccy dimension.
    if (qt == credit_quote_type::cds_index || qt == credit_quote_type::index_cds_tranche) {
        if (parts.size() != 2)
            return std::nullopt;
        return std::format(
            "{}/{}/{}/{}/{}", ore_type(qt), qm, id.reference_entity, parts[0], parts[1]);
    }

    // CDS/CREDIT_SPREAD/ENTITY/SENIORITY/CCY/TENOR — point=seniority,tenor (2 parts).
    // HAZARD_RATE/RATE/ENTITY/SENIORITY/CCY/TENOR — same 6-segment shape.
    if (parts.size() != 2)
        return std::nullopt;
    return std::format(
        "{}/{}/{}/{}/{}/{}", ore_type(qt), qm, id.reference_entity, parts[0], id.ccy, parts[1]);
}

std::string_view ore_type(commodity_quote_type qt) {
    switch (qt) {
    case commodity_quote_type::spot: return "COMMODITY";
    case commodity_quote_type::fwd:  return "COMMODITY_FWD";
    // NOTE: Real ORE CPR/RATE quotes are security-level, keyed by ISIN
    // (e.g. CPR/RATE/ISIN:XS0983610930, a scalar inside <Security> blocks in
    // curveconfig.xml), not commodity/ccy/tenor. Modelling CPR under
    // commodity_market_data_identifier is a deliberate simplification since
    // ORE Studio has no security-level identifier yet; the key emitted here
    // (CPR/RATE/CODE/CCY/TENOR) is ORE-Studio-internal shaped, not ORE-native.
    case commodity_quote_type::cpr:  return "CPR";
    }
    return "COMMODITY";
}

std::string_view ore_commodity_metric(commodity_quote_type qt) {
    switch (qt) {
    case commodity_quote_type::spot:
    case commodity_quote_type::fwd: return "PRICE";
    case commodity_quote_type::cpr: return "RATE";
    }
    return "PRICE";
}

std::optional<std::string> quote_key_commodity(const commodity_market_data_identifier& id) {
    if (id.type != instrument_type::quote)
        return std::nullopt;
    const auto qt = id.quote_type.value_or(commodity_quote_type::spot);
    // spot: COMMODITY/PRICE/CODE/CCY (scalar).
    if (qt == commodity_quote_type::spot)
        return std::format("{}/{}/{}/{}", ore_type(qt), ore_commodity_metric(qt),
                           id.commodity_code, id.ccy);
    // fwd/cpr: TYPE/METRIC/CODE/CCY/TENOR — curves, need point for the tenor.
    if (!id.point)
        return std::nullopt;
    return std::format("{}/{}/{}/{}/{}", ore_type(qt), ore_commodity_metric(qt),
                       id.commodity_code, id.ccy, to_upper(*id.point));
}

}

namespace ores::marketdata::core {

std::optional<std::string>
oresmd_projections::to_index_name(const domain::market_data_identifier& identifier) {
    if (const auto* ir = std::get_if<ir_market_data_identifier>(&identifier))
        return index_name_ir(*ir);
    return std::nullopt;
}

std::optional<std::string>
oresmd_projections::to_curve_key(const domain::market_data_identifier& identifier) {
    if (const auto* ir = std::get_if<ir_market_data_identifier>(&identifier))
        return curve_key_ir(*ir);
    return std::nullopt;
}

std::optional<std::string>
oresmd_projections::to_quote_key(const domain::market_data_identifier& identifier) {
    return std::visit(
        [](const auto& id) -> std::optional<std::string> {
            using T = std::decay_t<decltype(id)>;
            if constexpr (std::is_same_v<T, fx_market_data_identifier>)
                return quote_key_fx(id);
            else if constexpr (std::is_same_v<T, ir_market_data_identifier>)
                return quote_key_ir(id);
            else if constexpr (std::is_same_v<T, equity_market_data_identifier>)
                return quote_key_equity(id);
            else if constexpr (std::is_same_v<T, credit_market_data_identifier>)
                return quote_key_credit(id);
            else
                return quote_key_commodity(id);
        },
        identifier);
}

std::optional<market_series_key>
oresmd_projections::split_market_series_key(const std::string& key) {
    std::vector<std::string> parts;
    std::stringstream ss(key);
    std::string tok;
    while (std::getline(ss, tok, '/'))
        parts.push_back(tok);
    if (parts.size() < 3)
        return std::nullopt;

    market_series_key result;
    result.series_type = parts[0];
    result.metric = parts[1];
    result.qualifier = parts[2];
    for (std::size_t i = 3; i < parts.size(); ++i)
        result.qualifier += "/" + parts[i];
    return result;
}

}
