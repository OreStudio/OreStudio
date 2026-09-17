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
#include "ores.ore.core/market/series_key_registry.hpp"
#include <stdexcept>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace ores::ore::market {

namespace {

struct series_key_info {
    /**
     * @brief Number of slash-separated segments after the metric that belong
     *        to the qualifier.  All remaining segments form the point_id.
     */
    unsigned int qualifier_depth;

    /**
     * @brief True when keys of this type carry a point of their own.
     *
     * False for a type with no tenor or surface dimension: decompose_key folds
     * its whole remainder into the qualifier, and the type answers for the
     * single point with default_point.
     */
    bool has_point_dimension;

    /**
     * @brief The point recorded for an observation of this type whose key
     *        carries none.  Empty when the single point is not a tenor.
     */
    std::string default_point;
};

// Maps series_type → its key shape.
// Qualifier: segments of the key (after type/metric) that together identify
//            the series and are stable across all market dates.
// Point_id:  remaining segments that vary within the series (tenor, strike,
//            surface coordinate).
const std::unordered_map<std::string, series_key_info> k_registry = {
    // ─── FX ──────────────────────────────────────────────────────────────────
    // FX/RATE/ccy1/ccy2 — spot, whose point is the real tenor SPOT
    {"FX", {2, false, "SPOT"}},
    // FXFWD/RATE/ccy1/ccy2/tenor
    {"FXFWD", {2, true, ""}},
    // FX_OPTION/metric/ccy1/ccy2/expiry/delta_or_strike
    {"FX_OPTION", {2, true, ""}},

    // ─── RATES: curves ───────────────────────────────────────────────────────
    // DISCOUNT/RATE/ccy/curve_id/tenor
    {"DISCOUNT", {2, true, ""}},
    // ZERO/RATE/ccy/curve_id/day_count/tenor  (day_count = A365, ActAct, …)
    {"ZERO", {3, true, ""}},
    // MM/RATE/ccy/index_tenor/tenor
    {"MM", {2, true, ""}},
    // MM_FUTURE/RATE/ccy/expiry/tenor
    {"MM_FUTURE", {2, true, ""}},
    // FRA/RATE/ccy/start_tenor/length
    {"FRA", {2, true, ""}},
    // IMM_FRA/RATE/ccy/imm_date/length
    {"IMM_FRA", {2, true, ""}},
    // IR_SWAP/RATE/ccy/settle/index_tenor/maturity
    {"IR_SWAP", {3, true, ""}},

    // ─── RATES: spreads ──────────────────────────────────────────────────────
    // BASIS_SWAP/BASIS_SPREAD/long_tenor/short_tenor/ccy/maturity
    {"BASIS_SWAP", {3, true, ""}},
    // BMA_SWAP/RATIO/ccy/index_tenor/maturity
    {"BMA_SWAP", {2, true, ""}},
    // CC_BASIS_SWAP/BASIS_SPREAD/ccy1/tenor1/ccy2/tenor2/maturity
    {"CC_BASIS_SWAP", {4, true, ""}},
    // CC_FIX_FLOAT_SWAP/SPREAD/ccy1/tenor1/ccy2/tenor2/maturity
    {"CC_FIX_FLOAT_SWAP", {4, true, ""}},

    // ─── RATES: vol surfaces ─────────────────────────────────────────────────
    // SWAPTION/metric/ccy/expiry/swap_tenor/strike
    {"SWAPTION", {1, true, ""}},
    // CAPFLOOR/metric/ccy/maturity/index_tenor/.../strike
    {"CAPFLOOR", {1, true, ""}},

    // ─── CREDIT ──────────────────────────────────────────────────────────────
    // HAZARD_RATE/RATE/entity/seniority/ccy/tenor
    {"HAZARD_RATE", {3, true, ""}},
    // CDS/SPREAD/entity/seniority/ccy/tenor
    {"CDS", {3, true, ""}},
    // CDS_INDEX/SPREAD/index_family/index_term/tenor
    {"CDS_INDEX", {2, true, ""}},
    // INDEX_CDS_OPTION/metric/index_family/index_term/expiry/tenor
    {"INDEX_CDS_OPTION", {2, true, ""}},
    // RECOVERY_RATE/RATE/entity/seniority/ccy — one rate per entity, no tenor
    {"RECOVERY_RATE", {3, false, ""}},

    // ─── EQUITY ──────────────────────────────────────────────────────────────
    // EQUITY/PRICE/name — spot
    {"EQUITY", {1, false, "SPOT"}},
    // EQUITY_FWD/PRICE/name/tenor
    {"EQUITY_FWD", {1, true, ""}},
    // EQUITY_DIVIDEND/RATE/name/tenor
    {"EQUITY_DIVIDEND", {1, true, ""}},
    // EQUITY_OPTION/metric/name/ccy/expiry/strike
    {"EQUITY_OPTION", {2, true, ""}},

    // ─── COMMODITY ───────────────────────────────────────────────────────────
    // COMMODITY/PRICE/name — spot
    {"COMMODITY", {1, false, "SPOT"}},
    // COMMODITY_FWD/PRICE/name/ccy/tenor
    {"COMMODITY_FWD", {2, true, ""}},
    // COMMODITY_OPTION/metric/name/ccy/expiry/strike
    {"COMMODITY_OPTION", {2, true, ""}},

    // ─── INFLATION ───────────────────────────────────────────────────────────
    // ZC_INFLATIONSWAP/RATE/index/tenor
    {"ZC_INFLATIONSWAP", {1, true, ""}},
    // YY_INFLATIONSWAP/RATE/index/tenor
    {"YY_INFLATIONSWAP", {1, true, ""}},
    // ZC_INFLATIONCAPFLOOR/metric/index/maturity/.../strike
    {"ZC_INFLATIONCAPFLOOR", {1, true, ""}},
    // YY_INFLATIONCAPFLOOR/metric/index/maturity/.../strike
    {"YY_INFLATIONCAPFLOOR", {1, true, ""}},
    // SEASONALITY/RATE/index/month_id
    {"SEASONALITY", {1, true, ""}},

    // ─── BOND ────────────────────────────────────────────────────────────────
    // BOND/PRICE/issuer/seniority/tenor
    // BOND/YIELD_SPREAD/issuer/seniority/tenor
    {"BOND", {2, true, ""}},

    // ─── CROSS_ASSET ─────────────────────────────────────────────────────────
    // CORRELATION/RATE/operand1/operand2/expiry/strike — the two operands hold
    // fixed positions, so the surface coordinate after them is the point.
    {"CORRELATION", {2, true, ""}},
};

// ─── Helpers ─────────────────────────────────────────────────────────────────

std::vector<std::string_view> split_slash(std::string_view s) {
    std::vector<std::string_view> parts;
    std::size_t start = 0;
    while (true) {
        const auto pos = s.find('/', start);
        if (pos == std::string_view::npos) {
            parts.push_back(s.substr(start));
            break;
        }
        parts.push_back(s.substr(start, pos - start));
        start = pos + 1;
    }
    return parts;
}

std::string
join_slash(const std::vector<std::string_view>& parts, std::size_t start, std::size_t end) {
    std::string result;
    for (auto i = start; i < end; ++i) {
        if (i > start)
            result += '/';
        result.append(parts[i]);
    }
    return result;
}

} // namespace

decomposed_key decompose_key(const std::string& key) {
    const auto parts = split_slash(key);
    if (parts.size() < 2)
        throw std::invalid_argument("ORE key has fewer than 2 segments: " + key);

    decomposed_key dk;
    dk.series_type = std::string(parts[0]);
    dk.metric = std::string(parts[1]);

    const auto it = k_registry.find(dk.series_type);

    if (it == k_registry.end()) {
        // Unknown type — absorb all remaining segments into qualifier, no point_id.
        // This guarantees a lossless roundtrip for any new or non-standard key type.
        dk.qualifier = join_slash(parts, 2, parts.size());
        return dk;
    }

    const auto& info = it->second;
    const std::size_t q_end = 2 + info.qualifier_depth;

    if (parts.size() <= q_end || !info.has_point_dimension) {
        // All remaining segments form the qualifier: the type has no point
        // dimension, or the key is shorter than the type expects.
        dk.qualifier = join_slash(parts, 2, parts.size());
        return dk;
    }

    dk.qualifier = join_slash(parts, 2, q_end);
    dk.point_id = join_slash(parts, q_end, parts.size());
    return dk;
}

std::string reconstruct_key(const decomposed_key& dk) {
    auto key = dk.series_type + '/' + dk.metric + '/' + dk.qualifier;
    if (dk.point_id)
        key += '/' + *dk.point_id;
    return key;
}

bool has_point_dimension(const std::string& series_type) {
    const auto it = k_registry.find(series_type);
    return it != k_registry.end() && it->second.has_point_dimension;
}

std::string default_point_for(const std::string& series_type) {
    const auto it = k_registry.find(series_type);
    return it == k_registry.end() ? std::string{} : it->second.default_point;
}

} // namespace ores::ore::market
