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
#include "ores.marketdata.core/classification/series_classifier.hpp"
#include <algorithm>
#include <stdexcept>
#include <string_view>
#include <unordered_map>

namespace ores::marketdata::core {

namespace {

struct taxon {
    std::string asset_class;
    std::string series_subclass;
};

// Maps ORE series_type → the single asset class and subclass it measures.
// Every type here belongs to exactly one class; CORRELATION and GENERIC-MD do
// not and are handled below.
const std::unordered_map<std::string, taxon> k_table = {
    // FX
    {"FX", {"fx", "spot"}},
    {"FXFWD", {"fx", "forward"}},
    {"FX_OPTION", {"fx", "volatility"}},
    // Rates curves
    {"DISCOUNT", {"interest_rates", "yield"}},
    {"ZERO", {"interest_rates", "yield"}},
    {"MM", {"interest_rates", "yield"}},
    {"MM_FUTURE", {"interest_rates", "fra"}},
    {"FRA", {"interest_rates", "fra"}},
    {"IMM_FRA", {"interest_rates", "fra"}},
    {"IR_SWAP", {"interest_rates", "yield"}},
    // Rates spreads
    {"BASIS_SWAP", {"interest_rates", "basis"}},
    {"BMA_SWAP", {"interest_rates", "basis"}},
    {"CC_BASIS_SWAP", {"interest_rates", "xccy"}},
    {"CC_FIX_FLOAT_SWAP", {"interest_rates", "xccy"}},
    // Rates vols
    {"SWAPTION", {"interest_rates", "volatility"}},
    {"CAPFLOOR", {"interest_rates", "volatility"}},
    // Credit
    {"HAZARD_RATE", {"credit", "spread"}},
    {"CDS", {"credit", "spread"}},
    {"CDS_INDEX", {"credit", "index_credit"}},
    {"INDEX_CDS_OPTION", {"credit", "index_credit"}},
    {"RECOVERY_RATE", {"credit", "recovery"}},
    {"RATING", {"credit", "transition_probability"}},
    {"INDEX_CDS_TRANCHE", {"credit", "correlation"}},
    {"CPR", {"bond", "prepayment"}},
    // Equity
    {"EQUITY", {"equity", "spot"}},
    {"EQUITY_FWD", {"equity", "forward"}},
    {"EQUITY_DIVIDEND", {"equity", "forward"}},
    {"EQUITY_OPTION", {"equity", "volatility"}},
    // Commodity
    {"COMMODITY", {"commodity", "spot"}},
    {"COMMODITY_FWD", {"commodity", "forward"}},
    {"COMMODITY_OPTION", {"commodity", "volatility"}},
    {"OI_FUTURE", {"commodity", "forward"}},
    {"SHAPE_PROFILE", {"commodity", "seasonality"}},
    // Inflation
    {"ZC_INFLATIONSWAP", {"inflation", "swap"}},
    {"YY_INFLATIONSWAP", {"inflation", "swap"}},
    {"ZC_INFLATIONCAPFLOOR", {"inflation", "capfloor"}},
    {"YY_INFLATIONCAPFLOOR", {"inflation", "capfloor"}},
    {"SEASONALITY", {"inflation", "seasonality"}},
    // Bond
    {"BOND", {"bond", "price"}},
    {"BOND_OPTION", {"bond", "volatility"}},
    // Fixings (index series)
    {"FIXING", {"interest_rates", "index_fixing"}},
};

// Maps the instrument type GENERIC-MD states in its metric slot onto the same
// taxonomy the instrument's own series type carries.
const std::unordered_map<std::string, taxon> k_generic_md_table = {
    {"EQUITY_OPTION", {"equity", "volatility"}},
};

/**
 * ORE names a correlation operand after the factor class it belongs to: an
 * equity index is EQ-RIC:.SPX, an FX rate is FX-GENERIC-USD-EUR, a commodity
 * future is COMM-NYMEX:CL. A rates swap rate is the exception — it carries no
 * prefix and names its currency instead, as in EUR-CMS-10Y.
 */
std::optional<std::string> operand_asset_class(const std::string& operand) {
    if (operand.rfind("EQ-", 0) == 0)
        return "equity";
    if (operand.rfind("FX-", 0) == 0)
        return "fx";
    if (operand.rfind("COMM-", 0) == 0)
        return "commodity";
    if (operand.find("-CMS-") != std::string::npos)
        return "interest_rates";
    return std::nullopt;
}

std::vector<std::string_view> split_slash(const std::string& s) {
    std::vector<std::string_view> parts;
    std::string_view view(s);
    std::size_t start = 0;
    while (true) {
        const auto pos = view.find('/', start);
        if (pos == std::string_view::npos) {
            parts.push_back(view.substr(start));
            break;
        }
        parts.push_back(view.substr(start, pos - start));
        start = pos + 1;
    }
    return parts;
}

/**
 * The first two qualifier segments of a correlation key are its two operands;
 * any further segments are surface coordinates. Classes keep operand order and
 * appear once each, so FX against FX yields one class, not two.
 */
std::vector<std::string> correlation_asset_classes(const std::string& qualifier) {
    const auto parts = split_slash(qualifier);
    std::vector<std::string> classes;
    for (std::size_t i = 0; i < parts.size() && i < 2; ++i) {
        const auto code = operand_asset_class(std::string(parts[i]));
        if (!code)
            continue;
        if (std::find(classes.begin(), classes.end(), *code) == classes.end())
            classes.push_back(*code);
    }
    return classes;
}

}

series_classification series_classifier::classify(const std::string& series_type,
                                                  const std::string& metric,
                                                  const std::string& qualifier) {
    const auto result = try_classify(series_type, metric, qualifier);
    if (!result)
        throw std::invalid_argument("No classification rule for ORE series key: " + series_type +
                                    "/" + metric + "/" + qualifier);
    return *result;
}

std::optional<series_classification> series_classifier::try_classify(const std::string& series_type,
                                                                     const std::string& metric,
                                                                     const std::string& qualifier) {
    if (series_type == "CORRELATION")
        return series_classification{correlation_asset_classes(qualifier), "correlation"};

    if (series_type == "GENERIC-MD") {
        const auto it = k_generic_md_table.find(metric);
        if (it == k_generic_md_table.end())
            return std::nullopt;
        return series_classification{{it->second.asset_class}, it->second.series_subclass};
    }

    const auto it = k_table.find(series_type);
    if (it == k_table.end())
        return std::nullopt;
    return series_classification{{it->second.asset_class}, it->second.series_subclass};
}

std::vector<std::string> series_classifier::known_series_types() {
    std::vector<std::string> types;
    types.reserve(k_table.size() + 2);
    for (const auto& entry : k_table)
        types.push_back(entry.first);
    types.push_back("CORRELATION");
    types.push_back("GENERIC-MD");
    std::sort(types.begin(), types.end());
    return types;
}

}
