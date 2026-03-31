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
#include "ores.ore/market/series_key_registry.hpp"

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_template_test_macros.hpp>

namespace {

const std::string tags("[ore][market][registry]");

using ores::ore::market::decompose_key;
using ores::ore::market::reconstruct_key;

void check_roundtrip(const std::string& key) {
    INFO("key: " << key);
    const auto dk = decompose_key(key);
    CHECK(!dk.series_type.empty());
    CHECK(!dk.metric.empty());
    CHECK(!dk.qualifier.empty());
    CHECK(reconstruct_key(dk) == key);
}

struct test_case {
    std::string key;
    std::string series_type;
    std::string metric;
    std::string qualifier;
    std::optional<std::string> point_id;
};

void check_decomposition(const test_case& tc) {
    INFO("key: " << tc.key);
    const auto dk = decompose_key(tc.key);
    CHECK(dk.series_type == tc.series_type);
    CHECK(dk.metric      == tc.metric);
    CHECK(dk.qualifier   == tc.qualifier);
    CHECK(dk.point_id    == tc.point_id);
    CHECK(reconstruct_key(dk) == tc.key);
}

} // namespace

TEST_CASE("decompose_fx_scalar", tags) {
    check_decomposition({"FX/RATE/EUR/CHF", "FX", "RATE", "EUR/CHF", std::nullopt});
}

TEST_CASE("decompose_fx_forward", tags) {
    check_decomposition({"FXFWD/RATE/EUR/CHF/1Y", "FXFWD", "RATE", "EUR/CHF", "1Y"});
}

TEST_CASE("decompose_discount_curve", tags) {
    check_decomposition({"DISCOUNT/RATE/EUR/BANK_EUR_BORROW/2Y",
                         "DISCOUNT", "RATE", "EUR/BANK_EUR_BORROW", "2Y"});
}

TEST_CASE("decompose_zero_curve_with_day_count", tags) {
    check_decomposition({"ZERO/RATE/EUR/BANK_EUR_BORROW/A365/2Y",
                         "ZERO", "RATE", "EUR/BANK_EUR_BORROW/A365", "2Y"});
}

TEST_CASE("decompose_mm_curve", tags) {
    check_decomposition({"MM/RATE/CHF/0D/1D", "MM", "RATE", "CHF/0D", "1D"});
}

TEST_CASE("decompose_fra", tags) {
    check_decomposition({"FRA/RATE/CHF/1M/6M", "FRA", "RATE", "CHF/1M", "6M"});
}

TEST_CASE("decompose_ir_swap", tags) {
    check_decomposition({"IR_SWAP/RATE/CHF/2D/1D/1M",
                         "IR_SWAP", "RATE", "CHF/2D/1D", "1M"});
}

TEST_CASE("decompose_basis_swap", tags) {
    check_decomposition({"BASIS_SWAP/BASIS_SPREAD/6M/3M/CHF/1Y",
                         "BASIS_SWAP", "BASIS_SPREAD", "6M/3M/CHF", "1Y"});
}

TEST_CASE("decompose_bma_swap", tags) {
    check_decomposition({"BMA_SWAP/RATIO/USD/3M/3M",
                         "BMA_SWAP", "RATIO", "USD/3M", "3M"});
}

TEST_CASE("decompose_cc_basis_swap", tags) {
    check_decomposition({"CC_BASIS_SWAP/BASIS_SPREAD/USD/3M/CHF/3M/1Y",
                         "CC_BASIS_SWAP", "BASIS_SPREAD", "USD/3M/CHF/3M", "1Y"});
}

TEST_CASE("decompose_swaption_atm", tags) {
    check_decomposition({"SWAPTION/RATE_LNVOL/CHF/25Y/10Y/ATM",
                         "SWAPTION", "RATE_LNVOL", "CHF", "25Y/10Y/ATM"});
}

TEST_CASE("decompose_capfloor_with_strike", tags) {
    check_decomposition({"CAPFLOOR/RATE_LNVOL/CHF/20Y/6M/0/0/0.025",
                         "CAPFLOOR", "RATE_LNVOL", "CHF", "20Y/6M/0/0/0.025"});
}

TEST_CASE("decompose_hazard_rate", tags) {
    check_decomposition({"HAZARD_RATE/RATE/CPTY_A/SR/USD/5Y",
                         "HAZARD_RATE", "RATE", "CPTY_A/SR/USD", "5Y"});
}

TEST_CASE("decompose_recovery_rate_scalar", tags) {
    check_decomposition({"RECOVERY_RATE/RATE/CPTY_A/SR/USD",
                         "RECOVERY_RATE", "RATE", "CPTY_A/SR/USD", std::nullopt});
}

TEST_CASE("decompose_equity_spot_scalar", tags) {
    check_decomposition({"EQUITY/PRICE/SP5", "EQUITY", "PRICE", "SP5", std::nullopt});
}

TEST_CASE("decompose_equity_forward", tags) {
    check_decomposition({"EQUITY_FWD/PRICE/SP5/1Y",
                         "EQUITY_FWD", "PRICE", "SP5", "1Y"});
}

TEST_CASE("decompose_commodity_scalar", tags) {
    check_decomposition({"COMMODITY/PRICE/GOLD",
                         "COMMODITY", "PRICE", "GOLD", std::nullopt});
}

TEST_CASE("decompose_zc_inflation_swap", tags) {
    check_decomposition({"ZC_INFLATIONSWAP/RATE/UKRPI/2Y",
                         "ZC_INFLATIONSWAP", "RATE", "UKRPI", "2Y"});
}

TEST_CASE("decompose_correlation_scalar", tags) {
    check_decomposition({"CORRELATION/RATE/EUR-EURIBOR-6M/EUR-EURIBOR-3M",
                         "CORRELATION", "RATE",
                         "EUR-EURIBOR-6M/EUR-EURIBOR-3M", std::nullopt});
}

TEST_CASE("decompose_unknown_type_fallback", tags) {
    // Unknown types: qualifier absorbs everything after metric, no point_id.
    const auto dk = decompose_key("RATING/SCORE/MOODYS/CPTY_A/AAA");
    CHECK(dk.series_type == "RATING");
    CHECK(dk.metric      == "SCORE");
    CHECK(dk.qualifier   == "MOODYS/CPTY_A/AAA");
    CHECK(!dk.point_id.has_value());
    CHECK(reconstruct_key(dk) == "RATING/SCORE/MOODYS/CPTY_A/AAA");
}

TEST_CASE("decompose_key_too_short_throws", tags) {
    CHECK_THROWS_AS(decompose_key("ONLYONE"), std::invalid_argument);
    CHECK_THROWS_AS(decompose_key(""),        std::invalid_argument);
}

TEST_CASE("roundtrip_all_known_types", tags) {
    const std::vector<std::string> keys = {
        "FX/RATE/EUR/USD",
        "FXFWD/RATE/EUR/USD/6M",
        "FX_OPTION/RATE_LNVOL/EUR/USD/1Y/ATM",
        "DISCOUNT/RATE/EUR/EUR1D/5Y",
        "ZERO/RATE/EUR/EUR1D/ActAct/10Y",
        "MM/RATE/EUR/0D/3M",
        "FRA/RATE/EUR/3M/6M",
        "IR_SWAP/RATE/EUR/2D/6M/10Y",
        "BASIS_SWAP/BASIS_SPREAD/6M/3M/EUR/5Y",
        "BMA_SWAP/RATIO/USD/3M/10Y",
        "CC_BASIS_SWAP/BASIS_SPREAD/EUR/6M/USD/3M/5Y",
        "CC_FIX_FLOAT_SWAP/SPREAD/EUR/1Y/USD/6M/3Y",
        "SWAPTION/RATE_LNVOL/EUR/1Y/10Y/ATM",
        "CAPFLOOR/RATE_LNVOL/EUR/5Y/3M/0/0/0.01",
        "HAZARD_RATE/RATE/ENTITY/SNRFOR/USD/3Y",
        "CDS/SPREAD/ENTITY/SNRFOR/USD/5Y",
        "CDS_INDEX/SPREAD/ITRAXX/5Y/3Y",
        "INDEX_CDS_OPTION/SPREAD/ITRAXX/5Y/1Y/3Y",
        "RECOVERY_RATE/RATE/ENTITY/SNRFOR/USD",
        "EQUITY/PRICE/SP5",
        "EQUITY_FWD/PRICE/SP5/2Y",
        "EQUITY_DIVIDEND/RATE/SP5/1Y",
        "EQUITY_OPTION/RATE_LNVOL/SP5/USD/1Y/ATM",
        "COMMODITY/PRICE/GOLD",
        "COMMODITY_FWD/PRICE/GOLD/USD/6M",
        "COMMODITY_OPTION/RATE_LNVOL/GOLD/USD/1Y/ATM",
        "ZC_INFLATIONSWAP/RATE/UKRPI/5Y",
        "YY_INFLATIONSWAP/RATE/EUHICP/3Y",
        "ZC_INFLATIONCAPFLOOR/RATE_LNVOL/UKRPI/5Y/0/0/0.02",
        "YY_INFLATIONCAPFLOOR/RATE_LNVOL/EUHICP/3Y/0/0/0.01",
        "SEASONALITY/RATE/UKRPI/JAN",
        "BOND/PRICE/GOVT/SNRFOR/10Y",
        "CORRELATION/RATE/EUR-EURIBOR-6M/EUR-EURIBOR-3M",
    };

    for (const auto& key : keys)
        check_roundtrip(key);
}
