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
#include "ores.testing/series_key_shape_seed.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

const std::string tags("[ore][market][registry]");

using ores::ore::domain::series_key_shape;
using ores::ore::market::reconstruct_key;
using ores::ore::market::series_key_registry;
using ores::testing::seed_registry;
using ores::testing::seed_shapes;

void check_registry_roundtrip(const series_key_registry& registry, const std::string& key) {
    INFO("key: " << key);
    const auto dk = registry.decompose(key);
    CHECK(reconstruct_key(dk) == key);
}

struct test_case {
    std::string key;
    std::string series_type;
    std::string metric;
    std::string qualifier;
    std::optional<std::string> point_id;
};

void check_decomposition(const series_key_registry& registry, const test_case& tc) {
    INFO("key: " << tc.key);
    const auto dk = registry.decompose(tc.key);
    CHECK(dk.series_type == tc.series_type);
    CHECK(dk.metric == tc.metric);
    CHECK(dk.qualifier == tc.qualifier);
    CHECK(dk.point_id == tc.point_id);
    CHECK(reconstruct_key(dk) == tc.key);
}

series_key_shape make_shape(const std::string& series_type,
                            int qualifier_depth,
                            bool has_point_dimension,
                            const std::string& default_point = "") {
    series_key_shape shape;
    shape.series_type = series_type;
    shape.qualifier_depth = qualifier_depth;
    shape.has_point_dimension = has_point_dimension;
    shape.default_point = default_point;
    return shape;
}

// Covers each branch the registry takes: a multi-segment qualifier, a
// single-segment one, a type with no point and a real default point, and a
// type with no point and no default.
std::vector<series_key_shape> fixture() {
    return {
        make_shape("FX", 2, false, "SPOT"),
        make_shape("DISCOUNT", 2, true),
        make_shape("SWAPTION", 1, true),
        make_shape("RECOVERY_RATE", 3, false),
        make_shape("CORRELATION", 2, true),
    };
}

} // namespace

// =============================================================================
// The seed — the rows every other case in this file reads
// =============================================================================

TEST_CASE("seed_carries_a_row_for_every_type_the_corpus_needs", tags) {
    const auto& shapes = seed_shapes();
    std::vector<std::string> types;
    types.reserve(shapes.size());
    for (const auto& s : shapes)
        types.push_back(s.series_type);

    // The eight the compiled table never knew. Each one folded its whole key
    // into the qualifier, so every distinct key became its own series.
    for (const auto& expected : {"OI_FUTURE",
                                 "SHAPE_PROFILE",
                                 "BOND_OPTION",
                                 "RATING",
                                 "INDEX_CDS_TRANCHE",
                                 "GENERIC-MD",
                                 "CPR",
                                 "FIXING"}) {
        INFO("type: " << expected);
        CHECK(std::find(types.begin(), types.end(), expected) != types.end());
    }

    // The thirty-five that were compiled in, plus those eight.
    CHECK(shapes.size() == 43);
}

TEST_CASE("seed_rows_never_claim_a_point_and_a_default_point_at_once", tags) {
    for (const auto& s : seed_shapes()) {
        INFO("type: " << s.series_type);
        CHECK_FALSE((s.has_point_dimension && !s.default_point.empty()));
    }
}

// =============================================================================
// series_key_registry — the key grammar, read from the seed
// =============================================================================

TEST_CASE("decompose_fx_without_point", tags) {
    check_decomposition(seed_registry(),
                        {"FX/RATE/EUR/CHF", "FX", "RATE", "EUR/CHF", std::nullopt});
}

TEST_CASE("decompose_fx_forward", tags) {
    check_decomposition(seed_registry(),
                        {"FXFWD/RATE/EUR/CHF/1Y", "FXFWD", "RATE", "EUR/CHF", "1Y"});
}

TEST_CASE("decompose_discount_curve", tags) {
    check_decomposition(
        seed_registry(),
        {"DISCOUNT/RATE/EUR/BANK_EUR_BORROW/2Y", "DISCOUNT", "RATE", "EUR/BANK_EUR_BORROW", "2Y"});
}

TEST_CASE("decompose_zero_curve_with_day_count", tags) {
    check_decomposition(seed_registry(),
                        {"ZERO/RATE/EUR/BANK_EUR_BORROW/A365/2Y",
                         "ZERO",
                         "RATE",
                         "EUR/BANK_EUR_BORROW/A365",
                         "2Y"});
}

TEST_CASE("decompose_mm_curve", tags) {
    check_decomposition(seed_registry(), {"MM/RATE/CHF/0D/1D", "MM", "RATE", "CHF/0D", "1D"});
}

TEST_CASE("decompose_fra", tags) {
    check_decomposition(seed_registry(), {"FRA/RATE/CHF/1M/6M", "FRA", "RATE", "CHF/1M", "6M"});
}

TEST_CASE("decompose_ir_swap", tags) {
    check_decomposition(seed_registry(),
                        {"IR_SWAP/RATE/CHF/2D/1D/1M", "IR_SWAP", "RATE", "CHF/2D/1D", "1M"});
}

TEST_CASE("decompose_basis_swap", tags) {
    check_decomposition(
        seed_registry(),
        {"BASIS_SWAP/BASIS_SPREAD/6M/3M/CHF/1Y", "BASIS_SWAP", "BASIS_SPREAD", "6M/3M/CHF", "1Y"});
}

TEST_CASE("decompose_bma_swap", tags) {
    check_decomposition(seed_registry(),
                        {"BMA_SWAP/RATIO/USD/3M/3M", "BMA_SWAP", "RATIO", "USD/3M", "3M"});
}

TEST_CASE("decompose_cc_basis_swap", tags) {
    check_decomposition(seed_registry(),
                        {"CC_BASIS_SWAP/BASIS_SPREAD/USD/3M/CHF/3M/1Y",
                         "CC_BASIS_SWAP",
                         "BASIS_SPREAD",
                         "USD/3M/CHF/3M",
                         "1Y"});
}

TEST_CASE("decompose_swaption_atm", tags) {
    check_decomposition(
        seed_registry(),
        {"SWAPTION/RATE_LNVOL/CHF/25Y/10Y/ATM", "SWAPTION", "RATE_LNVOL", "CHF", "25Y/10Y/ATM"});
}

TEST_CASE("decompose_capfloor_with_strike", tags) {
    check_decomposition(seed_registry(),
                        {"CAPFLOOR/RATE_LNVOL/CHF/20Y/6M/0/0/0.025",
                         "CAPFLOOR",
                         "RATE_LNVOL",
                         "CHF",
                         "20Y/6M/0/0/0.025"});
}

TEST_CASE("decompose_hazard_rate", tags) {
    check_decomposition(
        seed_registry(),
        {"HAZARD_RATE/RATE/CPTY_A/SR/USD/5Y", "HAZARD_RATE", "RATE", "CPTY_A/SR/USD", "5Y"});
}

TEST_CASE("decompose_recovery_rate_without_point", tags) {
    check_decomposition(seed_registry(),
                        {"RECOVERY_RATE/RATE/CPTY_A/SR/USD",
                         "RECOVERY_RATE",
                         "RATE",
                         "CPTY_A/SR/USD",
                         std::nullopt});
}

TEST_CASE("decompose_equity_spot_without_point", tags) {
    check_decomposition(seed_registry(),
                        {"EQUITY/PRICE/SP5", "EQUITY", "PRICE", "SP5", std::nullopt});
}

TEST_CASE("decompose_equity_forward", tags) {
    check_decomposition(seed_registry(),
                        {"EQUITY_FWD/PRICE/SP5/1Y", "EQUITY_FWD", "PRICE", "SP5", "1Y"});
}

TEST_CASE("decompose_commodity_without_point", tags) {
    check_decomposition(seed_registry(),
                        {"COMMODITY/PRICE/GOLD", "COMMODITY", "PRICE", "GOLD", std::nullopt});
}

TEST_CASE("decompose_zc_inflation_swap", tags) {
    check_decomposition(
        seed_registry(),
        {"ZC_INFLATIONSWAP/RATE/UKRPI/2Y", "ZC_INFLATIONSWAP", "RATE", "UKRPI", "2Y"});
}

TEST_CASE("decompose_correlation_operands_only", tags) {
    check_decomposition(seed_registry(),
                        {"CORRELATION/RATE/EUR-EURIBOR-6M/EUR-EURIBOR-3M",
                         "CORRELATION",
                         "RATE",
                         "EUR-EURIBOR-6M/EUR-EURIBOR-3M",
                         std::nullopt});
}

TEST_CASE("decompose_correlation_surface_point", tags) {
    // The two operands hold fixed positions, so the expiry/strike after them is
    // the point. Treating it as part of the qualifier would make every surface
    // coordinate its own single-observation series.
    check_decomposition(seed_registry(),
                        {"CORRELATION/RATE/EUR-CMS-10Y/EUR-CMS-2Y/1Y/ATM",
                         "CORRELATION",
                         "RATE",
                         "EUR-CMS-10Y/EUR-CMS-2Y",
                         "1Y/ATM"});
}

// =============================================================================
// The eight types the compiled table never knew
// =============================================================================

TEST_CASE("decompose_the_eight_types_the_corpus_carried_unmapped", tags) {
    check_decomposition(
        seed_registry(),
        {"OI_FUTURE/PRICE/USD/2019-10/CME/3M", "OI_FUTURE", "PRICE", "USD", "2019-10/CME/3M"});
    check_decomposition(seed_registry(),
                        {"BOND_OPTION/RATE_LNVOL/EUR_GENERIC/10Y/10Y/ATM",
                         "BOND_OPTION",
                         "RATE_LNVOL",
                         "EUR_GENERIC",
                         "10Y/10Y/ATM"});
    check_decomposition(seed_registry(),
                        {"RATING/TRANSITION_PROBABILITY/PROVIDER_1/A/A/1Y",
                         "RATING",
                         "TRANSITION_PROBABILITY",
                         "PROVIDER_1",
                         "A/A/1Y"});
    check_decomposition(seed_registry(),
                        {"INDEX_CDS_TRANCHE/BASE_CORRELATION/2I65BYEG6/3Y/0.03",
                         "INDEX_CDS_TRANCHE",
                         "BASE_CORRELATION",
                         "2I65BYEG6",
                         "3Y/0.03"});
    check_decomposition(seed_registry(),
                        {"GENERIC-MD/EQUITY_OPTION/RIC:.SPX/USD/2025-10-03/3300/C",
                         "GENERIC-MD",
                         "EQUITY_OPTION",
                         "RIC:.SPX",
                         "USD/2025-10-03/3300/C"});
    check_decomposition(
        seed_registry(),
        {"CPR/RATE/ISIN:XS0983610930", "CPR", "RATE", "ISIN:XS0983610930", std::nullopt});
    check_decomposition(seed_registry(),
                        {"FIXING/RATE/EUR-EONIA", "FIXING", "RATE", "EUR-EONIA", std::nullopt});

    // SHAPE_PROFILE's keys carry a slash inside the profile name, so the whole
    // remainder after the metric is the point and the profile names the series.
    const auto profile =
        seed_registry().decompose("SHAPE_PROFILE/SHAPE_FACTOR/PJM_WH_RT_PK/2021-01-01/7200/SEC");
    CHECK(profile.qualifier == "PJM_WH_RT_PK");
    CHECK(profile.point_id == std::optional<std::string>("2021-01-01/7200/SEC"));
}

TEST_CASE("has_point_dimension_answers_per_type", tags) {
    const auto& registry = seed_registry();
    CHECK_FALSE(registry.has_point_dimension("FX"));
    CHECK_FALSE(registry.has_point_dimension("EQUITY"));
    CHECK_FALSE(registry.has_point_dimension("COMMODITY"));
    CHECK_FALSE(registry.has_point_dimension("RECOVERY_RATE"));
    CHECK_FALSE(registry.has_point_dimension("CPR"));
    CHECK_FALSE(registry.has_point_dimension("FIXING"));
    CHECK(registry.has_point_dimension("DISCOUNT"));
    CHECK(registry.has_point_dimension("SWAPTION"));
    CHECK(registry.has_point_dimension("CORRELATION"));
    CHECK(registry.has_point_dimension("RATING"));
    CHECK_FALSE(registry.has_point_dimension("NOT_A_REGISTERED_TYPE"));
}

TEST_CASE("default_point_is_spot_only_where_spot_is_a_tenor", tags) {
    const auto& registry = seed_registry();
    CHECK(registry.default_point_for("FX") == "SPOT");
    CHECK(registry.default_point_for("EQUITY") == "SPOT");
    CHECK(registry.default_point_for("COMMODITY") == "SPOT");
    CHECK(registry.default_point_for("DISCOUNT").empty());
    CHECK(registry.default_point_for("RECOVERY_RATE").empty());
    CHECK(registry.default_point_for("CORRELATION").empty());
    CHECK(registry.default_point_for("NOT_A_REGISTERED_TYPE").empty());
}

TEST_CASE("decompose_unknown_type_fallback", tags) {
    // A type with no row keeps its whole key: qualifier absorbs everything
    // after the metric, and the point stays empty, so the key reconstructs
    // verbatim and a type ORE adds later never aborts an import.
    const auto dk = seed_registry().decompose("SOMETHING_NEW/SCORE/MOODYS/CPTY_A/AAA");
    CHECK(dk.series_type == "SOMETHING_NEW");
    CHECK(dk.metric == "SCORE");
    CHECK(dk.qualifier == "MOODYS/CPTY_A/AAA");
    CHECK(!dk.point_id.has_value());
    CHECK(reconstruct_key(dk) == "SOMETHING_NEW/SCORE/MOODYS/CPTY_A/AAA");
}

TEST_CASE("decompose_too_short_throws", tags) {
    CHECK_THROWS_AS(seed_registry().decompose("ONLYONE"), std::invalid_argument);
    CHECK_THROWS_AS(seed_registry().decompose(""), std::invalid_argument);
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
        "CORRELATION/RATE/EUR-CMS-10Y/EUR-CMS-2Y/1Y/ATM",
        "OI_FUTURE/PRICE/USD/2019-10/CME/3M",
        "SHAPE_PROFILE/SHAPE_FACTOR/PJM_WH_RT_PK/2021-01-01/7200/SEC",
        "BOND_OPTION/RATE_LNVOL/EUR_GENERIC/10Y/10Y/ATM",
        "RATING/TRANSITION_PROBABILITY/PROVIDER_1/A/A/1Y",
        "INDEX_CDS_TRANCHE/BASE_CORRELATION/2I65BYEG6/3Y/0.03",
        "GENERIC-MD/EQUITY_OPTION/RIC:.SPX/USD/2025-10-03/3300/C",
        "CPR/RATE/ISIN:XS0983610930",
        "FIXING/RATE/EUR-EONIA",
    };

    for (const auto& key : keys)
        check_registry_roundtrip(seed_registry(), key);
}

// =============================================================================
// series_key_registry — a value built from rows, with no database
// =============================================================================

TEST_CASE("registry_decompose_splits_qualifier_from_point", tags) {
    const series_key_registry registry(fixture());

    const auto discount = registry.decompose("DISCOUNT/RATE/EUR/CURVE/2Y");
    CHECK(discount.series_type == "DISCOUNT");
    CHECK(discount.metric == "RATE");
    CHECK(discount.qualifier == "EUR/CURVE");
    CHECK(discount.point_id == std::optional<std::string>("2Y"));

    const auto swaption = registry.decompose("SWAPTION/RATE_LNVOL/CHF/25Y/10Y/ATM");
    CHECK(swaption.qualifier == "CHF");
    CHECK(swaption.point_id == std::optional<std::string>("25Y/10Y/ATM"));

    const auto correlation = registry.decompose("CORRELATION/RATE/EUR-CMS-10Y/EUR-CMS-2Y/1Y/ATM");
    CHECK(correlation.qualifier == "EUR-CMS-10Y/EUR-CMS-2Y");
    CHECK(correlation.point_id == std::optional<std::string>("1Y/ATM"));
}

TEST_CASE("registry_decompose_folds_a_type_with_no_point_dimension", tags) {
    const series_key_registry registry(fixture());

    const auto fx = registry.decompose("FX/RATE/EUR/CHF");
    CHECK(fx.qualifier == "EUR/CHF");
    CHECK(!fx.point_id.has_value());

    const auto recovery = registry.decompose("RECOVERY_RATE/RATE/CPTY_A/SR/USD");
    CHECK(recovery.qualifier == "CPTY_A/SR/USD");
    CHECK(!recovery.point_id.has_value());
}

TEST_CASE("registry_decompose_folds_a_key_shorter_than_its_type", tags) {
    const series_key_registry registry(fixture());

    const auto dk = registry.decompose("DISCOUNT/RATE/EUR");
    CHECK(dk.qualifier == "EUR");
    CHECK(!dk.point_id.has_value());
    CHECK(reconstruct_key(dk) == "DISCOUNT/RATE/EUR");
}

TEST_CASE("registry_decompose_folds_an_uncatalogued_type", tags) {
    const series_key_registry registry(fixture());

    const auto dk = registry.decompose("OI_FUTURE/PRICE/USD/2019-10/CME/3M");
    CHECK(dk.series_type == "OI_FUTURE");
    CHECK(dk.metric == "PRICE");
    CHECK(dk.qualifier == "USD/2019-10/CME/3M");
    CHECK(!dk.point_id.has_value());
    CHECK(reconstruct_key(dk) == "OI_FUTURE/PRICE/USD/2019-10/CME/3M");
}

TEST_CASE("registry_decompose_throws_on_a_key_with_no_metric", tags) {
    const series_key_registry registry(fixture());
    CHECK_THROWS_AS(registry.decompose("ONLYONE"), std::invalid_argument);
    CHECK_THROWS_AS(registry.decompose(""), std::invalid_argument);
}

TEST_CASE("registry_roundtrips_every_fixture_key", tags) {
    const series_key_registry registry(fixture());
    for (const auto& key : {"FX/RATE/EUR/CHF",
                            "DISCOUNT/RATE/EUR/CURVE/2Y",
                            "SWAPTION/RATE_LNVOL/CHF/25Y/10Y/ATM",
                            "RECOVERY_RATE/RATE/CPTY_A/SR/USD",
                            "CORRELATION/RATE/EUR-CMS-10Y/EUR-CMS-2Y/1Y/ATM",
                            "OI_FUTURE/PRICE/USD/2019-10/CME/3M"})
        check_registry_roundtrip(registry, key);
}

TEST_CASE("registry_answers_from_its_rows_and_not_a_compiled_table", tags) {
    const series_key_registry registry(fixture());

    CHECK(registry.has_point_dimension("DISCOUNT"));
    CHECK(registry.has_point_dimension("CORRELATION"));
    CHECK_FALSE(registry.has_point_dimension("FX"));
    CHECK_FALSE(registry.has_point_dimension("RECOVERY_RATE"));

    // IR_SWAP has no row in this fixture, so a registry built from rows has
    // nothing to answer with: there is no compiled fallback behind it.
    CHECK_FALSE(registry.has_point_dimension("IR_SWAP"));
    CHECK(registry.default_point_for("IR_SWAP").empty());
}

TEST_CASE("registry_default_point_answers_from_its_rows", tags) {
    const series_key_registry registry(fixture());
    CHECK(registry.default_point_for("FX") == "SPOT");
    CHECK(registry.default_point_for("DISCOUNT").empty());
    CHECK(registry.default_point_for("RECOVERY_RATE").empty());
    CHECK(registry.default_point_for("OI_FUTURE").empty());
}

TEST_CASE("registry_known_series_types_is_sorted", tags) {
    const series_key_registry registry(fixture());
    const std::vector<std::string> expected{
        "CORRELATION", "DISCOUNT", "FX", "RECOVERY_RATE", "SWAPTION"};
    CHECK(registry.known_series_types() == expected);
}

TEST_CASE("registry_rejects_an_unseeded_table", tags) {
    try {
        const series_key_registry registry(std::vector<series_key_shape>{});
        FAIL("accepted an empty table, known types: " << registry.known_series_types().size());
    } catch (const std::invalid_argument& ex) {
        const std::string msg{ex.what()};
        CHECK(msg.find("ores_ore_series_key_shapes_tbl") != std::string::npos);
        CHECK(msg.find("unseeded") != std::string::npos);
    }
}

TEST_CASE("registry_rejects_a_row_with_a_point_and_a_default_point", tags) {
    auto shapes = fixture();
    shapes.push_back(make_shape("FXFWD", 2, true, "SPOT"));

    try {
        const series_key_registry registry(shapes);
        FAIL("accepted a contradictory row, known types: " << registry.known_series_types().size());
    } catch (const std::invalid_argument& ex) {
        const std::string msg{ex.what()};
        CHECK(msg.find("FXFWD") != std::string::npos);
        CHECK(msg.find("SPOT") != std::string::npos);
    }
}
