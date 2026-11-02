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
#include "ores.ore.core/results/text_value_reader.hpp"
#include "ores.ore.core/results/output_catalogue.hpp"
#include "ores.testing/test_files.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

const std::string text_tags("[ore][results][text_value_reader]");

using ores::ore::results::family_named;
using ores::ore::results::is_text_value_family;
using ores::ore::results::read_text_values;

std::string text_sample_of(const std::string& code) {
    const auto* family = family_named(code);
    REQUIRE(family != nullptr);
    return ores::testing::read_file(ores::testing::project_root::resolve(family->sample_file));
}

}

TEST_CASE("an additional result keeps its trade, its name, its type and its value", text_tags) {
    const auto* family = family_named("additional_results");
    REQUIRE(family != nullptr);
    CHECK(is_text_value_family(*family));

    const auto outcome = read_text_values("#TradeId,ResultId,ResultType,ResultValue\n"
                                          "FX_TaRF,PricingConfigEngine,string,Generic\n"
                                          "FX_TaRF,npv,double,1234.5\n",
                                          *family);

    REQUIRE(outcome.refusals.empty());
    REQUIRE(outcome.rows.size() == 2);
    CHECK(outcome.rows[0].subject_type == "trade");
    CHECK(outcome.rows[0].subject == "FX_TaRF");
    CHECK(outcome.rows[0].name == "PricingConfigEngine");
    CHECK(outcome.rows[0].value_type == "string");
    CHECK(outcome.rows[0].value == "Generic");
    CHECK_FALSE(outcome.rows[0].figure.has_value());
    CHECK(outcome.rows[1].figure == 1234.5);
    CHECK(outcome.rows[1].line == 3);
}

TEST_CASE("a list of portfolio details names each entry by its heading or its type", text_tags) {
    const auto objects =
        read_text_values("#MarketObjectType,MarketObjectName\n\"DiscountCurve\",\"EUR|GBP|USD\"\n",
                         *family_named("marketObjects"));
    REQUIRE(objects.refusals.empty());
    REQUIRE(objects.rows.size() == 1);
    CHECK(objects.rows[0].name == "DiscountCurve");
    CHECK(objects.rows[0].value == "EUR|GBP|USD");
    CHECK(objects.rows[0].value_type == "text");
    CHECK(objects.rows[0].subject.empty());

    const auto indices =
        read_text_values("#SwapIndices\nEUR-CMS-30Y\nEUR-CMS-2Y\n", *family_named("swap_indices"));
    REQUIRE(indices.refusals.empty());
    REQUIRE(indices.rows.size() == 2);
    CHECK(indices.rows[1].name == "SwapIndices");
    CHECK(indices.rows[1].value == "EUR-CMS-2Y");
}

TEST_CASE("the calibrated model is one value, the whole document", text_tags) {
    const auto outcome = read_text_values(text_sample_of("calibration"), *family_named("calibration"));

    REQUIRE(outcome.refusals.empty());
    REQUIRE(outcome.rows.size() == 1);
    CHECK(outcome.rows[0].name == "CrossAssetModel");
    CHECK(outcome.rows[0].value_type == "xml");
    CHECK(outcome.rows[0].value.starts_with("<CrossAssetModel>"));
    CHECK(outcome.rows[0].value.ends_with("</CrossAssetModel>"));
}

TEST_CASE("a double that is not a figure is refused by line", text_tags) {
    const auto outcome = read_text_values("#TradeId,ResultId,ResultType,ResultValue\nT1,npv,double,x\n",
                                          *family_named("additional_results"));
    REQUIRE(outcome.refusals.size() == 1);
    CHECK(outcome.refusals.front().find("line 2") != std::string::npos);
}

TEST_CASE("every text-valued family's sample reads whole", text_tags) {
    for (const auto* code : {"additional_results", "marketObjects", "riskFactors",
                             "underlying_indices", "swap_indices", "calibration"}) {
        INFO("the family is " << code);
        const auto outcome = read_text_values(text_sample_of(code), *family_named(code));
        CHECK(outcome.refusals.empty());
        CHECK_FALSE(outcome.rows.empty());
    }
}
