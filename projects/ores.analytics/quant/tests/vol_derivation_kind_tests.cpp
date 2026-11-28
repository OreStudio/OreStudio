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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.analytics.quant/math/vol_derivation_kind.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <set>
#include <string>

using namespace ores::analytics::quant::math;

namespace {

std::set<std::string> coordinate_set(const vol_derivation_kind& kind) {
    std::set<std::string> out;
    for (const auto name : kind.coordinates)
        out.emplace(name);
    return out;
}

}

TEST_CASE("the platform declares one kind per class it derives", "[vol_derivation_kind]") {
    const auto kinds = vol_derivation_kinds();
    REQUIRE(kinds.size() == 2);

    std::set<std::string> codes;
    for (const auto& kind : kinds) {
        CHECK_FALSE(kind.code.empty());
        CHECK(kind.version > 0);
        CHECK_FALSE(kind.asset_class.empty());
        CHECK_FALSE(kind.smile_model.empty());
        CHECK_FALSE(kind.coordinates.empty());
        // A code names one kind, or a stored value could not say which wrote it.
        CHECK(codes.emplace(kind.code).second);
    }
    CHECK(codes.count("FX_VOL_DERIVATION") == 1);
    CHECK(codes.count("IR_VOL_DERIVATION") == 1);
}

TEST_CASE("each class states the transform its series needs", "[vol_derivation_kind]") {
    // A price keeps a sign, so a pair's spot is measured on its logarithm.
    CHECK(fx_vol_derivation().transform == volatility_transform::log_return);
    CHECK(fx_vol_derivation().asset_class == "fx");
    CHECK(coordinate_set(fx_vol_derivation()) ==
          std::set<std::string>{"unit_ccy", "ccy", "expiry", "strike_label"});

    // A rate may cross zero, so a term's rate is measured on its change.
    CHECK(ir_vol_derivation().transform == volatility_transform::difference);
    CHECK(ir_vol_derivation().asset_class == "interest_rates");
    CHECK(coordinate_set(ir_vol_derivation()) ==
          std::set<std::string>{"ccy", "expiry", "term", "strike_level"});
}

TEST_CASE("a kind is found by the code a stored value states", "[vol_derivation_kind]") {
    const auto fx = vol_derivation_kind_named("FX_VOL_DERIVATION");
    REQUIRE(fx);
    CHECK(fx->code == fx_vol_derivation().code);
    CHECK(fx->version == fx_vol_derivation().version);
    CHECK(fx->method_id == fx_vol_derivation().method_id);

    const auto ir = vol_derivation_kind_named("IR_VOL_DERIVATION");
    REQUIRE(ir);
    CHECK(ir->transform == volatility_transform::difference);

    CHECK_FALSE(vol_derivation_kind_named("CDS_VOL_DERIVATION"));
    CHECK_FALSE(vol_derivation_kind_named(""));
}

TEST_CASE("a kind's method id parses, and the FX one is the id a stored series already carries",
          "[vol_derivation_kind]") {
    const auto fx = method_id_of(fx_vol_derivation());
    CHECK(boost::uuids::to_string(fx) == "fefdf018-d882-5c4e-bd27-2aeedfcad9c9");
    CHECK_FALSE(fx.is_nil());

    // A different class is a different computation, so it is a different id.
    CHECK(method_id_of(ir_vol_derivation()) != fx);
    CHECK_FALSE(method_id_of(ir_vol_derivation()).is_nil());
}
