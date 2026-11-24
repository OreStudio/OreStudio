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
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */
#include "ores.marketdata.service/app/derived_market_object_kind.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

constexpr const char* derived_kind_tags = "[derived_kind]";

namespace {

using ores::marketdata::service::app::can_withdraw;
using ores::marketdata::service::app::declaration_of;
using ores::marketdata::service::app::declaration_of_publish_kind;
using ores::marketdata::service::app::derived_kind;
using ores::marketdata::service::app::kind_declarations;
using ores::marketdata::service::app::withdrawal_refusal_of_publish_kind;

}

TEST_CASE("a kind is declared once and found by its publish kind", derived_kind_tags) {
    for (const auto& declared : kind_declarations) {
        const auto found = declaration_of_publish_kind(declared.publish_kind_code);
        REQUIRE(found.has_value());
        CHECK(found->kind == declared.kind);
        CHECK(&declaration_of(declared.kind) == &declared);
    }
}

TEST_CASE("an unknown publish kind names no declaration", derived_kind_tags) {
    CHECK_FALSE(declaration_of_publish_kind("marketdata.no_such_kind").has_value());

    const auto refusal = withdrawal_refusal_of_publish_kind("marketdata.no_such_kind");
    REQUIRE(refusal.has_value());
    CHECK(refusal->find("marketdata.no_such_kind") != std::string::npos);
}

TEST_CASE("only the curve can take a published set out of use", derived_kind_tags) {
    CHECK(can_withdraw(declaration_of(derived_kind::curve)));
    CHECK_FALSE(can_withdraw(declaration_of(derived_kind::fx_forward)));
    CHECK_FALSE(can_withdraw(declaration_of(derived_kind::fx_vol_surface)));
}

TEST_CASE("a withdrawal is refused at the raise for a kind that has none", derived_kind_tags) {
    CHECK_FALSE(withdrawal_refusal_of_publish_kind("marketdata.curve_publish").has_value());

    for (const auto code : {"marketdata.fx_forward_publish",
                            "marketdata.fx_vol_surface_publish"}) {
        const auto refusal = withdrawal_refusal_of_publish_kind(code);
        REQUIRE(refusal.has_value());
        CHECK(refusal->find(code) != std::string::npos);
        CHECK(refusal->find("no withdrawal kind") != std::string::npos);
    }
}
