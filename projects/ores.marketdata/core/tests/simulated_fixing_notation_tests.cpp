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
#include "ores.marketdata.core/vendor/provider_notation.hpp"
#include "ores.marketdata.core/vendor/simulated_fixing_notation.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

namespace vendor = ores::marketdata::vendor;
namespace msg = ores::marketdata::messaging;

const std::string sofr("oresmd://ir/USD?type=fixing&index=ibor&name=SOFR");
const std::string libor_3m("oresmd://ir/GBP?type=fixing&index=ibor&name=LIBOR&tenor=3M");

msg::vendor_id id_of(std::string item) {
    return msg::vendor_id{.id_scheme = "oresmd", .external_id = std::move(item), .field = "value"};
}

}

TEST_CASE("the_simulated_fixing_notation_is_registered", "[vendor_notation][simulated_fixings]") {
    const auto* sut = vendor::notation_of("simulated-fixings");
    REQUIRE(sut != nullptr);
    CHECK(sut->id_scheme() == "oresmd");
}

TEST_CASE("a_fixing_is_written_as_its_address_and_read_back",
          "[vendor_notation][simulated_fixings]") {
    const vendor::simulated_fixing_notation sut;
    for (const auto& item : {sofr, libor_3m}) {
        const auto written = sut.write(vendor::quote_description{vendor::index_fixing{item}});
        REQUIRE(written.has_value());
        CHECK(*written == id_of(item));
        const auto read = sut.read(*written);
        REQUIRE(read.has_value());
        CHECK(*read == vendor::quote_description{vendor::index_fixing{item}});
    }
}

TEST_CASE("the_simulated_fixing_notation_refuses_what_is_not_a_fixing",
          "[vendor_notation][simulated_fixings]") {
    const vendor::simulated_fixing_notation sut;
    CHECK_FALSE(sut.write(vendor::quote_description{vendor::fx_spot_quote{"EUR", "USD"}}));
    CHECK_FALSE(sut.write(vendor::quote_description{vendor::index_fixing{"not an address"}}));
    CHECK_FALSE(sut.read(id_of(
        "oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1Y")));
    auto wrong_field = id_of(sofr);
    wrong_field.field = "PX_LAST";
    CHECK(sut.read(wrong_field).error().reason == vendor::notation_error::kind::wrong_field);
    auto wrong_scheme = id_of(sofr);
    wrong_scheme.id_scheme = "bbg";
    CHECK(sut.read(wrong_scheme).error().reason == vendor::notation_error::kind::wrong_scheme);
}
