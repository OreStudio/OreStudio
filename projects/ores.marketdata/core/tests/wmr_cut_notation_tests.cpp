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
#include "ores.marketdata.core/vendor/wmr_cut_notation.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

namespace vendor = ores::marketdata::vendor;
namespace msg = ores::marketdata::messaging;

const std::string cut_aud_jpy_nyc(
    "oresmd://fx/AUD?type=fixing&index=fx&source=WMR1000NYC&ccy=JPY");

msg::vendor_id cut_id_of(std::string external) {
    return msg::vendor_id{.id_scheme = "wmr_cut", .external_id = std::move(external), .field = "value"};
}

}

TEST_CASE("the_wmr_notation_is_registered", "[vendor_notation][wmr]") {
    const auto* sut = vendor::notation_of("wmr");
    REQUIRE(sut != nullptr);
    CHECK(sut->id_scheme() == "wmr_cut");
}

TEST_CASE("a_cut_fixing_is_written_as_its_cut_and_pair", "[vendor_notation][wmr]") {
    const vendor::wmr_cut_notation sut;
    const auto written = sut.write(vendor::quote_description{vendor::index_fixing{cut_aud_jpy_nyc}});
    REQUIRE(written.has_value());
    CHECK(*written == cut_id_of("WMR1000NYC AUDJPY"));
    const auto read = sut.read(*written);
    REQUIRE(read.has_value());
    CHECK(*read == vendor::quote_description{vendor::index_fixing{cut_aud_jpy_nyc}});
}

TEST_CASE("the_wmr_notation_refuses_what_is_not_a_cut", "[vendor_notation][wmr]") {
    const vendor::wmr_cut_notation sut;
    CHECK_FALSE(sut.write(vendor::quote_description{vendor::fx_spot_quote{"EUR", "USD"}}));
    CHECK_FALSE(sut.write(vendor::quote_description{
        vendor::index_fixing{"oresmd://ir/USD?type=fixing&index=ibor&name=SOFR"}}));
    CHECK_FALSE(sut.read(cut_id_of("WMR1000NYC")));
    CHECK_FALSE(sut.read(cut_id_of("WMR1000NYC EUR")));
    CHECK_FALSE(sut.read(cut_id_of("WMR1000NYC eurusd")));
    CHECK_FALSE(sut.read(cut_id_of("")));
}
