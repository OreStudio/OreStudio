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
#include "ores.marketdata.core/vendor/fixing_setting_notation.hpp"
#include "ores.marketdata.core/vendor/provider_notation.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

namespace vendor = ores::marketdata::vendor;
namespace msg = ores::marketdata::messaging;

const std::string setting_libor_3m("oresmd://ir/USD?type=fixing&index=ibor&name=LIBOR&tenor=3M");
const std::string setting_saron("oresmd://ir/CHF?type=fixing&index=ibor&name=SARON");

msg::vendor_id setting_id_of(std::string external) {
    return msg::vendor_id{
        .id_scheme = "setting", .external_id = std::move(external), .field = "value"};
}

}

TEST_CASE("a_rate_publisher_is_registered_for_each_administrator",
          "[vendor_notation][fixing_setting]") {
    for (const auto* provider : {"iba", "emmi", "hkab", "gpw", "banrep", "snb", "mnb", "bok"}) {
        const auto* sut = vendor::notation_of(provider);
        REQUIRE(sut != nullptr);
        CHECK(sut->id_scheme() == "setting");
        CHECK(sut->provider() == provider);
    }
}

TEST_CASE("a_setting_is_written_as_its_currency_name_and_tenor",
          "[vendor_notation][fixing_setting]") {
    const vendor::fixing_setting_notation sut{"iba"};
    const auto written =
        sut.write(vendor::quote_description{vendor::index_fixing{setting_libor_3m}});
    REQUIRE(written.has_value());
    CHECK(*written == setting_id_of("USD LIBOR 3M"));
    const auto read = sut.read(*written);
    REQUIRE(read.has_value());
    CHECK(*read == vendor::quote_description{vendor::index_fixing{setting_libor_3m}});
}

TEST_CASE("a_setting_without_a_tenor_is_written_without_one", "[vendor_notation][fixing_setting]") {
    const vendor::fixing_setting_notation sut{"snb"};
    const auto written = sut.write(vendor::quote_description{vendor::index_fixing{setting_saron}});
    REQUIRE(written.has_value());
    CHECK(*written == setting_id_of("CHF SARON"));
    const auto read = sut.read(*written);
    REQUIRE(read.has_value());
    CHECK(*read == vendor::quote_description{vendor::index_fixing{setting_saron}});
}

TEST_CASE("the_setting_notation_refuses_what_is_not_a_rate_setting",
          "[vendor_notation][fixing_setting]") {
    const vendor::fixing_setting_notation sut{"iba"};
    CHECK_FALSE(sut.write(vendor::quote_description{vendor::fx_spot_quote{"EUR", "USD"}}));
    CHECK_FALSE(sut.write(vendor::quote_description{
        vendor::index_fixing{"oresmd://fx/EUR?type=fixing&index=fx&source=ECB&ccy=USD"}}));
    CHECK_FALSE(sut.read(setting_id_of("USD")));
    CHECK_FALSE(sut.read(setting_id_of("USD LIBOR 3M EXTRA")));
    CHECK_FALSE(sut.read(setting_id_of("usd LIBOR 3M")));
    auto wrong_scheme = setting_id_of("USD LIBOR 3M");
    wrong_scheme.id_scheme = "wmr_cut";
    CHECK(sut.read(wrong_scheme).error().reason == vendor::notation_error::kind::wrong_scheme);
    auto wrong_field = setting_id_of("USD LIBOR 3M");
    wrong_field.field = "PX_LAST";
    CHECK(sut.read(wrong_field).error().reason == vendor::notation_error::kind::wrong_field);
}
