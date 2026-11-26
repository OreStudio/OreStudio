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
#include "ores.marketdata.core/vendor/derived_notation.hpp"
#include "ores.marketdata.core/vendor/provider_notation.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

// The derived provider spells the two kinds of value it states two ways: a fixing by its oresmd
// address, and a volatility point of an FX pair by the source's own scheme. One notation reads
// both, because a provider has one notation and it is how that provider spells every id it
// serves. These cases prove each arm and the refusals between them.

namespace {

namespace vendor = ores::marketdata::vendor;
namespace messaging = ores::marketdata::messaging;

const std::string derived_notation_tags("[vendor_notation][derived]");

const std::string a_fixing_item = "oresmd://ir/USD?type=fixing&index=ibor&name=SOFR&tenor=1M";

vendor::derived_notation sut;

}

TEST_CASE("a_fixing_reads_by_its_oresmd_address_and_writes_back", derived_notation_tags) {
    const auto read = sut.read(messaging::vendor_id{
        .id_scheme = "oresmd", .external_id = a_fixing_item, .field = "value"});
    REQUIRE(read.has_value());
    CHECK(*read == vendor::quote_description{vendor::index_fixing{.item = a_fixing_item}});

    const auto written = sut.write(*read);
    REQUIRE(written.has_value());
    CHECK(written->id_scheme == "oresmd");
    CHECK(written->external_id == a_fixing_item);
    CHECK(written->field == "value");
}

TEST_CASE("a_volatility_point_reads_by_the_sources_own_scheme_and_writes_back",
          derived_notation_tags) {
    const auto read = sut.read(messaging::vendor_id{
        .id_scheme = "derived_fx_vol", .external_id = "USDCZK/2M/10BF", .field = "rate_lnvol"});
    REQUIRE(read.has_value());
    CHECK(*read == vendor::quote_description{vendor::fx_option_quote{"USD", "CZK", "2M", "10BF"}});

    const auto written = sut.write(*read);
    REQUIRE(written.has_value());
    CHECK(written->id_scheme == "derived_fx_vol");
    CHECK(written->external_id == "USDCZK/2M/10BF");
    CHECK(written->field == "rate_lnvol");
}

TEST_CASE("a_point_that_is_not_a_pair_an_expiry_and_a_label_is_refused", derived_notation_tags) {
    for (const auto* item : {"USDCZK/2M",
                             "USDCZK/2M/10BF/1Y",
                             "USDCZKX/2M/10BF",
                             "USDCZK/2M/15BF",
                             "USDCZK/TENOR/ATM",
                             "USDCZK/2M/10BF "}) {
        INFO(item);
        const auto read = sut.read(messaging::vendor_id{
            .id_scheme = "derived_fx_vol", .external_id = item, .field = "rate_lnvol"});
        REQUIRE_FALSE(read.has_value());
        CHECK(read.error().reason == vendor::notation_error::kind::malformed);
    }
}

TEST_CASE("the_two_arms_do_not_read_one_anothers_ids", derived_notation_tags) {
    const auto fixing_as_point = sut.read(messaging::vendor_id{
        .id_scheme = "derived_fx_vol", .external_id = a_fixing_item, .field = "rate_lnvol"});
    REQUIRE_FALSE(fixing_as_point.has_value());

    const auto point_as_fixing = sut.read(messaging::vendor_id{
        .id_scheme = "oresmd", .external_id = "USDCZK/2M/10BF", .field = "value"});
    REQUIRE_FALSE(point_as_fixing.has_value());

    const auto another_provider = sut.read(
        messaging::vendor_id{.id_scheme = "demo_key", .external_id = "EUR/USD", .field = "value"});
    REQUIRE_FALSE(another_provider.has_value());
    CHECK(another_provider.error().reason == vendor::notation_error::kind::wrong_scheme);
}

TEST_CASE("the_derived_provider_writes_no_spot_and_no_pillar", derived_notation_tags) {
    const auto spot = sut.write(
        vendor::quote_description{vendor::fx_spot_quote{"EUR", "USD", vendor::price_side::last}});
    REQUIRE_FALSE(spot.has_value());
    CHECK(spot.error().reason == vendor::notation_error::kind::not_representable);

    const auto pillar = sut.write(vendor::quote_description{
        vendor::ir_curve_pillar{"USD", "sofr", "DEPO", "SPOT", "1M", ""}});
    REQUIRE_FALSE(pillar.has_value());
    CHECK(pillar.error().reason == vendor::notation_error::kind::not_representable);
}

TEST_CASE("a_point_of_one_currency_written_twice_is_refused", derived_notation_tags) {
    const auto written =
        sut.write(vendor::quote_description{vendor::fx_option_quote{"USD", "USD", "2M", "ATM"}});
    REQUIRE_FALSE(written.has_value());
    CHECK(written.error().reason == vendor::notation_error::kind::not_representable);
}
