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
#include "ores.synthetic.api/feeds/generator_outputs.hpp"
#include <boost/uuid/random_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

namespace {

const std::string generator_outputs_tags("[feeds][generator_outputs]");

using namespace ores::synthetic::feed;
namespace domain = ores::synthetic::domain;

domain::ir_curve_generation_config a_gbp_libor_3m_curve() {
    domain::ir_curve_generation_config c;
    c.id = boost::uuids::random_generator()();
    c.currency_code = "GBP";
    c.index_family = "libor";
    c.tenor = "3M";
    return c;
}

domain::ir_curve_template_entry
entry(const domain::ir_curve_generation_config& curve, std::string start, std::string end) {
    domain::ir_curve_template_entry e;
    e.ir_curve_config_id = curve.id;
    e.start_tenor_code = std::move(start);
    e.end_tenor_code = std::move(end);
    return e;
}

}

TEST_CASE("a_curve_publishes_its_dateless_pillars_and_its_fixing", generator_outputs_tags) {
    const auto curve = a_gbp_libor_3m_curve();
    const auto other = a_gbp_libor_3m_curve();
    const auto outputs = ir_curve_outputs(curve,
                                          {entry(curve, "SPOT", "1Y"),
                                           entry(curve, "3M", "6M"),
                                           entry(curve, "1F", "2F"),
                                           entry(other, "SPOT", "2Y")});

    REQUIRE(outputs.has_value());
    CHECK(*outputs ==
          std::vector<std::string>{
              "oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=3M&"
              "term=1Y",
              "oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=3M&"
              "term=6M",
              "oresmd://ir/GBP?type=fixing&index=ibor&name=LIBOR&tenor=3M"});
}

TEST_CASE("a_meeting_dated_curve_publishes_no_fixing", generator_outputs_tags) {
    auto curve = a_gbp_libor_3m_curve();
    curve.tenor = "FOMC";
    const auto outputs = ir_curve_outputs(curve, {entry(curve, "1F", "2F")});

    REQUIRE(outputs.has_value());
    CHECK(outputs->empty());
}

TEST_CASE("a_spot_feed_publishes_its_quote", generator_outputs_tags) {
    CHECK(fx_spot_quote_uri("oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD")
              .has_value());
    CHECK_FALSE(fx_spot_quote_uri(
                    "oresmd://fx/EUR?type=quote&instrument=fx_option&quote=rate_lnvol&ccy=USD&"
                    "expiry=1M&strike_label=ATM")
                    .has_value());
}

TEST_CASE("a_surface_publishes_each_expiry_at_atm_and_each_delta", generator_outputs_tags) {
    const auto outputs = fx_vol_surface_outputs("EUR", "USD", {"1M", "1Y"});

    REQUIRE(outputs.size() == 10);
    CHECK(outputs.front() ==
          "oresmd://fx/EUR?type=quote&instrument=fx_option&quote=rate_lnvol&ccy=USD&expiry=1M&"
          "strike_label=ATM");
    CHECK(outputs[1] == fx_vol_point_uri("EUR", "USD", "1M", "25RR"));
    CHECK(outputs.back() == fx_vol_point_uri("EUR", "USD", "1Y", "10BF"));
}
