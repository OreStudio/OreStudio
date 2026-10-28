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
#include "ores.synthetic.api/feeds/fixing_calibration.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <string>
#include <vector>

namespace {

const std::string tags("[feeds][fixing_calibration]");

using Catch::Approx;
using namespace ores::synthetic::feed;

const std::string ecb_eur_usd("oresmd://fx/EUR?type=fixing&index=fx&source=ECB&ccy=USD");
const std::string imf_usd_eur("oresmd://fx/USD?type=fixing&index=fx&source=IMF&ccy=EUR");

/// The hourly spread of a mixture of per-tick components over @p ticks_per_hour ticks.
double hourly_spread(const std::vector<mixture_component>& c, int ticks_per_hour) {
    double w = 0, m = 0, second = 0;
    for (const auto& x : c)
        w += x.weight;
    for (const auto& x : c) {
        m += x.weight / w * x.mean;
        second += x.weight / w * (x.stdev * x.stdev + x.mean * x.mean);
    }
    return std::sqrt((second - m * m) * ticks_per_hour);
}

}

TEST_CASE("an_item_of_the_pair_is_direct_and_one_of_the_reverse_pair_is_inverted", tags) {
    CHECK(orientation_of(ecb_eur_usd, "EUR", "USD").value() == item_orientation::direct);
    CHECK(orientation_of(imf_usd_eur, "EUR", "USD").value() == item_orientation::inverted);
}

TEST_CASE("an_item_of_another_pair_or_not_a_fixing_cannot_start_the_feed", tags) {
    CHECK_FALSE(orientation_of(ecb_eur_usd, "EUR", "GBP").has_value());
    CHECK_FALSE(orientation_of("oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD",
                               "EUR",
                               "USD")
                    .has_value());
}

TEST_CASE("the_spread_of_log_returns_is_their_sample_standard_deviation", tags) {
    const std::vector<double> values{1.0, 1.01, 0.99, 1.02};
    const double r1 = std::log(1.01), r2 = std::log(0.99 / 1.01), r3 = std::log(1.02 / 0.99);
    const double mean = (r1 + r2 + r3) / 3.0;
    const double expected = std::sqrt(
        ((r1 - mean) * (r1 - mean) + (r2 - mean) * (r2 - mean) + (r3 - mean) * (r3 - mean)) / 2.0);

    CHECK(business_day_log_return_spread(values).value() == Approx(expected));
}

TEST_CASE("an_inverted_series_has_the_same_spread", tags) {
    const std::vector<double> values{1.10, 1.12, 1.09, 1.11, 1.13};
    std::vector<double> inverted;
    for (const double v : values)
        inverted.push_back(1.0 / v);

    CHECK(business_day_log_return_spread(inverted).value() ==
          Approx(business_day_log_return_spread(values).value()));
}

TEST_CASE("fewer_than_two_returns_have_no_spread", tags) {
    CHECK_FALSE(business_day_log_return_spread({1.1, 1.2}).has_value());
    CHECK_FALSE(business_day_log_return_spread({}).has_value());
}

TEST_CASE("a_business_day_spread_is_spread_over_every_hour_of_the_year", tags) {
    CHECK(hourly_spread_of(0.01) == Approx(0.01 * std::sqrt(252.0 / 8760.0)));
}

TEST_CASE("the_scaled_mixture_has_the_measured_hourly_spread_and_keeps_its_shape", tags) {
    const std::vector<mixture_component> mixture{{0.0, 0.0001, 0.95}, {0.0, 0.0004, 0.05}};
    const int ticks_per_hour = 1800;
    const double target = 0.0008;

    const auto scaled = scaled_stdevs(mixture, target, ticks_per_hour);

    REQUIRE(scaled.has_value());
    REQUIRE(scaled->size() == 2);
    CHECK((*scaled)[1] / (*scaled)[0] == Approx(4.0));
    CHECK(hourly_spread({{0.0, (*scaled)[0], 0.95}, {0.0, (*scaled)[1], 0.05}}, ticks_per_hour) ==
          Approx(target));
}

TEST_CASE("the_scaled_mixture_keeps_its_means", tags) {
    const std::vector<mixture_component> mixture{{0.00001, 0.0001, 0.5}, {-0.00001, 0.0002, 0.5}};
    const auto scaled = scaled_stdevs(mixture, 0.01, 60);

    REQUIRE(scaled.has_value());
    CHECK(hourly_spread({{0.00001, (*scaled)[0], 0.5}, {-0.00001, (*scaled)[1], 0.5}}, 60) ==
          Approx(0.01));
}

TEST_CASE("a_mixture_that_cannot_be_scaled_is_an_error", tags) {
    CHECK_FALSE(scaled_stdevs({}, 0.01, 60).has_value());
    CHECK_FALSE(scaled_stdevs({{0.0, 0.0, 1.0}}, 0.01, 60).has_value());
    CHECK_FALSE(scaled_stdevs({{0.0, 0.001, 1.0}}, 0.01, 0).has_value());
    CHECK_FALSE(scaled_stdevs({{0.01, 0.001, 0.5}, {-0.01, 0.001, 0.5}}, 0.000001, 60).has_value());
}
