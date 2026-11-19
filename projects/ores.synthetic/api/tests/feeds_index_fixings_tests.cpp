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
#include "ores.synthetic.api/feeds/index_fixings.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <set>
#include <string>
#include <vector>

namespace {

const std::string index_fixings_tags("[feeds][index_fixings]");

using namespace ores::synthetic::feed;
namespace chr = std::chrono;

const std::string gbp_libor_3m("oresmd://ir/GBP?type=fixing&index=ibor&name=LIBOR&tenor=3M");

/// Friday, 2026-11-13, a business day.
const chr::year_month_day a_friday{chr::year{2026}, chr::November, chr::day{13}};

}

TEST_CASE("an_ibor_index_address_states_its_name_and_tenor", index_fixings_tags) {
    CHECK(index_fixing_uri("GBP", "libor", "3M").value() == gbp_libor_3m);
}

TEST_CASE("an_overnight_index_address_states_no_tenor", index_fixings_tags) {
    CHECK(index_fixing_uri("USD", "sofr", "").value() ==
          "oresmd://ir/USD?type=fixing&index=ibor&name=SOFR");
}

TEST_CASE("an_index_with_no_currency_or_family_has_no_address", index_fixings_tags) {
    CHECK_FALSE(index_fixing_uri("", "libor", "3M").has_value());
    CHECK_FALSE(index_fixing_uri("GBP", "", "3M").has_value());
}

TEST_CASE("a_history_ends_at_the_level_and_holds_the_business_days", index_fixings_tags) {
    const auto history = derive_fixing_history(0.05, a_friday, 5, 42, gbp_libor_3m, {}).value();
    REQUIRE(history.size() == 5);
    CHECK(history.back().value == "0.050000");
    CHECK(history.back().date == a_friday);
    for (const auto& fixing : history) {
        const chr::weekday day{chr::sys_days{fixing.date}};
        CHECK(day != chr::Saturday);
        CHECK(day != chr::Sunday);
    }
}

TEST_CASE("a_fixed_seed_states_one_history_and_another_seed_another", index_fixings_tags) {
    const auto first = derive_fixing_history(0.05, a_friday, 8, 42, gbp_libor_3m, {}).value();
    const auto again = derive_fixing_history(0.05, a_friday, 8, 42, gbp_libor_3m, {}).value();
    const auto other_seed = derive_fixing_history(0.05, a_friday, 8, 43, gbp_libor_3m, {}).value();
    CHECK(first == again);
    CHECK(first != other_seed);
}

TEST_CASE("one_index_states_its_own_history", index_fixings_tags) {
    const auto gbp = derive_fixing_history(0.05, a_friday, 8, 42, gbp_libor_3m, {}).value();
    const std::string usd_sofr("oresmd://ir/USD?type=fixing&index=ibor&name=SOFR");
    const auto usd = derive_fixing_history(0.05, a_friday, 8, 42, usd_sofr, {}).value();
    REQUIRE(gbp.size() == usd.size());
    bool differs = false;
    for (std::size_t i = 0; i < gbp.size(); ++i)
        differs = differs || gbp[i].value != usd[i].value;
    CHECK(differs);
}

TEST_CASE("a_held_day_is_left_out_and_the_others_are_unchanged", index_fixings_tags) {
    const auto whole = derive_fixing_history(0.05, a_friday, 8, 42, gbp_libor_3m, {}).value();
    REQUIRE(whole.size() == 8);
    const auto held =
        derive_fixing_history(0.05, a_friday, 8, 42, gbp_libor_3m, {whole[3].date, whole[6].date})
            .value();
    REQUIRE(held.size() == 6);
    std::vector<index_fixing> expected;
    for (std::size_t i = 0; i < whole.size(); ++i)
        if (i != 3 && i != 6)
            expected.push_back(whole[i]);
    CHECK(held == expected);
}

TEST_CASE("a_longer_window_keeps_the_days_it_shares_with_a_shorter_one", index_fixings_tags) {
    const auto short_window =
        derive_fixing_history(0.05, a_friday, 10, 42, gbp_libor_3m, {}).value();
    const auto long_window =
        derive_fixing_history(0.05, a_friday, 20, 42, gbp_libor_3m, {}).value();
    REQUIRE(long_window.size() == 20);
    const std::vector<index_fixing> tail(long_window.end() - 10, long_window.end());
    CHECK(tail == short_window);
}

TEST_CASE("a_negative_level_states_no_history_and_a_zero_one_states_zeros", index_fixings_tags) {
    CHECK_FALSE(derive_fixing_history(-0.01, a_friday, 5, 42, gbp_libor_3m, {}).has_value());
    const auto at_zero = derive_fixing_history(0.0, a_friday, 5, 42, gbp_libor_3m, {}).value();
    REQUIRE(at_zero.size() == 5);
    for (const auto& fixing : at_zero)
        CHECK(fixing.value == "0.000000");
}

TEST_CASE("a_history_of_no_days_is_empty", index_fixings_tags) {
    CHECK(derive_fixing_history(0.05, a_friday, 0, 42, gbp_libor_3m, {}).value().empty());
}
