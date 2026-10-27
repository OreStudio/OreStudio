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
#include "ores.marketdata.service/app/delivery_meter.hpp"
#include <boost/uuid/random_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

namespace {

using namespace std::chrono_literals;
using ores::marketdata::service::app::delivery_entry;
using ores::marketdata::service::app::delivery_key;
using ores::marketdata::service::app::delivery_meter;

delivery_key key(const boost::uuids::uuid& party,
                 const std::string& uri,
                 std::chrono::system_clock::time_point period) {
    delivery_key k;
    k.party_id = party;
    k.provider_code = "bloomberg";
    k.oresmd_uri = uri;
    k.period_start = period;
    return k;
}

}

TEST_CASE("deliveries_of_one_party_item_and_hour_are_added", "[app][delivery_meter]") {
    delivery_meter sut;
    const auto party = boost::uuids::random_generator{}();
    const auto hour = delivery_meter::hour_of(std::chrono::system_clock::now());

    sut.add(key(party, "eur", hour));
    sut.add(key(party, "eur", hour), 4);

    const auto counts = sut.drain();
    REQUIRE(counts.size() == 1);
    CHECK(counts.front().deliveries == 5);
}

TEST_CASE("each_party_item_and_hour_has_a_count_of_its_own", "[app][delivery_meter]") {
    delivery_meter sut;
    boost::uuids::random_generator gen;
    const auto a = gen();
    const auto b = gen();
    const auto hour = delivery_meter::hour_of(std::chrono::system_clock::now());

    sut.add(key(a, "eur", hour));
    sut.add(key(b, "eur", hour));
    sut.add(key(a, "gbp", hour));
    sut.add(key(a, "eur", hour + 1h));

    CHECK(sut.pending() == 4);
    CHECK(sut.drain().size() == 4);
}

TEST_CASE("a_drain_gives_the_counts_once_and_starts_again_from_zero", "[app][delivery_meter]") {
    delivery_meter sut;
    const auto party = boost::uuids::random_generator{}();
    const auto hour = delivery_meter::hour_of(std::chrono::system_clock::now());
    sut.add(key(party, "eur", hour), 3);

    CHECK(sut.drain().size() == 1);
    CHECK(sut.drain().empty());
    CHECK(sut.pending() == 0);
}

TEST_CASE("a_batch_of_counts_is_added_under_one_call", "[app][delivery_meter]") {
    delivery_meter sut;
    const auto party = boost::uuids::random_generator{}();
    const auto hour = delivery_meter::hour_of(std::chrono::system_clock::now());

    sut.add_many({delivery_entry{key(party, "eur", hour), 2},
                  delivery_entry{key(party, "eur", hour), 3},
                  delivery_entry{key(party, "gbp", hour), 1}});

    const auto counts = sut.drain();
    REQUIRE(counts.size() == 2);
    std::int64_t total = 0;
    for (const auto& c : counts)
        total += c.deliveries;
    CHECK(total == 6);
}

TEST_CASE("a_count_that_is_not_positive_adds_nothing", "[app][delivery_meter]") {
    delivery_meter sut;
    const auto party = boost::uuids::random_generator{}();
    const auto hour = delivery_meter::hour_of(std::chrono::system_clock::now());

    sut.add(key(party, "eur", hour), 0);
    sut.add(key(party, "eur", hour), -3);

    CHECK(sut.pending() == 0);
}

TEST_CASE("the_hour_of_a_time_is_the_start_of_its_hour", "[app][delivery_meter]") {
    using namespace std::chrono;
    const auto t = sys_days{2026y / October / 23} + 10h + 37min + 12s;

    CHECK(delivery_meter::hour_of(t) == sys_days{2026y / October / 23} + 10h);
}
