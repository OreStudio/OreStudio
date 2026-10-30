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
#include "ores.marketdata.service/app/billing.hpp"
#include <boost/uuid/random_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <numeric>
#include <string>

namespace {

using namespace std::chrono_literals;
using namespace ores::marketdata::service::app;

struct world {
    boost::uuids::random_generator gen;
    std::chrono::system_clock::time_point from =
        std::chrono::floor<std::chrono::hours>(std::chrono::system_clock::now()) - 48h;
    std::chrono::system_clock::time_point to = from + 24h;
    ores::marketdata::domain::feed live;
    ores::marketdata::domain::feed simulated;
    std::vector<ores::marketdata::domain::rate_card_component> card;
    usage_events events;
    boost::uuids::uuid alice = gen();
    boost::uuids::uuid bob = gen();

    world() {
        live.id = gen();
        live.name = "bloomberg-live";
        live.kind = "live";
        simulated.id = gen();
        simulated.name = "bloomberg-sim";
        simulated.kind = "simulated";
    }

    billing_period period() const {
        return {from, to};
    }

    void component(const ores::marketdata::domain::feed& source,
                   const std::string& driver,
                   std::int64_t charge,
                   const std::string& right = "") {
        ores::marketdata::domain::rate_card_component c;
        c.id = gen();
        c.feed_id = source.id;
        c.driver = driver;
        c.licence_right = right;
        c.currency = "USD";
        c.charge_micros = charge;
        c.effective_from = from - 24h;
        card.push_back(c);
    }

    void subscribes(const boost::uuids::uuid& party,
                    const ores::marketdata::domain::feed& source,
                    bool non_display = false,
                    bool derived = false) {
        ores::marketdata::domain::subscription_interval i;
        i.id = gen();
        i.party_id = party;
        i.feed_id = source.id;
        i.provider_code = "bloomberg";
        i.display = true;
        i.non_display = non_display;
        i.derived = derived;
        i.interval_from = from - 1h;
        events.subscriptions.push_back(i);
    }

    void demands(const boost::uuids::uuid& party,
                 const ores::marketdata::domain::feed& source,
                 const std::string& uri) {
        ores::marketdata::domain::demand_interval i;
        i.id = gen();
        i.party_id = party;
        i.feed_id = source.id;
        i.provider_code = "bloomberg";
        i.oresmd_uri = uri;
        i.interval_from = from - 1h;
        events.demands.push_back(i);
    }

    void delivered(const boost::uuids::uuid& party,
                   const ores::marketdata::domain::feed& source,
                   std::int64_t count) {
        ores::marketdata::domain::delivery_count n;
        n.id = gen();
        n.party_id = party;
        n.feed_id = source.id;
        n.provider_code = "bloomberg";
        n.oresmd_uri = "oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD";
        n.period_start = from + 1h;
        n.flushed_at = from + 2h;
        n.deliveries = count;
        events.deliveries.push_back(n);
    }
};

std::int64_t total_of(const allocation& a) {
    return std::accumulate(
        a.charges.begin(),
        a.charges.end(),
        a.unallocated_micros,
        [](std::int64_t sum, const party_charge& c) { return sum + c.total_micros; });
}

std::int64_t
charge_of(const allocation& a, const boost::uuids::uuid& party, const std::string& driver) {
    std::int64_t sum = 0;
    for (const auto& c : a.charges)
        if (c.party_id == party && c.driver == driver)
            sum += c.total_micros;
    return sum;
}

const std::string eur = "oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD";
const std::string gbp = "oresmd://fx/GBP?type=quote&instrument=fx_spot&quote=rate&ccy=USD";

}

TEST_CASE("an_even_split_adds_up_and_differs_by_at_most_one", "[app][billing]") {
    const auto parts = split_evenly(100, 3);
    REQUIRE(parts.size() == 3);
    CHECK(parts[0] == 34);
    CHECK(parts[1] == 33);
    CHECK(parts[2] == 33);
    CHECK(std::accumulate(parts.begin(), parts.end(), std::int64_t{0}) == 100);
    CHECK(split_evenly(7, 0).empty());
    CHECK(split_evenly(0, 4) == std::vector<std::int64_t>(4, 0));
}

TEST_CASE("a_weighted_split_adds_up_and_follows_the_weights", "[app][billing]") {
    const auto parts = split_weighted(1000, {1, 1, 2});
    REQUIRE(parts.size() == 3);
    CHECK(parts[0] == 250);
    CHECK(parts[1] == 250);
    CHECK(parts[2] == 500);

    const auto odd = split_weighted(100, {1, 1, 1});
    CHECK(std::accumulate(odd.begin(), odd.end(), std::int64_t{0}) == 100);
    CHECK(odd[0] == 34);
}

TEST_CASE("a_weighted_split_with_no_weight_gives_nothing", "[app][billing]") {
    CHECK(split_weighted(100, {0, 0}) == std::vector<std::int64_t>(2, 0));
    CHECK(split_weighted(100, {}).empty());
}

TEST_CASE("each_driver_counts_what_it_names", "[app][billing]") {
    world w;
    w.component(w.live, "fixed_per_period", 1000000);
    w.component(w.live, "per_instrument", 50000);
    w.component(w.live, "per_party", 200000);
    w.component(w.live, "per_licence", 300000, "non_display");
    w.component(w.live, "per_licence", 700000, "derived");
    w.component(w.live, "per_thousand_ticks", 20000);
    w.subscribes(w.alice, w.live, true, false);
    w.subscribes(w.bob, w.live, false, true);
    w.demands(w.alice, w.live, eur);
    w.demands(w.bob, w.live, eur);
    w.demands(w.bob, w.live, gbp);
    w.delivered(w.alice, w.live, 1500);
    w.delivered(w.bob, w.live, 500);

    const auto bill = rate_source(w.live, w.card, w.events, w.period());

    REQUIRE(bill.lines.size() == 6);
    CHECK(bill.lines[0].quantity == 1);
    CHECK(bill.lines[0].total_micros == 1000000);
    CHECK(bill.lines[1].quantity == 2);
    CHECK(bill.lines[1].total_micros == 100000);
    CHECK(bill.lines[2].quantity == 2);
    CHECK(bill.lines[2].total_micros == 400000);
    CHECK(bill.lines[3].quantity == 1);
    CHECK(bill.lines[3].total_micros == 300000);
    CHECK(bill.lines[4].quantity == 1);
    CHECK(bill.lines[4].total_micros == 700000);
    CHECK(bill.lines[5].quantity == 2000);
    CHECK(bill.lines[5].total_micros == 40000);
    CHECK(bill.total_micros == 1000000 + 100000 + 400000 + 300000 + 700000 + 40000);
    CHECK(bill.currency == "USD");
}

TEST_CASE("the_charge_for_ticks_is_rounded_to_the_nearest_micro", "[app][billing]") {
    world w;
    w.component(w.live, "per_thousand_ticks", 1);
    w.subscribes(w.alice, w.live);
    w.delivered(w.alice, w.live, 1499);

    CHECK(rate_source(w.live, w.card, w.events, w.period()).total_micros == 1);
    w.events.deliveries.front().deliveries = 1500;
    CHECK(rate_source(w.live, w.card, w.events, w.period()).total_micros == 2);
}

TEST_CASE("events_of_another_source_are_not_counted", "[app][billing]") {
    world w;
    w.component(w.live, "per_party", 100);
    w.subscribes(w.alice, w.simulated);

    CHECK(rate_source(w.live, w.card, w.events, w.period()).total_micros == 0);
}

TEST_CASE("an_interval_counts_only_when_it_overlaps_the_period", "[app][billing]") {
    world w;
    w.component(w.live, "per_party", 100);
    w.subscribes(w.alice, w.live);
    w.events.subscriptions.front().interval_to = w.from - 30min;
    w.subscribes(w.bob, w.live);
    w.events.subscriptions.back().interval_from = w.to;

    CHECK(rate_source(w.live, w.card, w.events, w.period()).total_micros == 0);

    w.events.subscriptions.front().interval_to = w.from + 1min;
    CHECK(rate_source(w.live, w.card, w.events, w.period()).total_micros == 100);
}

TEST_CASE("a_component_counts_only_in_the_period_it_applies_to", "[app][billing]") {
    world w;
    w.component(w.live, "fixed_per_period", 100);
    w.component(w.live, "fixed_per_period", 900);
    w.card.front().effective_to = w.from - 1h;
    w.card.back().effective_from = w.to + 1h;

    CHECK(rate_source(w.live, w.card, w.events, w.period()).lines.empty());
}

TEST_CASE("a_live_and_a_simulated_source_give_the_same_bill_apart_from_the_label",
          "[app][billing]") {
    world w;
    for (const auto* source : {&w.live, &w.simulated}) {
        w.component(*source, "fixed_per_period", 1000000);
        w.component(*source, "per_party", 200000);
        w.component(*source, "per_thousand_ticks", 20000);
        w.subscribes(w.alice, *source);
        w.subscribes(w.bob, *source);
        w.delivered(w.alice, *source, 3000);
    }

    const auto live = rate_source(w.live, w.card, w.events, w.period());
    const auto simulated = rate_source(w.simulated, w.card, w.events, w.period());

    CHECK_FALSE(live.shadow);
    CHECK(simulated.shadow);
    REQUIRE(live.lines.size() == simulated.lines.size());
    for (std::size_t i = 0; i < live.lines.size(); ++i) {
        CHECK(live.lines[i].driver == simulated.lines[i].driver);
        CHECK(live.lines[i].quantity == simulated.lines[i].quantity);
        CHECK(live.lines[i].total_micros == simulated.lines[i].total_micros);
    }
    CHECK(live.total_micros == simulated.total_micros);

    const auto live_split = allocate(live, w.events);
    const auto simulated_split = allocate(simulated, w.events);
    CHECK(live_split.charges.size() == simulated_split.charges.size());
    CHECK(total_of(live_split) == total_of(simulated_split));
}

TEST_CASE("a_fixed_fee_is_split_equally_among_the_active_subscribers", "[app][billing]") {
    world w;
    w.component(w.live, "fixed_per_period", 1000001);
    w.subscribes(w.alice, w.live);
    w.subscribes(w.bob, w.live);
    const auto bill = rate_source(w.live, w.card, w.events, w.period());

    const auto split = allocate(bill, w.events);

    CHECK(charge_of(split, w.alice, "fixed_per_period") +
              charge_of(split, w.bob, "fixed_per_period") ==
          1000001);
    const auto a = charge_of(split, w.alice, "fixed_per_period");
    const auto b = charge_of(split, w.bob, "fixed_per_period");
    CHECK(std::max(a, b) - std::min(a, b) <= 1);
    CHECK(split.unallocated_micros == 0);
}

TEST_CASE("a_fixed_fee_with_no_subscriber_is_left_unallocated", "[app][billing]") {
    world w;
    w.component(w.live, "fixed_per_period", 500);
    const auto bill = rate_source(w.live, w.card, w.events, w.period());

    const auto split = allocate(bill, w.events);

    CHECK(split.charges.empty());
    CHECK(split.unallocated_micros == 500);
}

TEST_CASE("an_instrument_is_shared_among_the_parties_that_demanded_it", "[app][billing]") {
    world w;
    w.component(w.live, "per_instrument", 90);
    w.subscribes(w.alice, w.live);
    w.subscribes(w.bob, w.live);
    w.demands(w.alice, w.live, eur);
    w.demands(w.bob, w.live, eur);
    w.demands(w.bob, w.live, gbp);
    const auto bill = rate_source(w.live, w.card, w.events, w.period());

    const auto split = allocate(bill, w.events);

    CHECK(charge_of(split, w.alice, "per_instrument") == 45);
    CHECK(charge_of(split, w.bob, "per_instrument") == 45 + 90);
    CHECK(total_of(split) == bill.total_micros);
}

TEST_CASE("a_licence_is_borne_by_the_party_that_holds_the_right", "[app][billing]") {
    world w;
    w.component(w.live, "per_licence", 700, "derived");
    w.subscribes(w.alice, w.live, false, true);
    w.subscribes(w.bob, w.live);
    const auto bill = rate_source(w.live, w.card, w.events, w.period());

    const auto split = allocate(bill, w.events);

    CHECK(charge_of(split, w.alice, "per_licence") == 700);
    CHECK(charge_of(split, w.bob, "per_licence") == 0);
}

TEST_CASE("ticks_are_split_by_the_deliveries_counted_for_each_party", "[app][billing]") {
    world w;
    w.component(w.live, "per_thousand_ticks", 1000000);
    w.subscribes(w.alice, w.live);
    w.subscribes(w.bob, w.live);
    w.delivered(w.alice, w.live, 3000);
    w.delivered(w.bob, w.live, 1000);
    const auto bill = rate_source(w.live, w.card, w.events, w.period());

    const auto split = allocate(bill, w.events);

    CHECK(bill.total_micros == 4000000);
    CHECK(charge_of(split, w.alice, "per_thousand_ticks") == 3000000);
    CHECK(charge_of(split, w.bob, "per_thousand_ticks") == 1000000);
}

TEST_CASE("the_charges_and_the_unallocated_part_add_up_to_the_bill", "[app][billing]") {
    world w;
    w.component(w.live, "fixed_per_period", 1000003);
    w.component(w.live, "per_instrument", 77777);
    w.component(w.live, "per_party", 12345);
    w.component(w.live, "per_licence", 54321, "non_display");
    w.component(w.live, "per_thousand_ticks", 33333);
    w.subscribes(w.alice, w.live, true, false);
    w.subscribes(w.bob, w.live, true, false);
    w.demands(w.alice, w.live, eur);
    w.demands(w.bob, w.live, eur);
    w.demands(w.alice, w.live, gbp);
    w.delivered(w.alice, w.live, 12345);
    w.delivered(w.bob, w.live, 6789);
    const auto bill = rate_source(w.live, w.card, w.events, w.period());

    const auto split = allocate(bill, w.events);

    CHECK(total_of(split) == bill.total_micros);
}

TEST_CASE("a_report_has_a_bill_for_each_source_with_a_rate_card", "[app][billing]") {
    world w;
    w.component(w.live, "per_party", 100);
    w.subscribes(w.alice, w.live);
    w.subscribes(w.alice, w.simulated);

    const auto report = make_report({w.live, w.simulated}, w.card, w.events, w.period());

    REQUIRE(report.bills.size() == 1);
    CHECK(report.bills.front().source_name == "bloomberg-live");
    CHECK(report.unallocated_micros == 0);
}

TEST_CASE("a_report_can_be_narrowed_to_one_source_by_name", "[app][billing]") {
    world w;
    for (const auto* source : {&w.live, &w.simulated}) {
        w.component(*source, "per_party", 100);
        w.subscribes(w.alice, *source);
    }

    const auto report =
        make_report({w.live, w.simulated}, w.card, w.events, w.period(), "bloomberg-sim");

    REQUIRE(report.bills.size() == 1);
    CHECK(report.bills.front().source_name == "bloomberg-sim");
    CHECK(report.bills.front().shadow);
}

TEST_CASE("the_charges_of_a_report_add_up_to_its_bills", "[app][billing]") {
    world w;
    for (const auto* source : {&w.live, &w.simulated}) {
        w.component(*source, "fixed_per_period", 1000003);
        w.component(*source, "per_thousand_ticks", 33333);
        w.subscribes(w.alice, *source);
        w.subscribes(w.bob, *source);
        w.delivered(w.alice, *source, 7001);
        w.delivered(w.bob, *source, 2999);
    }

    const auto report = make_report({w.live, w.simulated}, w.card, w.events, w.period());

    std::int64_t billed = 0;
    for (const auto& b : report.bills)
        billed += b.total_micros;
    std::int64_t charged = report.unallocated_micros;
    for (const auto& c : report.charges)
        charged += c.total_micros;
    CHECK(billed > 0);
    CHECK(charged == billed);
}

TEST_CASE("of_two_components_that_overlap_the_newest_one_bills", "[app][billing]") {
    world w;
    w.component(w.live, "per_party", 100);
    w.component(w.live, "per_party", 250);
    w.card.back().effective_from = w.from - 1h;
    w.subscribes(w.alice, w.live);

    const auto bill = rate_source(w.live, w.card, w.events, w.period());

    REQUIRE(bill.lines.size() == 1);
    CHECK(bill.lines.front().charge_micros == 250);
    CHECK(bill.total_micros == 250);
}

TEST_CASE("components_of_different_licence_rights_do_not_replace_each_other", "[app][billing]") {
    world w;
    w.component(w.live, "per_licence", 100, "non_display");
    w.component(w.live, "per_licence", 200, "derived");
    w.subscribes(w.alice, w.live, true, true);

    const auto bill = rate_source(w.live, w.card, w.events, w.period());

    CHECK(bill.lines.size() == 2);
    CHECK(bill.total_micros == 300);
}
