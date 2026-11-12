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
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.synthetic.api/feeds/derived_fx_vol.hpp"
#include "ores.synthetic.api/feeds/derived_fx_vol_feed.hpp"
#include "ores.testing/fx_vol_surface_fixture.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <charconv>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <expected>
#include <limits>
#include <map>
#include <random>
#include <set>
#include <span>
#include <string>
#include <vector>

namespace {

const std::string derived_tags("[feeds][derived_fx_vol]");

using Catch::Approx;
using namespace ores::synthetic::feed;
using namespace std::chrono;
namespace mm = ores::marketdata::messaging;
namespace qm = ores::analytics::quant::math;

constexpr year_month_day derived_start{year{2021}, month{1}, day{4}};

/// Business-day fixings of the euro in two currencies from one seeded random walk each.
std::map<std::string, dated_series> derived_fixings(int count, std::uint32_t seed) {
    std::mt19937 rng(seed);
    std::normal_distribution<double> usd(0.0, 0.005);
    std::normal_distribution<double> jpy(0.0, 0.007);
    dated_series eur_usd;
    dated_series eur_jpy;
    double u = 1.10;
    double j = 130.0;
    for (sys_days d{derived_start}; static_cast<int>(eur_usd.dates.size()) < count; d += days{1}) {
        const weekday w{d};
        if (w == Saturday || w == Sunday)
            continue;
        u *= std::exp(usd(rng));
        j *= std::exp(jpy(rng));
        eur_usd.dates.emplace_back(d);
        eur_usd.values.push_back(u);
        eur_jpy.dates.emplace_back(d);
        eur_jpy.values.push_back(j);
    }
    return {{ecb_fixing_item("USD"), eur_usd}, {ecb_fixing_item("JPY"), eur_jpy}};
}

/// The fixings up to a day, as the reader would return them.
dated_series derived_window(const dated_series& all, year_month_day to) {
    dated_series out;
    for (std::size_t i = 0; i < all.dates.size(); ++i)
        if (all.dates[i] <= to) {
            out.dates.push_back(all.dates[i]);
            out.values.push_back(all.values[i]);
        }
    return out;
}

derived_fx_readers derived_readers_of(const std::map<std::string, dated_series>& fixings) {
    return {.fixings = [&fixings](const std::string& item,
                                  year_month_day to) -> std::expected<dated_series, std::string> {
                const auto it = fixings.find(item);
                if (it == fixings.end())
                    return std::unexpected("no such item");
                return derived_window(it->second, to);
            },
            .convention = [](const derived_fx_pair& pair)
                -> std::expected<ores::refdata::domain::fx_option_convention, std::string> {
                if (pair.quote == "JPY")
                    return ores::testing::eur_usd_fx_option_convention("PaSpot", "PaFwd");
                return ores::testing::eur_usd_fx_option_convention();
            }};
}

std::vector<std::string> derived_ids_of(const derived_fx_pair& pair) {
    std::vector<std::string> out;
    for (const auto& e : derived_fx_expiries)
        for (const auto label : derived_fx_labels)
            out.push_back(derived_fx_external_id(
                {.pair = pair, .expiry = std::string(e.label), .label = std::string(label)}));
    return out;
}

mm::adapter_demand_request derived_demand_of(const std::vector<std::string>& ids) {
    mm::adapter_demand_request request;
    for (const auto& id : ids)
        request.ids.push_back(
            {std::string(derived_fx_id_scheme), id, std::string(derived_fx_field)});
    return request;
}

}

TEST_CASE("a_derived_vendor_id_names_a_pair_an_expiry_and_a_label_and_reads_back", derived_tags) {
    const derived_fx_quote_id id{
        .pair = {.base = "EUR", .quote = "USD"}, .expiry = "1Y", .label = "25RR"};
    CHECK(derived_fx_external_id(id) == "EURUSD/1Y/25RR");
    CHECK(parse_derived_fx_external_id("EURUSD/1Y/25RR") == id);
    CHECK_FALSE(parse_derived_fx_external_id("EURUSD/5Y/25RR"));
    CHECK_FALSE(parse_derived_fx_external_id("EURUSD/1Y/15RR"));
    CHECK_FALSE(parse_derived_fx_external_id("EURUS/1Y/ATM"));
    CHECK_FALSE(parse_derived_fx_external_id("EURUSD/1Y/ATM/X"));
    CHECK_FALSE(parse_derived_fx_external_id("EURUSD"));
}

TEST_CASE("a_pair_reads_the_euro_fixings_of_its_sides", derived_tags) {
    const auto direct = ecb_legs_of({.base = "EUR", .quote = "USD"});
    CHECK(direct.plus == ecb_fixing_item("USD"));
    CHECK_FALSE(direct.minus);
    const auto cross = ecb_legs_of({.base = "USD", .quote = "JPY"});
    CHECK(cross.plus == ecb_fixing_item("JPY"));
    CHECK(cross.minus == ecb_fixing_item("USD"));
    const auto inverse = ecb_legs_of({.base = "GBP", .quote = "EUR"});
    CHECK_FALSE(inverse.plus);
    CHECK(inverse.minus == ecb_fixing_item("GBP"));
}

TEST_CASE("a_cross_returns_the_change_of_the_ratio_of_its_legs_on_the_days_both_hold",
          derived_tags) {
    const dated_series jpy{.dates = {derived_start,
                                     year_month_day{sys_days{derived_start} + days{1}},
                                     year_month_day{sys_days{derived_start} + days{2}},
                                     year_month_day{sys_days{derived_start} + days{3}}},
                           .values = {130.0, 131.0, 0.0, 129.0}};
    const dated_series usd{.dates = {derived_start,
                                     year_month_day{sys_days{derived_start} + days{1}},
                                     year_month_day{sys_days{derived_start} + days{3}}},
                           .values = {1.10, 1.12, 1.09}};
    const auto r = pair_log_returns(jpy, usd);
    REQUIRE(r);
    REQUIRE(r->returns.size() == 2);
    CHECK(r->returns[0] == Approx(std::log((131.0 / 1.12) / (130.0 / 1.10))).epsilon(1e-12));
    CHECK(r->returns[1] == Approx(std::log((129.0 / 1.09) / (131.0 / 1.12))).epsilon(1e-12));
    CHECK(r->last == year_month_day{sys_days{derived_start} + days{3}});

    const auto inverse = pair_log_returns(std::nullopt, usd);
    REQUIRE(inverse);
    CHECK(inverse->returns[0] == Approx(-std::log(1.12 / 1.10)).epsilon(1e-14));
    CHECK_FALSE(pair_log_returns(std::nullopt, std::nullopt));
}

TEST_CASE("a_derivation_becomes_one_tick_per_label_of_each_quoted_expiry", derived_tags) {
    const auto fixings = derived_fixings(400, 3);
    const auto returns = pair_log_returns(fixings.at(ecb_fixing_item("USD")), std::nullopt);
    REQUIRE(returns);
    std::vector<qm::fx_expiry> expiries;
    for (const auto& e : derived_fx_expiries)
        expiries.push_back(
            {.label = std::string(e.label),
             .years = e.years,
             .convention = *convention_at(ores::testing::eur_usd_fx_option_convention(), e.label)});
    const auto d = qm::derive_fx_quotes(returns->returns, expiries, {});
    REQUIRE(d);
    REQUIRE(d->expiries[7].has_value() == false);
    const auto ticks = derived_fx_ticks({.base = "EUR", .quote = "USD"},
                                        *d,
                                        *returns->last,
                                        40,
                                        std::chrono::system_clock::time_point::max());
    REQUIRE(ticks.size() == 7 * derived_fx_labels.size());
    CHECK(ticks.front().external_id == "EURUSD/1W/ATM");
    CHECK(ticks[1].external_id == "EURUSD/1W/25RR");
    CHECK(ticks.back().external_id == "EURUSD/1Y/10BF");
    CHECK(ticks.front().sequence == 40);
    CHECK(ticks.back().sequence == 40 + ticks.size() - 1);
    for (const auto& t : ticks) {
        CHECK(t.id_scheme == derived_fx_id_scheme);
        CHECK(t.field == derived_fx_field);
        CHECK(t.vendor_time == sys_days{*returns->last} + hours(17));
    }
    double atm = 0.0;
    std::from_chars(
        ticks.front().value.data(), ticks.front().value.data() + ticks.front().value.size(), atm);
    CHECK(atm == d->expiries[0]->quotes.atm);
}

TEST_CASE("the_feed_serves_only_its_notation_and_answers_demand_from_its_universe", derived_tags) {
    const auto fixings = derived_fixings(10, 1);
    derived_fx_vol_feed feed("derived-fx-vol", "derived", derived_readers_of(fixings));
    auto ids = derived_ids_of({.base = "EUR", .quote = "USD"});
    ids.emplace_back("not-a-derived-id");
    feed.set_universe(ids);
    const auto announce = feed.announcement();
    CHECK(announce.adapter_id == "derived-fx-vol");
    CHECK(announce.provider_code == "derived");
    CHECK(announce.universe.size() == ids.size() - 1);

    auto request = derived_demand_of({"EURUSD/1Y/ATM", "GBPUSD/1Y/ATM"});
    request.ids.push_back({"iso4217", "USD", "rate"});
    const auto response = feed.answer(request);
    REQUIRE(response.accepted.size() == 1);
    CHECK(response.accepted[0].external_id == "EURUSD/1Y/ATM");
    CHECK(response.rejected.size() == 2);
}

TEST_CASE("the_feed_publishes_a_day_once_and_the_next_day_when_its_fixings_arrive", derived_tags) {
    const auto fixings = derived_fixings(1300, 7);
    derived_fx_vol_feed feed("derived-fx-vol", "derived", derived_readers_of(fixings));
    const auto eurusd = derived_ids_of({.base = "EUR", .quote = "USD"});
    const auto usdjpy = derived_ids_of({.base = "USD", .quote = "JPY"});
    std::vector<std::string> universe = eurusd;
    universe.insert(universe.end(), usdjpy.begin(), usdjpy.end());
    feed.set_universe(universe);
    feed.demand(derived_demand_of({"EURUSD/1M/ATM", "EURUSD/1Y/25RR", "USDJPY/3M/25BF"}));

    const auto& usd = fixings.at(ecb_fixing_item("USD"));
    const auto day = usd.dates[1200];
    std::vector<mm::adapter_tick> sent;
    const auto collect = [&](const mm::adapter_tick& t) {
        sent.push_back(t);
    };
    CHECK(feed.refresh(day, collect) == 3);
    CHECK(feed.failures().empty());
    std::set<std::string> names;
    for (const auto& t : sent) {
        names.insert(t.external_id);
        CHECK(t.vendor_time == sys_days{day} + hours(17));
    }
    CHECK(names == std::set<std::string>{"EURUSD/1M/ATM", "EURUSD/1Y/25RR", "USDJPY/3M/25BF"});

    sent.clear();
    CHECK(feed.refresh(day, collect) == 0);
    const auto next = usd.dates[1201];
    CHECK(feed.refresh(next, collect) == 3);
    for (const auto& t : sent)
        CHECK(t.vendor_time == sys_days{next} + hours(17));
}

TEST_CASE("a_pair_the_feed_cannot_derive_is_named_and_the_others_are_still_sent", derived_tags) {
    auto fixings = derived_fixings(1300, 9);
    derived_fx_vol_feed feed("derived-fx-vol", "derived", derived_readers_of(fixings));
    const auto eurusd = derived_ids_of({.base = "EUR", .quote = "USD"});
    const auto gbpusd = derived_ids_of({.base = "GBP", .quote = "USD"});
    std::vector<std::string> universe = eurusd;
    universe.insert(universe.end(), gbpusd.begin(), gbpusd.end());
    feed.set_universe(universe);
    feed.demand(derived_demand_of({"EURUSD/3M/ATM", "GBPUSD/3M/ATM"}));
    std::size_t sent = 0;
    CHECK(feed.refresh(fixings.at(ecb_fixing_item("USD")).dates.back(),
                       [&](const mm::adapter_tick&) { ++sent; }) == 1);
    CHECK(sent == 1);
    REQUIRE(feed.failures().size() == 1);
    CHECK(feed.failures()[0].starts_with("GBPUSD: "));
}

TEST_CASE("an_id_demanded_after_its_pair_was_sent_is_sent_at_once_and_alone", derived_tags) {
    const auto fixings = derived_fixings(1300, 11);
    derived_fx_vol_feed feed("derived-fx-vol", "derived", derived_readers_of(fixings));
    feed.set_universe(derived_ids_of({.base = "EUR", .quote = "USD"}));
    const auto day = fixings.at(ecb_fixing_item("USD")).dates[1250];
    std::vector<std::string> sent;
    const auto collect = [&](const mm::adapter_tick& t) {
        sent.push_back(t.external_id);
    };

    feed.demand(derived_demand_of({"EURUSD/1M/ATM"}));
    CHECK(feed.refresh(day, collect) == 1);
    feed.demand(derived_demand_of({"EURUSD/1M/ATM", "EURUSD/6M/10RR"}));
    sent.clear();
    CHECK(feed.refresh(day, collect) == 1);
    CHECK(sent == std::vector<std::string>{"EURUSD/6M/10RR"});
    sent.clear();
    CHECK(feed.refresh(day, collect) == 0);
}

TEST_CASE("an_id_a_party_needs_for_the_first_time_is_sent_again_the_same_day", derived_tags) {
    const auto fixings = derived_fixings(1300, 13);
    derived_fx_vol_feed feed("derived-fx-vol", "derived", derived_readers_of(fixings));
    feed.set_universe(derived_ids_of({.base = "EUR", .quote = "USD"}));
    const auto day = fixings.at(ecb_fixing_item("USD")).dates[1250];
    std::vector<std::string> sent;
    const auto collect = [&](const mm::adapter_tick& t) {
        sent.push_back(t.external_id);
    };

    feed.demand(derived_demand_of({"EURUSD/1M/ATM", "EURUSD/3M/ATM"}));
    CHECK(feed.refresh(day, collect) == 2);

    auto again = derived_demand_of({"EURUSD/1M/ATM", "EURUSD/3M/ATM"});
    again.image = {again.ids.front()};
    feed.demand(again);
    sent.clear();
    CHECK(feed.refresh(day, collect) == 1);
    CHECK(sent == std::vector<std::string>{again.ids.front().external_id});

    sent.clear();
    CHECK(feed.refresh(day, collect) == 0);
}

TEST_CASE("a_quote_that_is_not_finite_is_left_out_of_the_ticks", derived_tags) {
    qm::fx_derivation d;
    d.expiries.emplace_back(qm::derived_fx_expiry{
        .label = "1M",
        .quotes = {.atm = 0.1,
                   .rr25 = std::numeric_limits<double>::quiet_NaN(),
                   .bf25 = 0.002,
                   .rr10 = std::numeric_limits<double>::infinity(),
                   .bf10 = 0.006},
        .smile = {.sigma = 0.1, .skewness = 0.0, .excess_kurtosis = 0.0, .years = 1.0 / 12.0},
        .window = 21});
    const auto ticks = derived_fx_ticks({.base = "EUR", .quote = "USD"},
                                        d,
                                        derived_start,
                                        0,
                                        std::chrono::system_clock::time_point::max());
    REQUIRE(ticks.size() == 3);
    CHECK(ticks[0].external_id == "EURUSD/1M/ATM");
    CHECK(ticks[1].external_id == "EURUSD/1M/25BF");
    CHECK(ticks[2].external_id == "EURUSD/1M/10BF");
}

TEST_CASE("a_quote_of_today_sent_before_the_end_of_the_day_is_not_dated_after_its_send",
          derived_tags) {
    using namespace std::chrono;
    qm::fx_derivation d;
    d.expiries.emplace_back(qm::derived_fx_expiry{
        .label = "1W",
        .quotes = {.atm = 0.1, .rr25 = 0.001, .bf25 = 0.002, .rr10 = 0.003, .bf10 = 0.006},
        .smile = {.sigma = 0.1, .skewness = 0.0, .excess_kurtosis = 0.0, .years = 7.0 / 365.0},
        .window = 21});
    const auto sent = sys_days{derived_start} + hours(15) + minutes(46);

    const auto today = derived_fx_ticks({.base = "EUR", .quote = "USD"}, d, derived_start, 0, sent);
    const auto past = derived_fx_ticks({.base = "EUR", .quote = "USD"},
                                       d,
                                       derived_start,
                                       0,
                                       std::chrono::system_clock::time_point::max());

    REQUIRE_FALSE(today.empty());
    for (const auto& t : today)
        CHECK(t.vendor_time == sent);
    REQUIRE_FALSE(past.empty());
    for (const auto& t : past)
        CHECK(t.vendor_time == sys_days{derived_start} + hours(17));
}

TEST_CASE("a_pair_names_its_option_convention_as_refdata_does", derived_tags) {
    CHECK(fx_option_convention_id({.base = "USD", .quote = "JPY"}) == "USD-JPY-FXOPTION");
}

TEST_CASE("a_pair_names_its_series_and_each_quote_as_the_catalogue_does", derived_tags) {
    CHECK(derived_fx_series_uri({.base = "USD", .quote = "JPY"}) ==
          "oresmd://fx/USD?type=series&instrument=fx_option&quote=rate_lnvol&ccy=JPY");
    CHECK(derived_fx_quote_uri(
              {.pair = {.base = "EUR", .quote = "USD"}, .expiry = "1Y", .label = "25RR"}) ==
          "oresmd://fx/EUR?type=quote&instrument=fx_option&quote=rate_lnvol&ccy=USD&expiry=1Y&"
          "strike_label=25RR");
}

TEST_CASE("a_return_carries_the_day_it_ends_on", derived_tags) {
    const dated_series usd{.dates = {derived_start,
                                     year_month_day{sys_days{derived_start} + days{1}},
                                     year_month_day{sys_days{derived_start} + days{4}}},
                           .values = {1.10, 1.12, 1.09}};
    const auto r = pair_log_returns(usd, std::nullopt);
    REQUIRE(r);
    CHECK(r->dates ==
          std::vector<year_month_day>{year_month_day{sys_days{derived_start} + days{1}},
                                      year_month_day{sys_days{derived_start} + days{4}}});
}

TEST_CASE("a_missing_day_is_derived_from_the_returns_up_to_it_and_no_later", derived_tags) {
    const auto fixings = derived_fixings(400, 13);
    const auto returns = pair_log_returns(fixings.at(ecb_fixing_item("USD")), std::nullopt);
    REQUIRE(returns);
    const auto convention = ores::testing::eur_usd_fx_option_convention();
    const derived_fx_pair pair{.base = "EUR", .quote = "USD"};
    const std::set<year_month_day> stored{returns->dates[300]};
    const auto history = derive_missing_days(pair, *returns, convention, stored, {}, 250);
    REQUIRE(history);
    CHECK(history->underived.empty());
    const auto* days = &history->days;
    REQUIRE(days->size() == returns->returns.size() - 249 - 1);
    CHECK(days->front().day == returns->dates[249]);
    CHECK(days->back().day == returns->dates.back());
    for (const auto& d : *days)
        CHECK(d.day != returns->dates[300]);

    const auto& on = (*days)[10];
    const auto k = static_cast<std::size_t>(249 + 10);
    REQUIRE(on.day == returns->dates[k]);
    const std::vector<qm::fx_expiry> month{
        {.label = "1M", .years = 1.0 / 12.0, .convention = *convention_at(convention, "1M")}};
    const auto direct =
        qm::derive_fx_quotes(std::span<const double>(returns->returns.data(), k + 1), month, {});
    REQUIRE(direct);
    REQUIRE(direct->expiries[0]);
    const auto atm = derived_fx_quote_uri({.pair = pair, .expiry = "1M", .label = "ATM"});
    bool found = false;
    for (const auto& p : on.points)
        if (p.coordinate == atm) {
            CHECK(p.value == direct->expiries[0]->quotes.atm);
            found = true;
        }
    CHECK(found);

    const auto whole = qm::derive_fx_quotes(returns->returns, month, {});
    CHECK(whole->expiries[0]->quotes.atm != direct->expiries[0]->quotes.atm);
}

TEST_CASE("a_day_whose_history_covers_no_long_expiry_quotes_only_the_short_ones", derived_tags) {
    const auto fixings = derived_fixings(300, 17);
    const auto returns = pair_log_returns(fixings.at(ecb_fixing_item("USD")), std::nullopt);
    REQUIRE(returns);
    const auto history = derive_missing_days({.base = "EUR", .quote = "USD"},
                                             *returns,
                                             ores::testing::eur_usd_fx_option_convention(),
                                             {},
                                             {},
                                             30);
    REQUIRE(history);
    const auto* days = &history->days;
    REQUIRE_FALSE(days->empty());
    for (const auto& p : days->front().points)
        CHECK(p.coordinate.find("&expiry=1Y&") == std::string::npos);
    bool year = false;
    for (const auto& p : days->back().points)
        year = year || p.coordinate.find("&expiry=1Y&") != std::string::npos;
    CHECK(year);
}

namespace {

/// A store held in memory: each pair's series, and the days written to it.
struct derived_memory_store {
    std::map<std::string, std::set<year_month_day>> held;
    std::map<std::string, std::size_t> writes;
    std::set<std::string> refuse;

    derived_fx_store store() {
        return {
            .ensure_series =
                [this](const derived_fx_pair& pair) -> std::expected<std::string, std::string> {
                const auto uri = derived_fx_series_uri(pair);
                if (refuse.contains(pair.base + pair.quote))
                    return std::unexpected("refused");
                held.try_emplace(uri);
                return uri;
            },
            .stored_days = [this](const std::string& series, const derived_fx_pair&, year_month_day)
                -> std::expected<std::set<year_month_day>, std::string> { return held[series]; },
            .write = [this](const std::string& series, const std::vector<derived_fx_day>& days)
                -> std::expected<void, std::string> {
                for (const auto& d : days)
                    held[series].insert(d.day);
                ++writes[series];
                return {};
            }};
    }
};

}

TEST_CASE("the_feed_keeps_the_whole_history_once_and_then_each_new_day", derived_tags) {
    const auto fixings = derived_fixings(400, 19);
    derived_memory_store memory;
    derived_fx_vol_feed feed(
        "derived-fx-vol", "derived", derived_readers_of(fixings), {}, memory.store());
    feed.set_universe(derived_ids_of({.base = "EUR", .quote = "USD"}));
    const auto& usd = fixings.at(ecb_fixing_item("USD"));
    const auto series = derived_fx_series_uri({.base = "EUR", .quote = "USD"});

    const auto first = feed.persist(usd.dates[350]);
    CHECK(feed.failures().empty());
    CHECK(first == 350 - 21 + 1);
    CHECK(memory.held[series].size() == first);
    CHECK(feed.persist(usd.dates[350]) == 0);
    CHECK(feed.persist(usd.dates[351]) == 1);
    CHECK(memory.held[series].contains(usd.dates[351]));
    CHECK(memory.writes[series] == 2);
}

TEST_CASE("the_feed_writes_no_day_the_store_already_holds", derived_tags) {
    const auto fixings = derived_fixings(200, 23);
    derived_memory_store memory;
    const auto& usd = fixings.at(ecb_fixing_item("USD"));
    const auto series = derived_fx_series_uri({.base = "EUR", .quote = "USD"});
    for (std::size_t i = 21; i <= 150; ++i)
        memory.held[series].insert(usd.dates[i]);
    derived_fx_vol_feed feed(
        "derived-fx-vol", "derived", derived_readers_of(fixings), {}, memory.store());
    feed.set_universe(derived_ids_of({.base = "EUR", .quote = "USD"}));
    CHECK(feed.persist(usd.dates.back()) == (200 - 1) - 150);
}

TEST_CASE("the_feed_keeps_every_pair_of_its_universe_and_names_one_it_cannot", derived_tags) {
    const auto fixings = derived_fixings(200, 29);
    derived_memory_store memory;
    memory.refuse.insert("EURUSD");
    derived_fx_vol_feed feed(
        "derived-fx-vol", "derived", derived_readers_of(fixings), {}, memory.store());
    auto universe = derived_ids_of({.base = "EUR", .quote = "USD"});
    const auto usdjpy = derived_ids_of({.base = "USD", .quote = "JPY"});
    universe.insert(universe.end(), usdjpy.begin(), usdjpy.end());
    feed.set_universe(universe);
    CHECK(feed.persist(fixings.at(ecb_fixing_item("USD")).dates.back()) > 0);
    REQUIRE(feed.failures().size() == 1);
    CHECK(feed.failures()[0].starts_with("EURUSD: the series"));
    CHECK_FALSE(memory.held[derived_fx_series_uri({.base = "USD", .quote = "JPY"})].empty());
}

TEST_CASE("a_feed_given_a_pair_limit_keeps_one_pair_a_call", derived_tags) {
    const auto fixings = derived_fixings(200, 43);
    derived_memory_store memory;
    derived_fx_vol_feed feed(
        "derived-fx-vol", "derived", derived_readers_of(fixings), {}, memory.store());
    auto universe = derived_ids_of({.base = "EUR", .quote = "USD"});
    const auto usdjpy = derived_ids_of({.base = "USD", .quote = "JPY"});
    universe.insert(universe.end(), usdjpy.begin(), usdjpy.end());
    feed.set_universe(universe);
    const auto day = fixings.at(ecb_fixing_item("USD")).dates.back();
    const auto eurusd = derived_fx_series_uri({.base = "EUR", .quote = "USD"});
    const auto usdjpy_series = derived_fx_series_uri({.base = "USD", .quote = "JPY"});

    CHECK(feed.persist(day, 1) > 0);
    CHECK(memory.held[eurusd].empty() != memory.held[usdjpy_series].empty());

    CHECK(feed.persist(day, 1) > 0);
    CHECK_FALSE(memory.held[eurusd].empty());
    CHECK_FALSE(memory.held[usdjpy_series].empty());

    CHECK(feed.persist(day, 1) == 0);
}

TEST_CASE("a_demand_that_replaces_one_not_yet_taken_keeps_both_images", derived_tags) {
    ores::marketdata::messaging::adapter_demand_request earlier;
    earlier.ids = {{"derived", "first", "rate"}};
    earlier.image = {{"derived", "first", "rate"}};
    ores::marketdata::messaging::adapter_demand_request later;
    later.ids = {{"derived", "first", "rate"}, {"derived", "second", "rate"}};
    later.image = {{"derived", "second", "rate"}};

    const auto held = coalesce_demands(earlier, later);

    CHECK(held.ids.size() == 2u);
    std::set<std::string> imaged;
    for (const auto& id : held.image)
        imaged.insert(id.external_id);
    CHECK(imaged == std::set<std::string>{"first", "second"});
}

TEST_CASE("a_feed_with_no_store_keeps_nothing", derived_tags) {
    const auto fixings = derived_fixings(200, 31);
    derived_fx_vol_feed feed("derived-fx-vol", "derived", derived_readers_of(fixings));
    feed.set_universe(derived_ids_of({.base = "EUR", .quote = "USD"}));
    CHECK(feed.persist(fixings.at(ecb_fixing_item("USD")).dates.back()) == 0);
}

TEST_CASE("a_history_held_whole_is_not_derived_again", derived_tags) {
    const auto fixings = derived_fixings(200, 37);
    derived_memory_store memory;
    auto readers = derived_readers_of(fixings);
    int conventions = 0;
    readers.convention = [&conventions](const derived_fx_pair&)
        -> std::expected<ores::refdata::domain::fx_option_convention, std::string> {
        if (++conventions > 1)
            return std::unexpected("read twice");
        return ores::testing::eur_usd_fx_option_convention();
    };
    derived_fx_vol_feed feed("derived-fx-vol", "derived", std::move(readers), {}, memory.store());
    feed.set_universe(derived_ids_of({.base = "EUR", .quote = "USD"}));
    const auto day = fixings.at(ecb_fixing_item("USD")).dates.back();
    CHECK(feed.persist(day) > 0);
    CHECK(feed.persist(day) == 0);
    CHECK(feed.failures().empty());
    CHECK(conventions == 1);
}

TEST_CASE("days_that_cannot_be_derived_are_named_once_and_not_tried_again", derived_tags) {
    auto fixings = derived_fixings(200, 41);
    auto& usd = fixings.at(ecb_fixing_item("USD"));
    for (std::size_t i = 0; i < 60; ++i)
        usd.values[i] = 1.10;
    derived_memory_store memory;
    derived_fx_vol_feed feed(
        "derived-fx-vol", "derived", derived_readers_of(fixings), {}, memory.store());
    feed.set_universe(derived_ids_of({.base = "EUR", .quote = "USD"}));
    CHECK(feed.persist(usd.dates.back()) > 0);
    REQUIRE(feed.failures().size() == 1);
    CHECK(feed.failures()[0].find("could not be derived") != std::string::npos);
    CHECK(feed.persist(usd.dates.back()) == 0);
    CHECK(feed.failures().empty());
}
