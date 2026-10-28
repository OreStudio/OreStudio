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
#include "ores.marketdata.official/domain/frankfurter.hpp"
#include "ores.marketdata.official/domain/source_feed.hpp"
#include <catch2/catch_test_macros.hpp>
#include <map>
#include <string>
#include <vector>

namespace {

using namespace ores::marketdata::official::domain;
using namespace std::chrono;
namespace net = ores::http::client::net;

/// Rows of =/v2/providers/ecb/rates= as Frankfurter sent them on 2026-10-28, cut short.
const std::string ecb_rates = R"([{"date":"2026-10-20","base":"EUR","quote":"AUD","rate":1.6056},)"
                              R"({"date":"2026-10-20","base":"EUR","quote":"EUR","rate":1.0},)"
                              R"({"date":"2026-10-20","base":"EUR","quote":"USD","rate":1.1612},)"
                              R"({"date":"2026-10-21","base":"EUR","quote":"USD","rate":1.1598},)"
                              R"({"date":"2026-10-21","base":"EUR","quote":"CZK","rate":24.329}])";

/// Entries of =/v2/providers= in the same shape, cut short.
const std::string providers =
    R"([{"key":"ECB","pivot_currency":"EUR","currencies":["AUD","EUR","USD","CZK"]},)"
    R"({"key":"BOE","pivot_currency":"GBP","currencies":["GBP","USD","EUR"]}])";

/// A transport that answers each address with a stored body, and records what it was asked.
class stored_transport final : public net::transport {
public:
    std::map<std::string, std::string> bodies;
    std::vector<std::string> asked;

    std::expected<net::response, net::failure> get(std::string_view url,
                                                   const net::request_limits&) override {
        asked.emplace_back(url);
        const auto found = bodies.find(std::string(url));
        if (found == bodies.end())
            return std::unexpected(net::failure{net::failure_kind::network, "no stored body"});
        return net::response{200, "application/json", found->second};
    }
};

constexpr year_month_day d(int day) {
    return year_month_day{year{2026}, month{10}, std::chrono::day{static_cast<unsigned>(day)}};
}

}

TEST_CASE("a_rates_response_reads_as_rates_without_the_pivot_against_itself",
          "[domain][frankfurter]") {
    const auto rates = parse_rates(ecb_rates);

    REQUIRE(rates.has_value());
    REQUIRE(rates->size() == 4);
    CHECK(rates->front().date == d(20));
    CHECK(rates->front().base == "EUR");
    CHECK(rates->front().quote == "AUD");
    CHECK(rates->front().rate == "1.6056");
    CHECK(rates->back().rate == "24.329");
}

TEST_CASE("a_provider_quotes_its_currencies_and_not_its_pivot", "[domain][frankfurter]") {
    const auto ecb = parse_universe(providers, "ecb");

    REQUIRE(ecb.has_value());
    CHECK(ecb->pivot == "EUR");
    CHECK(ecb->currencies == std::vector<std::string>{"AUD", "USD", "CZK"});
    CHECK_FALSE(parse_universe(providers, "fed").has_value());
}

TEST_CASE("a_response_that_is_not_a_list_of_rates_is_refused", "[domain][frankfurter]") {
    CHECK_FALSE(parse_rates("{}").has_value());
    CHECK_FALSE(parse_rates("not json").has_value());
    CHECK_FALSE(parse_rates(R"([{"date":"2026-10-20","base":"EUR"}])").has_value());
    CHECK(parse_rates("[]").value().empty());
}

TEST_CASE("the_fetcher_asks_for_one_provider_and_never_the_blend", "[domain][frankfurter]") {
    stored_transport transport;
    transport.bodies["https://api.frankfurter.dev/v2/providers/ecb/"
                     "rates?from=2026-10-20&to=2026-10-21&base=EUR"] = ecb_rates;
    frankfurter_fetcher sut(transport, "https://api.frankfurter.dev/");

    const auto rates = sut.rates("ECB", "EUR", d(20), d(21));

    REQUIRE(rates.has_value());
    CHECK(rates->size() == 4);
    REQUIRE(transport.asked.size() == 1);
    CHECK(transport.asked.front().contains("/v2/providers/ecb/"));
}

TEST_CASE("a_source_sends_its_demanded_currencies_once_and_then_only_what_is_new",
          "[domain][source_feed]") {
    stored_transport transport;
    const std::string base = "https://api.frankfurter.dev";
    transport.bodies[base + "/v2/providers"] = providers;
    transport.bodies[base + "/v2/providers/ecb/rates?from=2026-10-19&to=2026-10-21&base=EUR"] =
        ecb_rates;
    transport.bodies[base + "/v2/providers/ecb/rates?from=2026-10-22&to=2026-10-22&base=EUR"] =
        R"([{"date":"2026-10-22","base":"EUR","quote":"USD","rate":1.1620}])";
    frankfurter_fetcher fetcher(transport, base);
    source_feed sut({"ecb-frankfurter", "ecb", "ECB"}, fetcher, 2);
    REQUIRE(sut.load_universe().has_value());

    const auto answer = sut.demand({{{"iso4217", "USD", "rate"}, {"iso4217", "JPY", "rate"}}});
    CHECK(answer.accepted.size() == 1);
    REQUIRE(answer.rejected.size() == 1);
    CHECK(answer.rejected.front().id.external_id == "JPY");

    std::vector<ores::marketdata::messaging::adapter_tick> sent;
    const auto publish = [&](const ores::marketdata::messaging::adapter_tick& t) {
        sent.push_back(t);
    };
    REQUIRE(sut.refresh(d(21), publish).value() == 2);
    REQUIRE(sent.size() == 2);
    CHECK(sent[0].external_id == "USD");
    CHECK(sent[0].value == "1.1612");
    CHECK(sent[0].vendor_time == sys_days{d(20)} + hours(12));
    CHECK(sent[1].value == "1.1598");
    CHECK(sent[1].sequence > sent[0].sequence);

    CHECK(sut.refresh(d(22), publish).value() == 1);
    CHECK(sent.back().value == "1.162");
}

TEST_CASE("a_source_announces_its_universe_in_the_publishers_notation", "[domain][source_feed]") {
    stored_transport transport;
    transport.bodies["https://x/v2/providers"] = providers;
    frankfurter_fetcher fetcher(transport, "https://x");
    source_feed sut({"boe-frankfurter", "boe", "BOE"}, fetcher, 30);
    REQUIRE(sut.load_universe().has_value());

    const auto announce = sut.announcement();

    CHECK(announce.adapter_id == "boe-frankfurter");
    CHECK(announce.provider_code == "boe");
    REQUIRE(announce.universe.size() == 2);
    CHECK(announce.universe.front().id_scheme == "iso4217");
    CHECK(announce.universe.front().field == "rate");
}

TEST_CASE("a_rate_against_another_base_than_the_pivot_is_refused", "[domain][source_feed]") {
    stored_transport transport;
    const std::string base = "https://x";
    transport.bodies[base + "/v2/providers"] = providers;
    transport.bodies[base + "/v2/providers/boe/rates?from=2026-10-20&to=2026-10-21&base=GBP"] =
        R"([{"date":"2026-10-20","base":"EUR","quote":"USD","rate":1.1612},)"
        R"({"date":"2026-10-21","base":"GBP","quote":"USD","rate":1.3389}])";
    frankfurter_fetcher fetcher(transport, base);
    source_feed sut({"boe-frankfurter", "boe", "BOE"}, fetcher, 1);
    REQUIRE(sut.load_universe().has_value());
    sut.demand({{{"iso4217", "USD", "rate"}}});

    std::vector<ores::marketdata::messaging::adapter_tick> sent;
    const auto sent_count = sut.refresh(
        d(21), [&](const ores::marketdata::messaging::adapter_tick& t) { sent.push_back(t); });

    REQUIRE(sent_count.value() == 1);
    CHECK(sent.front().value == "1.3389");
    CHECK(sut.misbased() == 1);
}
