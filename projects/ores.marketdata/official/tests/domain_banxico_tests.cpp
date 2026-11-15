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
#include "ores.marketdata.official/domain/banxico.hpp"
#include "ores.marketdata.official/domain/secret.hpp"
#include <catch2/catch_test_macros.hpp>
#include <deque>
#include <string>
#include <utility>
#include <vector>

namespace {

using namespace ores::marketdata::official::domain;
using namespace std::chrono;
namespace net = ores::http::client::net;

const std::string fondeo_key = "SF331451";
const std::string token = "0123456789abcdef";

/// The TIIE de Fondeo as the SIE API sent it on 2026-11-11, for the 6th to the 10th of November.
const std::string fondeo_json =
    R"({"bmx":{"series":[{"idSerie":"SF331451","titulo":"TIIE de Fondeo a Un Día Hábil Bancario, Mediana ponderada por volumen","datos":[{"fecha":"06/11/2026","dato":"6.2600"},{"fecha":"09/11/2026","dato":"6.2600"},{"fecha":"10/11/2026","dato":"6.2600"}]}]}})";

/// What the API sent on 2026-11-11 for a window of a weekend, which holds no value.
const std::string weekend_json =
    R"({"bmx":{"series":[{"idSerie":"SF331451","titulo":"TIIE de Fondeo a Un Día Hábil Bancario, Mediana ponderada por volumen"}]}})";

/// A transport that gives its stored answers in order, and records each address asked.
class banxico_transport final : public net::transport {
public:
    std::deque<net::response> answers;
    std::vector<std::string> asked;

    std::expected<net::response, net::failure> get(std::string_view url,
                                                   const net::request_limits&) override {
        asked.emplace_back(url);
        if (answers.empty())
            return std::unexpected(net::failure{net::failure_kind::network, "no stored answer"});
        auto answer = std::move(answers.front());
        answers.pop_front();
        return answer;
    }
};

constexpr year_month_day banxico_day(int y, int m, int d) {
    return year_month_day{year{y}, month{static_cast<unsigned>(m)}, day{static_cast<unsigned>(d)}};
}

}

TEST_CASE("a_fondeo_answer_reads_as_decimal_fixings_of_its_key", "[domain][banxico]") {
    const auto fixings = parse_banxico_series(fondeo_json, fondeo_key);

    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 3);
    CHECK((*fixings)[0].date == banxico_day(2026, 11, 6));
    CHECK((*fixings)[0].external_id == fondeo_key);
    CHECK((*fixings)[0].value == "0.0626");
    CHECK((*fixings)[2].date == banxico_day(2026, 11, 10));
}

TEST_CASE("a_banxico_series_with_no_datos_reads_as_no_fixing", "[domain][banxico]") {
    const auto fixings = parse_banxico_series(weekend_json, fondeo_key);

    REQUIRE(fixings.has_value());
    CHECK(fixings->empty());
}

TEST_CASE("a_banxico_value_of_n_e_is_skipped", "[domain][banxico]") {
    const auto fixings = parse_banxico_series(
        R"({"bmx":{"series":[{"idSerie":"SF331451","datos":[{"fecha":"06/11/2026","dato":"N/E"},{"fecha":"09/11/2026","dato":"6.26"}]}]}})",
        fondeo_key);

    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 1);
    CHECK((*fixings)[0].date == banxico_day(2026, 11, 9));
}

TEST_CASE("a_malformed_banxico_answer_is_an_error_and_not_a_fixing", "[domain][banxico]") {
    CHECK_FALSE(parse_banxico_series("Token inválido", fondeo_key).has_value());
    CHECK_FALSE(parse_banxico_series(R"({"bmx":{"series":[]}})", fondeo_key).has_value());
    CHECK_FALSE(parse_banxico_series(weekend_json, "SF43718").has_value());
    CHECK_FALSE(
        parse_banxico_series(
            R"({"bmx":{"series":[{"idSerie":"SF331451","datos":[{"fecha":"2026-11-06","dato":"6.26"}]}]}})",
            fondeo_key)
            .has_value());
    CHECK_FALSE(
        parse_banxico_series(
            R"({"bmx":{"series":[{"idSerie":"SF331451","datos":[{"fecha":"06/11/2026","dato":6.26}]}]}})",
            fondeo_key)
            .has_value());
    CHECK_FALSE(
        parse_banxico_series(
            R"({"bmx":{"series":[{"idSerie":"SF331451","datos":[{"fecha":"06/11/2026","dato":"6,26"}]}]}})",
            fondeo_key)
            .has_value());
}

TEST_CASE("the_banxico_fetcher_reads_the_window_of_its_series_with_its_token",
          "[domain][banxico]") {
    banxico_transport transport;
    transport.answers.push_back({200, "application/json", fondeo_json});
    banxico_fetcher fetcher(transport, "https://www.banxico.org.mx/", token);

    const auto universe = fetcher.universe(fondeo_key);
    const auto fixings =
        fetcher.fixings(fondeo_key, banxico_day(2026, 11, 9), banxico_day(2026, 11, 10));

    REQUIRE(universe.has_value());
    CHECK(*universe == std::vector<std::string>{fondeo_key});
    REQUIRE(fixings.has_value());
    CHECK(fixings->size() == 2);
    CHECK(transport.asked ==
          std::vector<std::string>{"https://www.banxico.org.mx/SieAPIRest/service/v1/series/"
                                   "SF331451/datos/2026-11-09/2026-11-10?token=" +
                                   token});
}

TEST_CASE("a_refused_banxico_read_is_an_error_that_does_not_hold_the_token", "[domain][banxico]") {
    banxico_transport transport;
    transport.answers.push_back({302, "text/html", ""});
    banxico_fetcher fetcher(transport, "https://www.banxico.org.mx", token);

    const auto fixings =
        fetcher.fixings("SF999999", banxico_day(2026, 11, 6), banxico_day(2026, 11, 10));

    REQUIRE_FALSE(fixings.has_value());
    CHECK(fixings.error().find("302") != std::string::npos);
    CHECK(fixings.error().find(token) == std::string::npos);
}

TEST_CASE("a_banxico_fetcher_without_a_token_reads_nothing_and_names_the_variable",
          "[domain][banxico]") {
    banxico_transport transport;
    banxico_fetcher fetcher(
        transport,
        "https://www.banxico.org.mx",
        resolve_secret("env:ORES_MARKETDATA_BANXICO_API_KEY",
                       [](const char*) -> std::optional<std::string> { return std::nullopt; }));

    const auto universe = fetcher.universe(fondeo_key);

    REQUIRE_FALSE(universe.has_value());
    CHECK(universe.error().find("ORES_MARKETDATA_BANXICO_API_KEY") != std::string::npos);
    const auto bad_id = fetcher.universe("SF331451/x");
    REQUIRE_FALSE(bad_id.has_value());
    CHECK(bad_id.error().find("is not a series id") != std::string::npos);
    CHECK(transport.asked.empty());
}

TEST_CASE("a_banxico_key_is_a_series_id", "[domain][banxico]") {
    banxico_transport transport;
    banxico_fetcher fetcher(transport, "https://www.banxico.org.mx", token);

    CHECK_FALSE(fetcher.universe("").has_value());
    CHECK_FALSE(fetcher.universe("SF").has_value());
    CHECK_FALSE(fetcher.universe("331451").has_value());
    CHECK_FALSE(fetcher.universe("sf331451").has_value());
    CHECK_FALSE(fetcher.universe("SF331451/../x").has_value());
    CHECK_FALSE(fetcher.universe("SF331451?token=x").has_value());
    CHECK(transport.asked.empty());
}

TEST_CASE("an_env_ref_resolves_to_the_value_of_its_variable", "[domain][secret]") {
    const auto lookup = [](const char* name) -> std::optional<std::string> {
        return std::string_view(name) == "ORES_TEST_KEY" ? std::optional<std::string>{"s3cret"} :
                                                           std::nullopt;
    };

    const auto found = resolve_secret("env:ORES_TEST_KEY", lookup);

    REQUIRE(found.has_value());
    CHECK(*found == "s3cret");
}

TEST_CASE("an_unset_empty_or_malformed_ref_is_an_error_without_a_value", "[domain][secret]") {
    const auto empty = [](const char*) -> std::optional<std::string> {
        return std::string{};
    };
    const auto unset = [](const char*) -> std::optional<std::string> {
        return std::nullopt;
    };

    CHECK_FALSE(resolve_secret("env:ORES_TEST_KEY", empty).has_value());
    CHECK_FALSE(resolve_secret("env:ORES_TEST_KEY", unset).has_value());
    CHECK_FALSE(resolve_secret("env:", unset).has_value());
    CHECK_FALSE(resolve_secret("vault:credentials/banxico", unset).has_value());
    CHECK_FALSE(resolve_secret("", unset).has_value());
}
