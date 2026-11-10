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
#include "ores.marketdata.official/app/official_adapter.hpp"
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace {

using namespace ores::marketdata::official;
using namespace std::chrono;

class named_fetcher final : public domain::fetcher {
public:
    explicit named_fetcher(std::string code)
        : code_(std::move(code)) {}

    std::string_view adapter_code() const override {
        return code_;
    }
    std::string_view id_scheme() const override {
        return "series_code";
    }
    std::string_view field() const override {
        return "value";
    }
    std::expected<std::vector<std::string>, std::string> universe(std::string_view) override {
        return std::vector<std::string>{};
    }
    std::expected<std::vector<domain::published_fixing>, std::string>
    fixings(std::string_view, year_month_day, year_month_day) override {
        return std::vector<domain::published_fixing>{};
    }

private:
    std::string code_;
};

std::vector<domain::hosted_source> one_source(std::string adapter_code) {
    return {{"nyfed-sofr", "nyfed", std::move(adapter_code), "SOFR"}};
}

}

TEST_CASE("a_source_whose_adapter_is_not_hosted_is_refused", "[app][official_adapter]") {
    ores::nats::service::client nats(ores::nats::config::nats_options{});
    named_fetcher frankfurter("frankfurter");

    CHECK_THROWS_AS(app::official_adapter(nats, one_source("nyfed"), {&frankfurter}, 1, {}),
                    std::invalid_argument);
}

TEST_CASE("a_source_is_hosted_by_the_fetcher_of_its_adapter", "[app][official_adapter]") {
    ores::nats::service::client nats(ores::nats::config::nats_options{});
    named_fetcher frankfurter("frankfurter");
    named_fetcher nyfed("nyfed");

    CHECK_NOTHROW(app::official_adapter(nats, one_source("nyfed"), {&frankfurter, &nyfed}, 1, {}));
}

TEST_CASE("two_fetchers_of_one_adapter_are_refused", "[app][official_adapter]") {
    ores::nats::service::client nats(ores::nats::config::nats_options{});
    named_fetcher first("nyfed");
    named_fetcher second("nyfed");

    CHECK_THROWS_AS(app::official_adapter(nats, one_source("nyfed"), {&first, &second}, 1, {}),
                    std::invalid_argument);
}
