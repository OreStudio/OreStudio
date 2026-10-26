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
#include "../src/generator_fleet.hpp"
#include <atomic>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <functional>
#include <memory>
#include <string>
#include <thread>
#include <vector>

namespace {

const std::string tags("[generator_fleet]");

namespace messaging = ores::marketdata::messaging;
namespace domain = ores::marketdata::domain;
using ores::synthetic::service::generator_fleet;
using ores::synthetic::service::generator_offer;
using ores::synthetic::service::generator_runner;
using ores::synthetic::service::running_set;

// The vendor id of one pair of a fictional provider.
messaging::vendor_id pair_id(const std::string& pair) {
    return {.id_scheme = "demo_key", .external_id = pair, .field = "value"};
}

// A generator of one named feed that runs until it is stopped.
class named_generator final : public domain::i_feed {
public:
    explicit named_generator(std::string name)
        : name_(std::move(name)) {}

    const std::string& source_name() const override {
        return name_;
    }
    const std::vector<std::string>& series_uris() const override {
        return series_;
    }
    const std::string& role() const override {
        return name_;
    }
    const std::string& nats_subject() const override {
        return name_;
    }
    std::string_view kind() const override {
        return "named";
    }
    std::uint64_t publish_count() const override {
        return 0;
    }
    void start() override {
        while (!stop_.load())
            std::this_thread::sleep_for(std::chrono::milliseconds(1));
    }
    void stop() override {
        stop_.store(true);
    }

private:
    std::string name_;
    std::vector<std::string> series_;
    std::atomic<bool> stop_{false};
};

// One offer: the id it answers and the feed, by source name, that it builds.
generator_offer offer_of(const std::string& pair, const std::string& source_name) {
    return {.id = pair_id(pair), .make = [source_name] {
                return std::make_shared<named_generator>(source_name);
            }};
}

std::shared_ptr<domain::i_feed> feed_named(const std::string& source_name) {
    return std::make_shared<named_generator>(source_name);
}

}

TEST_CASE("the running set names the feeds the demanded runner runs", tags) {
    running_set names;
    generator_runner runner([] { return std::vector{offer_of("EUR/USD", "synthetic.eurusd")}; },
                            &names);

    const auto ran = runner.apply({pair_id("EUR/USD")});
    CHECK(ran.started == 1);
    CHECK(names.names() == std::vector<std::string>{"synthetic.eurusd"});

    // A demand that drops the feed releases its name, so no screen reads it as running.
    runner.apply({});
    CHECK(names.names().empty());
}

TEST_CASE("a manual run is not reconciled away by the next demand", tags) {
    generator_fleet fleet;
    generator_runner runner([] { return std::vector{offer_of("EUR/USD", "synthetic.eurusd")}; },
                            &fleet.running_names());

    CHECK(fleet.start_manual("config-1", feed_named("synthetic.manual")));
    CHECK(fleet.running_count() == 1);

    // The demand of an adapter names the pair it serves and nothing of the operator's feed.
    runner.apply({pair_id("EUR/USD")});
    CHECK(fleet.running_source_names() ==
          std::vector<std::string>({"synthetic.eurusd", "synthetic.manual"}));

    runner.apply({});
    CHECK(fleet.running_source_names() == std::vector<std::string>{"synthetic.manual"});
}

TEST_CASE("a feed that already runs starts no second generator", tags) {
    generator_fleet fleet;
    generator_runner runner([] { return std::vector{offer_of("EUR/USD", "synthetic.eurusd")}; },
                            &fleet.running_names());

    // The operator starts the feed the adapter demands, then the demand arrives.
    CHECK(fleet.start_manual("config-1", feed_named("synthetic.eurusd")));
    const auto demanded = runner.apply({pair_id("EUR/USD")});

    CHECK(demanded.accepted.size() == 1);
    CHECK(demanded.started == 0);
    CHECK(runner.running_count() == 0);
    CHECK(fleet.running_source_names() == std::vector<std::string>{"synthetic.eurusd"});
}

TEST_CASE("a manual start of a feed that already runs starts nothing", tags) {
    generator_fleet fleet;
    generator_runner runner([] { return std::vector{offer_of("EUR/USD", "synthetic.eurusd")}; },
                            &fleet.running_names());

    runner.apply({pair_id("EUR/USD")});
    CHECK_FALSE(fleet.start_manual("config-1", feed_named("synthetic.eurusd")));
    CHECK(fleet.running_count() == 1);
}

TEST_CASE("a stop by source name reaches a demanded run and a manual one alike", tags) {
    generator_fleet fleet;
    generator_runner runner([] { return std::vector{offer_of("EUR/USD", "synthetic.eurusd")}; },
                            &fleet.running_names());
    fleet.attach(runner);

    runner.apply({pair_id("EUR/USD")});
    CHECK(fleet.start_manual("config-1", feed_named("synthetic.manual")));

    CHECK(fleet.stop_source("synthetic.eurusd") == 1);
    CHECK(fleet.stop_source("synthetic.manual") == 1);
    CHECK(fleet.running_source_names().empty());
    CHECK(fleet.stop_source("synthetic.eurusd") == 0);

    // The next demand starts the demanded feed again: a stop is not a config change.
    runner.apply({pair_id("EUR/USD")});
    CHECK(fleet.running_source_names() == std::vector<std::string>{"synthetic.eurusd"});

    fleet.detach(runner);
}
