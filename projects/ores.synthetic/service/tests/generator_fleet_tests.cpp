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
#include "../src/shared_surfaces.hpp"
#include <atomic>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <functional>
#include <map>
#include <memory>
#include <stdexcept>
#include <string>
#include <thread>
#include <vector>

namespace {

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

// A generator whose start throws, as one does when what it reads is not there yet.
class start_throwing_generator final : public domain::i_feed {
public:
    explicit start_throwing_generator(std::string name)
        : name_(std::move(name)) {}

    static std::atomic<bool>& thrown() {
        static std::atomic<bool> instance{false};
        return instance;
    }

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
        return "failing";
    }
    std::uint64_t publish_count() const override {
        return 0;
    }
    void start() override {
        thrown().store(true);
        throw std::runtime_error("nothing to start from yet");
    }
    void stop() override {}

private:
    std::string name_;
    std::vector<std::string> series_;
};

// A surface that records what is done to it: how many adapters it serves, and whether one was
// attached before it started.
class recording_surface final {
public:
    using point_targets = std::map<std::string, std::string>;

    void attach(const std::string& adapter_id, const std::string&, point_targets) {
        adapters_[adapter_id] = true;
    }
    std::size_t detach(const std::string& adapter_id) {
        adapters_.erase(adapter_id);
        return adapters_.size();
    }
    void start() {
        ++starts;
        attached_at_start = adapters_.size();
    }
    void stop() {
        ++stops;
    }
    std::uint64_t publish_count() const {
        return 0;
    }

    int starts = 0;
    int stops = 0;
    std::size_t attached_at_start = 0;

private:
    std::map<std::string, bool> adapters_;
};

using recording_surfaces = ores::synthetic::service::basic_shared_surfaces<recording_surface>;
using recording_hold = ores::synthetic::service::basic_shared_surface_hold<recording_surface>;

// One offer: the id it answers and the feed, by source name, that it builds.
generator_offer offer_of(const std::string& pair, const std::string& source_name) {
    return {.id = pair_id(pair),
            .make = [source_name] { return std::make_shared<named_generator>(source_name); }};
}

std::shared_ptr<domain::i_feed> feed_named(const std::string& source_name) {
    return std::make_shared<named_generator>(source_name);
}

}

TEST_CASE("the running set names the feeds the demanded runner runs", "[generator_fleet]") {
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

TEST_CASE("a manual run is not reconciled away by the next demand", "[generator_fleet]") {
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

TEST_CASE("a feed that already runs starts no second generator", "[generator_fleet]") {
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

TEST_CASE("a manual start of a feed that already runs starts nothing", "[generator_fleet]") {
    generator_fleet fleet;
    generator_runner runner([] { return std::vector{offer_of("EUR/USD", "synthetic.eurusd")}; },
                            &fleet.running_names());

    runner.apply({pair_id("EUR/USD")});
    CHECK_FALSE(fleet.start_manual("config-1", feed_named("synthetic.eurusd")));
    CHECK(fleet.running_count() == 1);
}

TEST_CASE("a stop by source name reaches a demanded run and a manual one alike",
          "[generator_fleet]") {
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

TEST_CASE("a generator whose start threw is built again by the next demand", "[generator_fleet]") {
    // The first build cannot start, as a surface cannot before its calendars have dates; the
    // second one can.
    auto builds = std::make_shared<std::atomic<int>>(0);
    generator_runner runner([builds] {
        return std::vector{generator_offer{
            .id = pair_id("EUR/USD"), .make = [builds]() -> std::shared_ptr<domain::i_feed> {
                if (builds->fetch_add(1) == 0)
                    return std::make_shared<start_throwing_generator>("synthetic.eurusd");
                return std::make_shared<named_generator>("synthetic.eurusd");
            }}};
    });

    CHECK(runner.apply({pair_id("EUR/USD")}).started == 1);

    // A demand that comes before the start has thrown keeps the generator; the first one after
    // builds it again.
    const auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(5);
    std::size_t restarted = 0;
    while (restarted == 0 && std::chrono::steady_clock::now() < deadline) {
        restarted = runner.apply({pair_id("EUR/USD")}).started;
        if (restarted == 0)
            std::this_thread::sleep_for(std::chrono::milliseconds(1));
    }
    CHECK(start_throwing_generator::thrown().load());
    CHECK(restarted == 1);
    CHECK(builds->load() == 2);
    CHECK(runner.running_count() == 1);

    // A generator that runs is kept, not built a third time.
    CHECK(runner.apply({pair_id("EUR/USD")}).started == 0);
    CHECK(builds->load() == 2);
}

TEST_CASE("two adapters on one surface share one latent path", "[shared_surfaces]") {
    recording_surfaces surfaces;
    int builds = 0;
    std::shared_ptr<recording_surface> built;
    const auto make = [&builds, &built] {
        ++builds;
        built = std::make_shared<recording_surface>();
        return built;
    };
    recording_hold bloomberg(
        surfaces, "feed-1", "bloomberg-a", "c-bbg", "synthetic.vol.eurusd.bloomberg", {}, make);
    recording_hold ice(surfaces, "feed-1", "ice-a", "c-ice", "synthetic.vol.eurusd.ice", {}, make);

    bloomberg.start();
    ice.start();
    CHECK(builds == 1);
    REQUIRE(built);
    CHECK(built->starts == 1);
    // The first adapter was attached before the surface started, so no tick went out unrouted.
    CHECK(built->attached_at_start == 1);
    CHECK(surfaces.running_count() == 1);

    // One adapter going leaves the surface quoting for the other.
    bloomberg.stop();
    CHECK(built->stops == 0);
    CHECK(surfaces.running_count() == 1);

    ice.stop();
    CHECK(built->stops == 1);
    CHECK(surfaces.running_count() == 0);

    // A hold stopped twice, or never started, detaches nothing.
    ice.stop();
    CHECK(built->stops == 1);
}

TEST_CASE("a surface built again after its last adapter left is a new path", "[shared_surfaces]") {
    recording_surfaces surfaces;
    int builds = 0;
    const auto make = [&builds] {
        ++builds;
        return std::make_shared<recording_surface>();
    };
    {
        recording_hold first(surfaces, "feed-1", "bloomberg-a", "c-bbg", "a", {}, make);
        first.start();
    }
    CHECK(surfaces.running_count() == 0);
    recording_hold again(surfaces, "feed-1", "bloomberg-a", "c-bbg", "a", {}, make);
    again.start();
    CHECK(builds == 2);
    CHECK(surfaces.running_count() == 1);
}
