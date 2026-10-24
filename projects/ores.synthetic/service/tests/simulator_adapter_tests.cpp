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
#include "../src/generator_runner.hpp"
#include "../src/simulator_adapter.hpp"
#include "../src/simulator_offers.hpp"
#include "ores.analytics.quant/domain/i_stochastic_process.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.marketdata.api/domain/adapter_subjects.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.synthetic.api/feeds/fx_spot_feed.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/test_database_manager.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <atomic>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <condition_variable>
#include <memory>
#include <mutex>
#include <set>
#include <span>
#include <stdexcept>
#include <string>
#include <thread>
#include <vector>

namespace {

const std::string tags("[simulator_adapter]");

namespace messaging = ores::marketdata::messaging;
namespace domain = ores::marketdata::domain;
using ores::synthetic::service::generator_offer;
using ores::synthetic::service::generator_runner;
using ores::synthetic::service::simulator_adapter;
using ores::synthetic::service::simulator_adapter_timing;

// The Bloomberg id of a pair written as "EUR/USD".
messaging::vendor_id bbg_id(const std::string& pair) {
    std::string letters;
    for (const char c : pair)
        if (c != '/')
            letters += c;
    return {.id_scheme = "bbg_ticker", .external_id = letters + " Curncy", .field = "PX_LAST"};
}

// A generator that publishes nothing and runs until it is stopped. The counters are shared, so a
// test sees a start and a stop after the runner has destroyed the generator.
struct generator_counters {
    std::atomic<int> built{0};
    std::atomic<int> started{0};
    std::atomic<int> stopped{0};
};

class idle_generator final : public domain::IFeed {
public:
    explicit idle_generator(std::shared_ptr<generator_counters> counters)
        : counters_(std::move(counters)) {}

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
        return "idle";
    }
    std::uint64_t publish_count() const override {
        return 0;
    }
    void start() override {
        ++counters_->started;
        while (!stop_.load())
            std::this_thread::sleep_for(std::chrono::milliseconds(1));
    }
    void stop() override {
        ++counters_->stopped;
        stop_.store(true);
    }

private:
    std::shared_ptr<generator_counters> counters_;
    std::string name_ = "idle";
    std::vector<std::string> series_;
    std::atomic<bool> stop_{false};
};

// A generator whose tick loop fails at once, as a build that passed and a start that did not.
class failing_generator final : public domain::IFeed {
public:
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
        throw std::runtime_error("the process cannot step");
    }
    void stop() override {}

private:
    std::string name_ = "failing";
    std::vector<std::string> series_;
};

generator_offer idle_offer(const std::string& pair, std::shared_ptr<generator_counters> counters) {
    return {.id = bbg_id(pair), .make = [counters] {
                ++counters->built;
                return std::make_shared<idle_generator>(counters);
            }};
}

bool eventually(const std::function<bool()>& done) {
    for (int i = 0; i < 400; ++i) {
        if (done())
            return true;
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }
    return done();
}

class flat_process final : public ores::analytics::quant::domain::IStochasticProcess {
public:
    double next() override {
        return 1.25;
    }
    double current() const override {
        return 1.25;
    }
};

// What an observer on a subject saw.
template <typename T>
class inbox {
public:
    inbox(ores::nats::service::client& nats, const std::string& subject) {
        sub_ = nats.subscribe(subject, [this](ores::nats::message msg) {
            auto decoded = ores::nats::default_wire_codec().decode<T>(msg.data);
            if (!decoded)
                return;
            std::lock_guard lock(mu_);
            seen_.push_back(*decoded);
            cv_.notify_all();
        });
    }

    bool wait_for_count(std::size_t n) {
        std::unique_lock lock(mu_);
        return cv_.wait_for(lock, std::chrono::seconds(5), [&] { return seen_.size() >= n; });
    }

    T first() {
        std::lock_guard lock(mu_);
        return seen_.front();
    }

private:
    std::mutex mu_;
    std::condition_variable cv_;
    std::vector<T> seen_;
    std::optional<ores::nats::service::subscription> sub_;
};

}

TEST_CASE("the runner starts one generator for each id in the demand", tags + "[runner]") {
    auto counters = std::make_shared<generator_counters>();
    generator_runner runner([&] {
        return std::vector{idle_offer("EUR/USD", counters), idle_offer("GBP/USD", counters)};
    });

    const auto out = runner.apply({bbg_id("EUR/USD"), bbg_id("GBP/USD")});

    CHECK(out.accepted.size() == 2);
    CHECK(out.rejected.empty());
    CHECK(out.started == 2);
    CHECK(runner.running_count() == 2);
    CHECK(eventually([&] { return counters->started == 2; }));
}

TEST_CASE("the runner keeps a running generator when the next demand names it again",
          tags + "[runner]") {
    auto counters = std::make_shared<generator_counters>();
    generator_runner runner([&] { return std::vector{idle_offer("EUR/USD", counters)}; });

    runner.apply({bbg_id("EUR/USD")});
    const auto again = runner.apply({bbg_id("EUR/USD")});

    CHECK(again.accepted.size() == 1);
    CHECK(again.started == 0);
    CHECK(counters->built == 1);
    CHECK(runner.running_count() == 1);
}

TEST_CASE("the runner stops a generator that the next demand drops", tags + "[runner]") {
    auto counters = std::make_shared<generator_counters>();
    generator_runner runner([&] {
        return std::vector{idle_offer("EUR/USD", counters), idle_offer("GBP/USD", counters)};
    });
    runner.apply({bbg_id("EUR/USD"), bbg_id("GBP/USD")});

    const auto out = runner.apply({bbg_id("GBP/USD")});

    CHECK(out.stopped == 1);
    CHECK(runner.running_count() == 1);
    CHECK(counters->stopped == 1);

    const auto none = runner.apply({});
    CHECK(none.stopped == 1);
    CHECK(runner.running_count() == 0);
    CHECK(counters->stopped == 2);
}

TEST_CASE("the runner rejects an id with no generator and still serves the others",
          tags + "[runner]") {
    auto counters = std::make_shared<generator_counters>();
    generator_runner runner([&] { return std::vector{idle_offer("EUR/USD", counters)}; });

    const auto out = runner.apply({bbg_id("EUR/USD"), bbg_id("XXX/YYY")});

    REQUIRE(out.accepted.size() == 1);
    CHECK(out.accepted.front().external_id == "EURUSD Curncy");
    REQUIRE(out.rejected.size() == 1);
    CHECK(out.rejected.front().id.external_id == "XXXYYY Curncy");
    CHECK(out.rejected.front().reason == "unknown_id");
}

TEST_CASE("the runner refuses a second generator for one vendor instrument", tags + "[runner]") {
    auto first = std::make_shared<generator_counters>();
    auto second = std::make_shared<generator_counters>();
    generator_runner runner(
        [&] { return std::vector{idle_offer("EUR/USD", first), idle_offer("EUR/USD", second)}; });

    CHECK(runner.universe().size() == 1);
    const auto out = runner.apply({bbg_id("EUR/USD")});

    CHECK(out.accepted.size() == 1);
    CHECK(first->built == 1);
    CHECK(second->built == 0);
    CHECK(runner.running_count() == 1);
}

TEST_CASE("the runner counts an id once when the demand repeats it", tags + "[runner]") {
    auto counters = std::make_shared<generator_counters>();
    generator_runner runner([&] { return std::vector{idle_offer("EUR/USD", counters)}; });

    const auto out = runner.apply({bbg_id("EUR/USD"), bbg_id("EUR/USD")});

    CHECK(out.accepted.size() == 1);
    CHECK(counters->built == 1);
}

TEST_CASE("the runner rejects an id whose generator cannot be built", tags + "[runner]") {
    auto counters = std::make_shared<generator_counters>();
    generator_runner runner([&] {
        return std::vector{generator_offer{.id = bbg_id("EUR/USD"),
                                           .make = []() -> std::shared_ptr<domain::IFeed> {
                                               throw std::runtime_error("no components");
                                           }},
                           idle_offer("GBP/USD", counters)};
    });

    const auto out = runner.apply({bbg_id("EUR/USD"), bbg_id("GBP/USD")});

    REQUIRE(out.rejected.size() == 1);
    CHECK(out.rejected.front().reason == "generator_failed");
    CHECK(out.accepted.size() == 1);
    CHECK(runner.running_count() == 1);
}

TEST_CASE("the runner survives a generator whose start throws", tags + "[runner]") {
    generator_runner runner([] {
        return std::vector{generator_offer{.id = bbg_id("EUR/USD"), .make = [] {
                                               return std::make_shared<failing_generator>();
                                           }}};
    });

    const auto out = runner.apply({bbg_id("EUR/USD")});
    std::this_thread::sleep_for(std::chrono::milliseconds(50));
    const auto none = runner.apply({});

    CHECK(out.accepted.size() == 1);
    CHECK(none.stopped == 1);
}

TEST_CASE("the adapter announces, answers the demand and sends ticks and heartbeats",
          tags + "[adapter]") {
    ores::nats::service::client nats(ores::testing::make_nats_options());
    nats.connect();

    const auto adapter_id =
        "test-sim-" + boost::uuids::to_string(boost::uuids::random_generator()());
    const auto target = bbg_id("EUR/USD");

    // The market data service, reduced to its answer to an announce.
    std::mutex mu;
    std::vector<messaging::adapter_announce_request> announces;
    auto announce_sub = nats.subscribe(domain::adapter_announce_subject(), [&](auto msg) {
        auto request =
            ores::nats::default_wire_codec().decode<messaging::adapter_announce_request>(msg.data);
        if (!request || request->adapter_id != adapter_id)
            return;
        {
            std::lock_guard lock(mu);
            announces.push_back(*request);
        }
        messaging::adapter_announce_response response;
        response.success = true;
        response.served_count = request->universe.size();
        ores::service::messaging::reply(nats, msg, response);
    });

    // The generator is the real FX feed with the adapter target.
    generator_runner runner([&] {
        return std::vector{generator_offer{
            .id = target, .make = [&] {
                return std::make_shared<ores::synthetic::feed::fx_spot_feed>(
                    nats,
                    "oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD",
                    "test.eur_usd",
                    "unused",
                    std::make_unique<flat_process>(),
                    36000000.0,
                    ores::synthetic::feed::adapter_target{
                        .id = target,
                        .subject =
                            domain::adapter_ingress_subject(adapter_id, target.external_id)});
            }}};
    });

    inbox<messaging::adapter_tick> ticks(
        nats, domain::adapter_ingress_subject(adapter_id, target.external_id));
    inbox<messaging::adapter_heartbeat> beats(nats, domain::adapter_heartbeat_subject(adapter_id));

    simulator_adapter adapter(
        nats,
        runner,
        adapter_id,
        "bloomberg",
        simulator_adapter_timing{.heartbeat_interval = std::chrono::milliseconds(20),
                                 .reannounce_interval = std::chrono::seconds(60),
                                 .retry_interval = std::chrono::milliseconds(50),
                                 .reply_timeout = std::chrono::milliseconds(500)});
    adapter.start();

    REQUIRE(eventually([&] { return adapter.announced(); }));
    {
        std::lock_guard lock(mu);
        REQUIRE(announces.size() == 1);
        CHECK(announces.front().provider_code == "bloomberg");
        REQUIRE(announces.front().universe.size() == 1);
        CHECK(announces.front().universe.front().external_id == target.external_id);
    }

    REQUIRE(beats.wait_for_count(2));
    CHECK(beats.first().adapter_id == adapter_id);

    messaging::adapter_demand_request demand;
    demand.ids = {target, bbg_id("XXX/YYY")};
    const auto bytes = ores::nats::default_wire_codec().encode(demand);
    const auto reply = nats.request_sync(domain::adapter_demand_subject(adapter_id),
                                         std::span<const std::byte>(bytes),
                                         {},
                                         std::chrono::seconds(2));
    const auto response =
        ores::nats::default_wire_codec().decode<messaging::adapter_demand_response>(reply.data);
    REQUIRE(response);
    REQUIRE(response->accepted.size() == 1);
    CHECK(response->accepted.front().external_id == target.external_id);
    REQUIRE(response->rejected.size() == 1);
    CHECK(response->rejected.front().reason == "unknown_id");

    REQUIRE(ticks.wait_for_count(1));
    const auto tick = ticks.first();
    CHECK(tick.id_scheme == "bbg_ticker");
    CHECK(tick.external_id == "EURUSD Curncy");
    CHECK(tick.field == "PX_LAST");
    CHECK(tick.value == "1.25");

    const auto empty = ores::nats::default_wire_codec().encode(messaging::adapter_demand_request{});
    const auto cleared = nats.request_sync(domain::adapter_demand_subject(adapter_id),
                                           std::span<const std::byte>(empty),
                                           {},
                                           std::chrono::seconds(2));
    CHECK_FALSE(cleared.data.empty());
    CHECK(runner.running_count() == 0);

    adapter.stop();
}

TEST_CASE("the adapter announces again until the service answers", tags + "[adapter]") {
    ores::nats::service::client nats(ores::testing::make_nats_options());
    nats.connect();
    const auto adapter_id =
        "test-sim-" + boost::uuids::to_string(boost::uuids::random_generator()());

    generator_runner runner([] { return std::vector<generator_offer>{}; });
    simulator_adapter adapter(
        nats,
        runner,
        adapter_id,
        "bloomberg",
        simulator_adapter_timing{.heartbeat_interval = std::chrono::milliseconds(20),
                                 .reannounce_interval = std::chrono::seconds(60),
                                 .retry_interval = std::chrono::milliseconds(50),
                                 .reply_timeout = std::chrono::milliseconds(100)});
    adapter.start();
    std::this_thread::sleep_for(std::chrono::milliseconds(300));
    CHECK_FALSE(adapter.announced());

    auto announce_sub = nats.subscribe(domain::adapter_announce_subject(), [&](auto msg) {
        auto request =
            ores::nats::default_wire_codec().decode<messaging::adapter_announce_request>(msg.data);
        if (!request || request->adapter_id != adapter_id)
            return;
        messaging::adapter_announce_response response;
        response.success = true;
        ores::service::messaging::reply(nats, msg, response);
    });

    CHECK(eventually([&] { return adapter.announced(); }));
    adapter.stop();
}

namespace {

namespace svc = ores::synthetic::service;

const auto system_container =
    boost::uuids::string_generator()("aaaaaaaa-0000-0000-0000-000000000001");
const auto party_container =
    boost::uuids::string_generator()("bbbbbbbb-0000-0000-0000-000000000002");
const auto other_provider_container =
    boost::uuids::string_generator()("dddddddd-0000-0000-0000-000000000004");

svc::feed_kind_candidate fx_candidate(const std::string& id,
                                      const boost::uuids::uuid& container,
                                      const std::string& pair,
                                      bool enabled = true) {
    svc::feed_kind_candidate c;
    c.feed_config_id = id;
    c.container_id = container;
    c.source_name = "test." + id;
    c.display_name = pair;
    c.enabled = enabled;
    if (!pair.empty())
        if (!pair.empty())
            c.fx_pair = std::make_pair(pair.substr(0, 3), pair.substr(4, 3));
    c.build_input = []() {
        return svc::feed_build_outcome{.input =
                                           ores::synthetic::feed::feed_build_input{
                                               ores::synthetic::feed::fx_spot_feed_build_input{}},
                                       .skip_reason = {}};
    };
    return c;
}

}

TEST_CASE("the offers are the startable system configs of the provider that have a vendor id",
          tags + "[offers]") {
    std::vector<ores::synthetic::feed::adapter_target> built;
    ores::synthetic::feed::feed_factory factory;
    factory.register_kind(
        std::string(ores::synthetic::feed::fx_spot_feed_kind),
        [&built](const auto&, const auto& input) -> std::shared_ptr<domain::IFeed> {
            built.push_back(
                *std::get<ores::synthetic::feed::fx_spot_feed_build_input>(input).adapter);
            return std::make_shared<idle_generator>(std::make_shared<generator_counters>());
        });

    const auto containers = [] {
        std::vector<ores::synthetic::domain::market_data_generation_config> out;
        ores::synthetic::domain::market_data_generation_config sys;
        sys.id = system_container;
        sys.enabled = true;
        sys.provider_code = "bloomberg";
        out.push_back(sys);
        ores::synthetic::domain::market_data_generation_config tenant_owned;
        tenant_owned.id = party_container;
        tenant_owned.enabled = true;
        tenant_owned.provider_code = "bloomberg";
        tenant_owned.tenant_id =
            ores::utility::uuid::tenant_id::from_uuid(
                boost::uuids::string_generator()("cccccccc-0000-0000-0000-000000000003"))
                .value();
        out.push_back(tenant_owned);
        ores::synthetic::domain::market_data_generation_config other_provider;
        other_provider.id = other_provider_container;
        other_provider.enabled = true;
        other_provider.provider_code = "lseg";
        out.push_back(other_provider);
        return out;
    };
    svc::feed_kind_registry registry(svc::feed_kind_registry::deps{
        .factory = &factory,
        .folder_subtree = [](const auto&, const auto&) { return std::set<boost::uuids::uuid>{}; },
        .container_by_id =
            [](const auto&, const auto&) {
                return std::optional<ores::synthetic::domain::market_data_generation_config>{};
            },
        .containers =
            [containers](const auto&) {
                return containers();
            }});

    svc::feed_kind kind;
    kind.kind = std::string(ores::synthetic::feed::fx_spot_feed_kind);
    kind.config_permission = "synthetic::fx_spot_generation_configs:read";
    kind.candidates = [](const auto&, const std::string&) {
        return std::vector{fx_candidate("1", system_container, "EUR/USD"),
                           fx_candidate("2", system_container, ""),
                           fx_candidate("3", party_container, "GBP/USD"),
                           fx_candidate("4", system_container, "USD/JPY", false),
                           fx_candidate("5", system_container, "EUR/USD"),
                           fx_candidate("6", other_provider_container, "USD/CHF")};
    };
    kind.simulate_subject = "ores.test.fx.simulate";
    kind.decode_simulate = [](const auto&) {
        return std::optional<svc::feed_simulate_envelope>{};
    };
    kind.reply_simulate = [](auto&, const auto&, const auto&) {
    };
    registry.register_kind(std::move(kind));

    const auto opts = ores::testing::test_database_manager::make_database_options();
    const auto ctx = ores::database::context_factory::make_context(
        ores::database::context_factory::configuration{.database_options = opts,
                                                       .pool_size = 1,
                                                       .num_attempts = 1,
                                                       .wait_time_in_seconds = 1,
                                                       .service_account = opts.user});
    ores::nats::service::client nats(ores::testing::make_nats_options());
    ores::nats::service::nats_client auth_nats(nats, [](bool) { return std::string(); });
    const ores::synthetic::feed::feed_build_context build{nats, auth_nats};

    const auto offers =
        svc::system_generator_offers(registry, ctx, build, "bloomberg-a", "bloomberg");

    REQUIRE(offers.size() == 2);
    CHECK(offers[0].id.external_id == "EURUSD Curncy");
    CHECK(offers[1].id.external_id == "EURUSD Curncy");

    // The runner keeps the first of the two and builds one generator.
    generator_runner runner([&] {
        return svc::system_generator_offers(registry, ctx, build, "bloomberg-a", "bloomberg");
    });
    CHECK(runner.universe().size() == 1);
    runner.apply({bbg_id("EUR/USD")});
    REQUIRE(built.size() == 1);
    CHECK(built.front().id.external_id == "EURUSD Curncy");
    CHECK(built.front().subject == domain::adapter_ingress_subject("bloomberg-a", "EURUSD Curncy"));
}
