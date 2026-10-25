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
#include "ores.synthetic.service/app/application.hpp"
#include "../feed_controller.hpp"
#include "../feed_kind_registry.hpp"
#include "../generator_runner.hpp"
#include "../registrar.hpp"
#include "../simulator_adapter.hpp"
#include "../simulator_offers.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.eventing.api/service/event_bus.hpp"
#include "ores.eventing.core/service/postgres_event_source.hpp"
#include "ores.iam.client/client/service_token_provider.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.api/domain/tick_subjects.hpp"
#include "ores.marketdata.client/market_data_client.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.platform/concurrency/stoppable_thread.hpp"
#include "ores.service/service/domain_service_runner.hpp"
#include "ores.service/service/heartbeat_publisher.hpp"
#include "ores.synthetic.api/domain/step_subjects.hpp"
#include "ores.synthetic.api/feeds/feed_factory.hpp"
#include "ores.synthetic.core/messaging/registrar.hpp"
#include "ores.synthetic.service/app/application_exception.hpp"
#include "ores.synthetic.service/messaging/event_registrar.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/version/version.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <map>
#include <memory>
#include <rfl/json.hpp>
#include <set>
#include <span>
#include <thread>
#include <vector>

namespace ores::synthetic::service::app {

using namespace ores::logging;
namespace ev = ores::eventing;

namespace {
constexpr std::string_view service_name = "ores.synthetic.service";
constexpr std::string_view service_version = ORES_VERSION;

// An adapter serves one source of the catalogue. Its id is the name of the source, and the provider
// of the source is the one whose notation its ticks use.
struct simulated_adapter final {
    std::unique_ptr<generator_runner> runner;
    std::unique_ptr<simulator_adapter> adapter;
};

// The simulator serves each source of the catalogue whose kind is simulated, one adapter for each.
// The simulator is the implementation of that kind, so it is the one place that reads the kind.
// The catalogue belongs to marketdata, so the sources come through its client.
std::vector<simulated_adapter>
make_adapters(const std::vector<ores::marketdata::domain::market_data_source>& sources,
              const ores::database::context& db_ctx,
              ores::nats::service::client& nats,
              const feed_kind_registry& registry,
              const ores::synthetic::feed::feed_build_context& build,
              const std::shared_ptr<ores::synthetic::feed::fault_state>& faults) {
    std::vector<simulated_adapter> out;
    for (const auto& source : sources) {
        if (source.kind != "simulated")
            continue;
        simulated_adapter one;
        one.runner = std::make_unique<generator_runner>([&registry,
                                                         &build,
                                                         ctx = db_ctx,
                                                         faults,
                                                         name = source.name,
                                                         provider = source.provider_code] {
            return system_generator_offers(registry, ctx, build, name, provider, faults);
        });
        one.adapter = std::make_unique<simulator_adapter>(nats,
                                                          *one.runner,
                                                          source.name,
                                                          source.provider_code,
                                                          simulator_adapter_timing{},
                                                          faults);
        out.push_back(std::move(one));
    }
    return out;
}

// The adapters of the simulator, started once marketdata answers. The services of an installation
// start in no fixed order, so the simulator asks again until the catalogue is readable.
class adapter_fleet final {
private:
    static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.synthetic.service.app.adapter_fleet");
        return instance;
    }

public:
    adapter_fleet(ores::nats::service::client& nats,
                  ores::nats::service::nats_client& svc_nats,
                  ores::database::context db_ctx,
                  const feed_kind_registry& registry,
                  const ores::synthetic::feed::feed_build_context& build,
                  std::shared_ptr<ores::synthetic::feed::fault_state> faults)
        : nats_(nats)
        , svc_nats_(svc_nats)
        , db_ctx_(std::move(db_ctx))
        , registry_(registry)
        , build_(build)
        , faults_(std::move(faults))
        , worker_([this](ores::platform::concurrency::stop_token stop) { run(stop); }) {}

    ~adapter_fleet() {
        worker_.request_stop();
        if (worker_.joinable())
            worker_.join();
        for (auto& one : adapters_) {
            one.adapter->stop();
            one.runner->stop_all();
        }
    }

private:
    // Builds and starts every adapter before any is kept, so a failed start leaves none running.
    void start_all(const std::vector<ores::marketdata::domain::market_data_source>& sources) {
        auto built = make_adapters(sources, db_ctx_, nats_, registry_, build_, faults_);
        std::size_t attempted = 0;
        try {
            for (auto& one : built) {
                ++attempted;
                one.adapter->start();
            }
        } catch (...) {
            // The adapter that threw is stopped too, because it may be half started.
            for (std::size_t i = 0; i < attempted; ++i) {
                try {
                    built[i].adapter->stop();
                    built[i].runner->stop_all();
                } catch (const std::exception& e) {
                    BOOST_LOG_SEV(lg(), warn) << "An adapter did not stop cleanly: " << e.what();
                }
            }
            throw;
        }
        adapters_ = std::move(built);
        if (adapters_.empty())
            BOOST_LOG_SEV(lg(), warn)
                << "The catalogue holds no simulated source, so the simulator serves no adapter.";
        else
            BOOST_LOG_SEV(lg(), info)
                << adapters_.size() << " simulated source(s) served as adapters";
    }

    void run(ores::platform::concurrency::stop_token stop) {
        using namespace std::chrono_literals;
        auto wait = 2s;
        while (!stop.stop_requested()) {
            std::string failure;
            try {
                ores::marketdata::client::market_data_client client(svc_nats_);
                const auto sources = client.list_market_data_sources();
                if (sources) {
                    start_all(*sources);
                    return;
                }
                failure = sources.error();
                if (failure.find("denied") != std::string::npos)
                    failure += " (an environment populated before the grant of "
                               "market_data_sources:read needs the IAM roles populated again)";
            } catch (const std::exception& e) {
                failure = e.what();
            }
            BOOST_LOG_SEV(lg(), warn) << "The simulator serves no adapter yet: " << failure
                                      << ". Trying again in " << wait.count() << " s.";
            ores::platform::concurrency::wait_for(
                stop, std::chrono::duration_cast<std::chrono::milliseconds>(wait));
            wait = std::min(wait * 2, std::chrono::seconds(30));
        }
    }

    ores::nats::service::client& nats_;
    ores::nats::service::nats_client& svc_nats_;
    ores::database::context db_ctx_;
    const feed_kind_registry& registry_;
    const ores::synthetic::feed::feed_build_context& build_;
    std::shared_ptr<ores::synthetic::feed::fault_state> faults_;
    std::vector<simulated_adapter> adapters_;
    ores::platform::concurrency::jthread worker_;
};
}

ores::database::context application::make_context(const ores::database::database_options& db_opts) {
    using ores::database::context_factory;

    context_factory::configuration cfg{.database_options = db_opts,
                                       .pool_size = static_cast<std::size_t>(db_opts.pool_size),
                                       .num_attempts = 10,
                                       .wait_time_in_seconds = 1,
                                       .service_account = db_opts.user};

    return context_factory::make_context(cfg);
}

application::application() = default;

boost::asio::awaitable<void> application::run(boost::asio::io_context& io_ctx,
                                              const config::options& cfg) const {

    BOOST_LOG_SEV(lg(), info) << ores::utility::version::format_startup_message(
        "ores.synthetic.service", 0, 1);

    ores::nats::service::client nats(cfg.nats);
    nats.connect();

    // Authenticated client for service-to-service calls. The synthetic service
    // talks to the marketdata service (series + observations), all of which
    // require authentication and the marketdata permissions granted to the
    // SyntheticService IAM role. The token provider authenticates with the
    // service's own database account credentials.
    ores::nats::service::nats_client svc_nats(
        nats,
        ores::iam::client::make_service_token_provider(
            nats, cfg.database.user, cfg.database.password()));

    auto db_ctx = make_context(cfg.database);

    // =========================================================================
    // Entity change event pipeline: PostgreSQL LISTEN/NOTIFY → NATS publish
    // =========================================================================
    ev::service::event_bus event_bus;
    ev::service::postgres_event_source event_source(make_context(cfg.database), event_bus);
    auto generated_event_subs =
        messaging::event_registrar::register_event_mappings(event_source, event_bus, nats);
    event_source.start();
    BOOST_LOG_SEV(lg(), info) << "Entity change event pipeline started.";

    try {
        auto admin = nats.make_admin();
        // A tick is perishable and goes out on core NATS. The two streams that
        // used to store every tick for seven days had no consumer, and each
        // publish waited for their acknowledgement. A server that still holds them
        // from an earlier run loses them here. The delete is idempotent.
        admin.delete_stream(nats.make_stream_name("synthetic_ticks"));
        admin.delete_stream(nats.make_stream_name("synthetic_sandbox_ticks"));
        // synthetic_steps is not ensured here. The service's own registrar
        // declares it, from the same table it subscribes, so the stream's
        // coverage and the consumer's filters cannot disagree.
        BOOST_LOG_SEV(lg(), info) << "JetStream streams ready: the tick streams are retired";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to ensure JetStream stream: " << e.what();
        throw;
    }

    auto ctrl = std::make_shared<feed_controller>(nats);

    // The one per-kind registry: the config-plane dispatch table every
    // control-plane verb reads.
    const auto registry = make_default_feed_kind_registry();

    // One market for the whole run: every feed built here consults it, so a
    // busy market lifts all of them together. A feed only reads it; this thread
    // is its single owner, and moving it on a few times a second is enough for a
    // state whose dwell times are minutes.
    ores::analytics::quant::domain::market_regime market;
    // The heat any event anywhere adds to. The same thread fades it, so the decay
    // is paid once a period rather than once per reader.
    // Jumps and decay: half a rate per event, fading at twenty a second, which
    // settles at an event rate of ten a second adding a quarter of a rate. Zero
    // jump would leave the factor inert, so the default is explicit here.
    ores::analytics::quant::domain::shared_intensity intensity(0.5, 20.0);

    // One clock for the whole run: every feed registers its tick body here at
    // start() and leaves at stop(), so the order in which series tick is decided
    // in one place and a series can see another's event.
    ores::analytics::quant::service::tick_scheduler scheduler;
    scheduler.set_hooks([](std::uint64_t, const std::string&) {},
                        [](const std::exception& ex) {
                            BOOST_LOG_SEV(lg(), error) << "SYNTHETIC TICK FAILED: " << ex.what();
                        });
    std::jthread scheduler_thread([&scheduler](const std::stop_token& stop) {
        scheduler.run();
        (void)stop;
    });

    constexpr auto market_period = std::chrono::milliseconds(100);
    std::jthread market_owner([&market, &intensity, market_period](const std::stop_token& stop) {
        while (!stop.stop_requested()) {
            std::this_thread::sleep_for(market_period);
            market.advance_to_now();
            intensity.fade(std::chrono::duration<double>(market_period).count());
        }
    });

    // The simulator is an adapter of the market data service for each simulated source. It
    // serves the system collections of the provider of the source, and it starts a generator for
    // each id that the demand names.
    const ores::synthetic::feed::feed_build_context adapter_build{.nats = nats,
                                                                  .auth_nats = svc_nats,
                                                                  .market = &market,
                                                                  .intensity = &intensity,
                                                                  .scheduler = &scheduler};
    const auto faults = std::make_shared<ores::synthetic::feed::fault_state>();
    adapter_fleet fleet(nats, svc_nats, db_ctx, registry, adapter_build, faults);

    co_await ores::service::service::run(
        io_ctx,
        nats,
        std::move(db_ctx),
        "ores.synthetic.service",
        [ctrl, faults, &svc_nats, &registry, &market, &intensity, &scheduler](
            auto& n, auto c, auto v) {
            auto subs = ores::synthetic::messaging::registrar::register_handlers(n, c, v);
            auto market_subs = ores::synthetic::service::registrar::register_handlers(
                n, svc_nats, ctrl, faults, c, v, registry, &market, &intensity, &scheduler);
            subs.insert(subs.end(),
                        std::make_move_iterator(market_subs.begin()),
                        std::make_move_iterator(market_subs.end()));
            return subs;
        },
        [&nats](boost::asio::io_context& ioc) {
            auto hb = std::make_shared<ores::service::service::heartbeat_publisher>(
                std::string(service_name), std::string(service_version), nats);
            boost::asio::co_spawn(ioc, [hb]() { return hb->run(); }, boost::asio::detached);
        });

    scheduler.stop();
    ctrl->shutdown();
    event_source.stop();
    co_return;
}

}
