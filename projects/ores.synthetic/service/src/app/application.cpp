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
#include "ores.marketdata.core/repository/market_data_source_repository.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
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
std::vector<simulated_adapter>
make_adapters(const ores::database::context& db_ctx,
              ores::nats::service::client& nats,
              const feed_kind_registry& registry,
              const ores::synthetic::feed::feed_build_context& build,
              const std::shared_ptr<ores::synthetic::feed::fault_state>& faults) {
    std::vector<simulated_adapter> out;
    const auto sources =
        ores::marketdata::repository::market_data_source_repository{}.read_latest(db_ctx);
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

    auto ctrl = std::make_shared<feed_controller>(nats, svc_nats);

    // The one per-kind registry: the config-plane dispatch table every
    // control-plane verb reads.
    const auto registry = make_default_feed_kind_registry();

    // The simulator is an adapter of the market data service for each simulated source. It
    // serves the system collections of the provider of the source, and it starts a generator for
    // each id that the demand names.
    const ores::synthetic::feed::feed_build_context adapter_build{nats, svc_nats};
    const auto faults = std::make_shared<ores::synthetic::feed::fault_state>();
    auto adapters = make_adapters(db_ctx, nats, registry, adapter_build, faults);
    for (auto& one : adapters)
        one.adapter->start();
    if (adapters.empty())
        BOOST_LOG_SEV(lg(), warn)
            << "The catalogue holds no simulated source, so the simulator serves no adapter.";
    else
        BOOST_LOG_SEV(lg(), info) << adapters.size() << " simulated source(s) served as adapters";

    co_await ores::service::service::run(
        io_ctx,
        nats,
        std::move(db_ctx),
        "ores.synthetic.service",
        [ctrl, faults, &svc_nats, &registry](auto& n, auto c, auto v) {
            auto subs = ores::synthetic::messaging::registrar::register_handlers(n, c, v);
            auto market_subs = ores::synthetic::service::registrar::register_handlers(
                n, svc_nats, ctrl, faults, c, v, registry);
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

    for (auto& one : adapters) {
        one.adapter->stop();
        one.runner->stop_all();
    }
    ctrl->shutdown();
    event_source.stop();
    co_return;
}

}
