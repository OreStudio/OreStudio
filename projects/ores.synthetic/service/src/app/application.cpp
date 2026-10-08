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
#include "../registrar.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.eventing.api/service/event_bus.hpp"
#include "ores.eventing.core/service/postgres_event_source.hpp"
#include "ores.iam.client/client/service_token_provider.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.client/market_data_client.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.service/service/domain_service_runner.hpp"
#include "ores.service/service/heartbeat_publisher.hpp"
#include "ores.synthetic.api/feeds/feed_factory.hpp"
#include "ores.synthetic.core/messaging/registrar.hpp"
#include "ores.synthetic.service/app/application_exception.hpp"
#include "ores.synthetic.service/messaging/event_registrar.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/version/version.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <memory>
#include <rfl/json.hpp>
#include <span>
#include <vector>

namespace ores::synthetic::service::app {

using namespace ores::logging;
namespace ev = ores::eventing;

namespace {
constexpr std::string_view service_name = "ores.synthetic.service";
constexpr std::string_view service_version = ORES_VERSION;

// One boot-time walk: start every auto-startable feed of every kind the
// registry knows. The startability gate lives on the attempt itself, and the
// walk adds its own auto_start term, which is genuinely about blameless boot
// behaviour rather than about a kind.
auto& auto_start_lg() {
    static auto instance = ores::logging::make_logger("ores.synthetic.service.app.auto_start");
    return instance;
}

void auto_start_feeds(feed_controller& ctrl,
                      const feed_kind_registry& registry,
                      const ores::synthetic::feed::feed_build_context& bctx,
                      const ores::database::context& ctx) {
    int started = 0;
    for (const auto& target : registry.targets(ctx)) {
        const auto& c = target.row.candidate;
        if (!c.auto_start)
            continue;
        try {
            auto attempt = registry.make_feed(target, bctx);
            if (!attempt.feed) {
                BOOST_LOG_SEV(auto_start_lg(), warn)
                    << "Skipping enabled feed " << c.display_name << " — " << attempt.failure;
                continue;
            }
            std::string conflicting_source_name;
            if (ctrl.add(std::move(attempt.feed),
                         target.binding_mode,
                         bctx.caller_bearer_token,
                         &conflicting_source_name)) {
                ++started;
            } else if (!conflicting_source_name.empty()) {
                // A genuine seed-data misconfiguration (two auto-start
                // configs sharing a market data key), not a per-request
                // error -- log clearly and move on rather than failing the
                // whole auto-start pass. An already-running same-source row
                // (a duplicate config) stays silent, as before.
                BOOST_LOG_SEV(auto_start_lg(), error)
                    << "Skipping auto-start of " << c.display_name << " — feed '"
                    << conflicting_source_name
                    << "' is already running for the same market data key.";
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(auto_start_lg(), error)
                << "Failed to auto-start " << c.display_name << " (" << target.row.kind->kind
                << "): " << e.what();
        }
    }
    BOOST_LOG_SEV(auto_start_lg(), info) << "Auto-started " << started << " enabled feed(s).";
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
        // The unified tick scheme (synthetic.v1.ops.tick.<kind>.<source>) is fully covered
        // by synthetic_ticks's "synthetic.v1.ops.tick.>" filter; the retired
        // synthetic_curve_ticks stream (curve_family subjects) is no longer ensured —
        // a stale one on an already-running server is inert (no producers, no consumers).
        admin.ensure_stream(nats.make_stream_name("synthetic_ticks"),
                            {nats.make_subject("synthetic.v1.ops.tick.>")});
        // Sandboxed feeds (binding_mode::sandboxed, see feed_controller's
        // producer_subject) publish under a distinct
        // "synthetic.v1.ops.sandbox_tick.>" subject, not covered by
        // synthetic_ticks's "synthetic.v1.ops.tick.>" filter -- js_publish to a
        // subject with no matching stream throws, so this needs its own stream.
        admin.ensure_stream(nats.make_stream_name("synthetic_sandbox_ticks"),
                            {nats.make_subject("synthetic.v1.ops.sandbox_tick.>")});
        BOOST_LOG_SEV(lg(), info)
            << "JetStream streams ready: synthetic_ticks, synthetic_sandbox_ticks";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to ensure JetStream stream: " << e.what();
        throw;
    }

    auto ctrl = std::make_shared<feed_controller>(nats, svc_nats);

    // The one per-kind registry: the config-plane dispatch table every
    // control-plane verb and the boot walk below read.
    const auto registry = make_default_feed_kind_registry();

    // The shared inputs every producer builder needs. Auto-start has no
    // end-user session, so the caller bearer token is empty.
    const ores::synthetic::feed::feed_build_context bctx{nats, svc_nats, {}};

    // Autonomous, config-driven generation: one boot-time walk starts every
    // auto-startable feed of every kind. Each feed resolves its own series
    // and publishes on its synthetic producer channel.
    auto_start_feeds(*ctrl, registry, bctx, db_ctx);
    BOOST_LOG_SEV(lg(), info) << "Feed controller ready — " << ctrl->running_count()
                              << " feed(s) auto-started; waiting for control signals";

    co_await ores::service::service::run(
        io_ctx,
        nats,
        std::move(db_ctx),
        "ores.synthetic.service",
        [ctrl, &svc_nats, &registry](auto& n, auto c, auto v) {
            auto subs = ores::synthetic::messaging::registrar::register_handlers(n, c, v);
            auto market_subs = ores::synthetic::service::registrar::register_handlers(
                n, svc_nats, ctrl, c, v, registry);
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

    ctrl->shutdown();
    event_source.stop();
    co_return;
}

}
