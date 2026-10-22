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
#include "../feed_restore.hpp"
#include "../registrar.hpp"
#include "../restore_retry.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.database/service/party_context.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.eventing.api/service/event_bus.hpp"
#include "ores.eventing.core/service/postgres_event_source.hpp"
#include "ores.iam.client/client/service_token_provider.hpp"
#include "ores.iam.core/repository/tenant_repository.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.api/domain/tick_subjects.hpp"
#include "ores.marketdata.client/market_data_client.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.platform/concurrency/stoppable_thread.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
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

// The restore of the feeds: one pass for each tenant when the service starts,
// and tries again for the feeds that a dependency kept from starting. A context
// sees only its own tenant's collections, plus the system ones, so the walk runs
// once for each tenant.
auto& auto_start_lg() {
    static auto instance = ores::logging::make_logger("ores.synthetic.service.app.auto_start");
    return instance;
}

// A scope is what one pass reads through: the system tenant, or one root party
// of a tenant, written "<tenant>/<party>". A party's collection is visible only
// to a context with the party in its visible set, and the subtree of a root party
// is the visible set. A tenant can have more than one root party, such as its
// system party and its top operational party, so each root is a scope of its own.
std::vector<std::string> tenant_scopes(const ores::database::context& db_ctx,
                                       const std::string& tenant) {
    const auto tenant_ctx = ores::database::service::tenant_context::with_tenant(db_ctx, tenant);
    std::vector<std::string> scopes;
    for (const auto& party : ores::refdata::repository::party_repository().read_latest(tenant_ctx))
        if (!party.parent_party_id)
            scopes.push_back(tenant + "/" + boost::uuids::to_string(party.id));
    if (scopes.empty())
        scopes.push_back(tenant);
    return scopes;
}

ores::database::context scope_context(const ores::database::context& db_ctx,
                                      const std::string& scope) {
    const auto slash = scope.find('/');
    const auto tenant = scope.substr(0, slash);
    // The service's own context belongs to the system tenant, so it cannot read
    // a tenant's collections. The system tenant is read through it.
    if (tenant == ores::utility::uuid::tenant_id::system().to_string())
        return db_ctx;
    const auto tenant_ctx = ores::database::service::tenant_context::with_tenant(db_ctx, tenant);
    if (slash == std::string::npos)
        return tenant_ctx;
    return ores::database::service::party_context::with_party(
        tenant_ctx,
        tenant_ctx.tenant_id(),
        boost::uuids::string_generator()(scope.substr(slash + 1)),
        tenant_ctx.actor());
}

// The feeds that no pass has started yet, by scope and feed config id.
using pending_feeds = std::map<std::string, std::map<std::string, restore_failure>>;

// Replaces what a scope still has to start with the failures of its last pass.
// A feed that a wait cannot cure is logged and left out.
void note_failures(pending_feeds& pending, const std::string& scope, const restore_report& report) {
    auto& feeds = pending[scope];
    feeds.clear();
    for (const auto& f : report.failures) {
        if (f.retry)
            feeds[f.feed_config_id] = f;
        else
            BOOST_LOG_SEV(auto_start_lg(), error) << "Feed " << f.display_name << " of " << scope
                                                  << " does not start — " << f.reason << ".";
    }
    if (feeds.empty())
        pending.erase(scope);
}

std::size_t pending_total(const pending_feeds& pending) {
    std::size_t total = 0;
    for (const auto& [_, feeds] : pending)
        total += feeds.size();
    return total;
}

// One pass for the system tenant and then for each root party of every other
// active tenant. A scope that fails does not stop the others, and its feeds stay
// pending.
int restore_all_tenants(const feed_restorer& restorer,
                        const ores::database::context& db_ctx,
                        pending_feeds& pending) {
    int started = 0;
    const auto run = [&](const std::string& scope) {
        try {
            const auto report = restorer.pass(scope_context(db_ctx, scope));
            BOOST_LOG_SEV(auto_start_lg(), info)
                << "Auto-started " << report.started << " enabled feed(s) for " << scope << ".";
            started += static_cast<int>(report.started);
            note_failures(pending, scope, report);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(auto_start_lg(), error)
                << "Failed to auto-start feeds for " << scope << ": " << e.what();
        }
    };
    const auto system = ores::utility::uuid::tenant_id::system().to_string();
    run(system);
    for (const auto& tenant : ores::iam::repository::tenant_repository().read_latest(db_ctx)) {
        const auto id = boost::uuids::to_string(tenant.id);
        if (id == system || tenant.status != "active")
            continue;
        try {
            for (const auto& scope : tenant_scopes(db_ctx, id))
                run(scope);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(auto_start_lg(), error)
                << "Failed to read the parties of tenant " << id << ": " << e.what();
        }
    }
    return started;
}

// One try for each feed that is still pending. Answers how many remain.
std::size_t retry_pending(const feed_restorer& restorer,
                          const ores::database::context& db_ctx,
                          pending_feeds& pending) {
    const auto scopes = pending;
    for (const auto& [scope, feeds] : scopes) {
        std::set<std::string> ids;
        for (const auto& [id, _] : feeds)
            ids.insert(id);
        try {
            const auto report = restorer.pass(scope_context(db_ctx, scope), &ids);
            if (report.started > 0)
                BOOST_LOG_SEV(auto_start_lg(), info)
                    << "Auto-started " << report.started << " more feed(s) for " << scope << ".";
            note_failures(pending, scope, report);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(auto_start_lg(), warn)
                << "Feeds of " << scope << " are not read yet: " << e.what();
        }
    }
    return pending_total(pending);
}

void log_unstarted(const pending_feeds& pending) {
    for (const auto& [scope, feeds] : pending)
        for (const auto& [_, f] : feeds)
            BOOST_LOG_SEV(auto_start_lg(), error)
                << "Feed " << f.display_name << " of " << scope
                << " did not start before the restore gave up — " << f.reason << ".";
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
    // control-plane verb and the boot walk below read.
    const auto registry = make_default_feed_kind_registry();

    // The shared inputs every producer builder needs. The run token of each
    // feed is set when the feed starts.
    const ores::synthetic::feed::feed_build_context bctx{nats, svc_nats, {}};

    // Autonomous, config-driven generation: one boot-time walk starts every
    // auto-startable feed of every kind. Each feed resolves its own series
    // and publishes on its synthetic producer channel. A feed that a dependency
    // keeps from starting is tried again in the background.
    const auto restorer = std::make_shared<feed_restorer>(*ctrl, registry, bctx);
    auto pending = std::make_shared<pending_feeds>();
    restore_all_tenants(*restorer, db_ctx, *pending);
    BOOST_LOG_SEV(lg(), info) << "Feed controller ready — " << ctrl->running_count()
                              << " feed(s) auto-started; waiting for control signals";

    ores::platform::concurrency::stoppable_thread restore_retries;
    if (const auto left = pending_total(*pending); left > 0) {
        BOOST_LOG_SEV(lg(), warn) << left << " feed(s) did not start; trying again for up to "
                                  << std::chrono::duration_cast<std::chrono::minutes>(
                                         restore_retry_policy{}.budget)
                                         .count()
                                  << " minutes";
        restore_retries = ores::platform::concurrency::stoppable_thread(
            [restorer, pending, retry_ctx = db_ctx](
                const ores::platform::concurrency::stop_source& stop) {
                const auto sleep = [&](std::chrono::milliseconds span) {
                    stop.wait_for(span);
                };
                const auto left = retry_restore(
                    restore_retry_policy{},
                    pending_total(*pending),
                    [&] { return retry_pending(*restorer, retry_ctx, *pending); },
                    sleep,
                    [&] { return stop.stop_requested(); });
                if (left > 0 && !stop.stop_requested())
                    log_unstarted(*pending);
            });
    }

    co_await ores::service::service::run(
        io_ctx,
        nats,
        std::move(db_ctx),
        "ores.synthetic.service",
        [ctrl, restorer, &svc_nats, &registry](auto& n, auto c, auto v) {
            auto subs = ores::synthetic::messaging::registrar::register_handlers(n, c, v);
            auto market_subs = ores::synthetic::service::registrar::register_handlers(
                n, svc_nats, ctrl, restorer, c, v, registry);
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

    if (restore_retries.joinable()) {
        restore_retries.request_stop();
        restore_retries.join();
    }
    ctrl->shutdown();
    event_source.stop();
    co_return;
}

}
