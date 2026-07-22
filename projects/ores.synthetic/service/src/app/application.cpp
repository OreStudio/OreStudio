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
#include "../curve_feed_controller.hpp"
#include "../feed_controller.hpp"
#include "../ir_curve_feed.hpp"
#include "../ir_curve_template_resolver.hpp"
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
#include "ores.synthetic.api/domain/gmm_component.hpp"
#include "ores.synthetic.core/messaging/registrar.hpp"
#include "ores.synthetic.core/repository/fx_spot_generation_config_repository.hpp"
#include "ores.synthetic.core/repository/gmm_component_repository.hpp"
#include "ores.synthetic.core/repository/ir_curve_generation_config_repository.hpp"
#include "ores.synthetic.core/repository/ir_curve_template_entry_repository.hpp"
#include "ores.synthetic.core/repository/market_data_generation_config_repository.hpp"
#include "ores.synthetic.service/app/application_exception.hpp"
#include "ores.synthetic.service/messaging/event_registrar.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/version/version.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <map>
#include <memory>
#include <rfl/json.hpp>
#include <set>
#include <span>
#include <vector>

namespace ores::synthetic::service::app {

using namespace ores::logging;
namespace ev = ores::eventing;

namespace {
constexpr std::string_view service_name = "ores.synthetic.service";
constexpr std::string_view service_version = ORES_VERSION;

// Start every enabled FX rate across enabled feed configs as a producer.
auto& auto_start_lg() {
    static auto instance = ores::logging::make_logger("ores.synthetic.service.app.auto_start");
    return instance;
}

void auto_start_enabled_feeds(feed_controller& ctrl, const ores::database::context& ctx) {
    namespace repo = ores::synthetic::repository;

    repo::market_data_generation_config_repository feed_repo;
    repo::fx_spot_generation_config_repository fx_repo;
    repo::gmm_component_repository comp_repo;

    const auto feeds = feed_repo.read_latest(ctx);
    const auto fxs = fx_repo.read_latest(ctx);
    const auto comps = comp_repo.read_latest(ctx);

    std::set<boost::uuids::uuid> enabled_feeds;
    for (const auto& f : feeds)
        if (f.enabled)
            enabled_feeds.insert(f.id);

    // Group components by their parent FX config id; note the field asymmetry —
    // gmm_component::fx_spot_config_id keys against fx_spot_generation_config::id.
    std::map<boost::uuids::uuid, std::vector<ores::synthetic::domain::gmm_component>> by_fx;
    for (const auto& c : comps)
        by_fx[c.fx_spot_config_id].push_back(c);

    int started = 0;
    for (const auto& fx : fxs) {
        if (!fx.enabled || !enabled_feeds.contains(fx.config_id))
            continue;
        const auto it = by_fx.find(fx.id);
        if (it == by_fx.end() || it->second.empty()) {
            BOOST_LOG_SEV(auto_start_lg(), warn)
                << "Skipping enabled FX rate " << fx.ore_key << " — no GMM components.";
            continue;
        }
        std::vector<double> means, stdevs, weights;
        for (const auto& c : it->second) {
            means.push_back(c.mean);
            stdevs.push_back(c.stdev);
            weights.push_back(c.weight);
        }
        const auto r = ctrl.start(fx.ore_key,
                                  fx.source_name,
                                  std::move(means),
                                  std::move(stdevs),
                                  std::move(weights),
                                  fx.gmm_initial_price,
                                  static_cast<double>(fx.ticks_per_hour),
                                  fx.process_type);
        if (r == feed_controller::start_result::started)
            ++started;
    }
    BOOST_LOG_SEV(auto_start_lg(), info) << "Auto-started " << started << " enabled feed(s).";
}

// Start every enabled ir_curve_generation_config as its own ir_curve_feed producer. Auto-start
// covers system-tenant configs only (this function's ctx is the service's own unscoped context);
// per-tenant configs (e.g. a party's own published dataset) are started on demand instead, via
// ir_curve_feed_config_handler's NATS control-plane -- mirroring feed_controller/
// market_feed_config_handler's split for FX.
void auto_start_enabled_ir_curve_feeds(ores::nats::service::client& nats,
                                       ores::synthetic::service::curve_feed_controller& ctrl,
                                       const ores::database::context& ctx) {
    namespace synth_repo = ores::synthetic::repository;
    using ores::synthetic::service::build_ir_curve_refdata_context;
    using ores::synthetic::service::make_ir_curve_feed;

    synth_repo::ir_curve_generation_config_repository config_repo;
    synth_repo::ir_curve_template_entry_repository entry_repo;

    const auto configs = config_repo.read_latest(ctx);
    const auto entries = entry_repo.read_latest(ctx);

    auto refctx = build_ir_curve_refdata_context(ctx);
    if (!refctx) {
        BOOST_LOG_SEV(auto_start_lg(), error)
            << "RATES_SPOT_FORWARD tenor convention not found — no IR curve feeds started.";
        return;
    }

    std::map<boost::uuids::uuid, std::vector<ores::synthetic::domain::ir_curve_template_entry>>
        entries_by_config;
    for (const auto& e : entries)
        entries_by_config[e.ir_curve_config_id].push_back(e);

    int started = 0;
    for (const auto& cfg : configs) {
        // auto_start is the auto-start-eligibility flag; enabled alone only means "startable at
        // all" (manually or automatically) -- an enabled=true, auto_start=false config (e.g. a
        // legacy/alternate-index variant) is deliberately skipped here and left for on-demand
        // start only.
        if (!cfg.enabled || !cfg.auto_start)
            continue;
        const auto it = entries_by_config.find(cfg.id);
        if (it == entries_by_config.end() || it->second.empty()) {
            BOOST_LOG_SEV(auto_start_lg(), warn)
                << "Skipping enabled IR curve config " << cfg.currency_code << "/" << cfg.index_name
                << " — no template entries.";
            continue;
        }

        try {
            std::string conflicting_source_name;
            if (ctrl.add(make_ir_curve_feed(nats, cfg, it->second, *refctx),
                         &conflicting_source_name)) {
                ++started;
            } else {
                // A genuine seed-data misconfiguration (two auto_start=true configs sharing a
                // qualifier), not a per-request error -- log clearly and move on rather than
                // failing the whole auto-start pass.
                BOOST_LOG_SEV(auto_start_lg(), error)
                    << "Skipping IR curve config " << cfg.currency_code << "/" << cfg.index_name
                    << " — qualifier already held by auto-started feed '" << conflicting_source_name
                    << "'; both are enabled+auto_start for the same market data key.";
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(auto_start_lg(), error)
                << "Failed to start IR curve feed for " << cfg.currency_code << "/"
                << cfg.index_name << ": " << e.what();
        }
    }
    BOOST_LOG_SEV(auto_start_lg(), info)
        << "Auto-started " << started << " enabled IR curve feed(s).";
}
}

ores::database::context application::make_context(const ores::database::database_options& db_opts) {
    using ores::database::context_factory;

    context_factory::configuration cfg{.database_options = db_opts,
                                       .pool_size = 4,
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
    BOOST_LOG_SEV(lg(), info) << "Connected to NATS: " << cfg.nats.url << " (namespace: '"
                              << (cfg.nats.subject_prefix.empty() ? "(none)" :
                                                                    cfg.nats.subject_prefix)
                              << "')";

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
        admin.ensure_stream(nats.make_stream_name("synthetic_ticks"),
                            {nats.make_subject("synthetic.v1.tick.>")});
        // A separate, dedicated stream -- not appended onto synthetic_ticks -- because
        // ensure_stream() only creates a stream when it does not already exist; it never
        // updates an existing stream's subject list. synthetic_ticks predates the curve
        // family subject on every environment that already has it, so adding the subject
        // here would silently never take effect there.
        admin.ensure_stream(nats.make_stream_name("synthetic_curve_ticks"),
                            {nats.make_subject("synthetic.v1.curve_family.>")});
        BOOST_LOG_SEV(lg(), info)
            << "JetStream streams ready: synthetic_ticks, synthetic_curve_ticks";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to ensure JetStream stream: " << e.what();
        throw;
    }

    auto ctrl = std::make_shared<feed_controller>(nats, svc_nats, db_ctx.tenant_id());

    // Autonomous, config-driven generation: start every enabled FX rate across
    // enabled configs. Each feed resolves its own series and publishes on its
    // synthetic producer channel.
    auto_start_enabled_feeds(*ctrl, db_ctx);
    BOOST_LOG_SEV(lg(), info) << "Feed controller ready — " << ctrl->running_count()
                              << " feed(s) auto-started; waiting for control signals";

    auto curve_ctrl = std::make_shared<ores::synthetic::service::curve_feed_controller>();
    auto_start_enabled_ir_curve_feeds(nats, *curve_ctrl, db_ctx);
    BOOST_LOG_SEV(lg(), info) << "Curve feed controller ready — " << curve_ctrl->running_count()
                              << " feed(s) auto-started";

    co_await ores::service::service::run(
        io_ctx,
        nats,
        std::move(db_ctx),
        "ores.synthetic.service",
        [ctrl, curve_ctrl](auto& n, auto c, auto v) {
            auto subs = ores::synthetic::messaging::registrar::register_handlers(n, c, v);
            auto market_subs =
                ores::synthetic::service::registrar::register_handlers(n, ctrl, curve_ctrl, c, v);
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
    curve_ctrl->shutdown();
    event_source.stop();
    co_return;
}

}
