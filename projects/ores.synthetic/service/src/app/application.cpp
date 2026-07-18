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
#include "ores.analytics.quant/service/process_factory.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.eventing.api/service/event_bus.hpp"
#include "ores.eventing.core/service/postgres_event_source.hpp"
#include "ores.iam.client/client/service_token_provider.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.client/market_data_client.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.refdata.core/repository/instrument_code_repository.hpp"
#include "ores.refdata.core/repository/payment_frequency_repository.hpp"
#include "ores.refdata.core/repository/tenor_convention_repository.hpp"
#include "ores.refdata.core/repository/tenor_convention_resolution_repository.hpp"
#include "ores.refdata.core/repository/tenor_repository.hpp"
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
#include <algorithm>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
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

std::string lowercase(std::string s) {
    std::transform(s.begin(), s.end(), s.begin(), [](unsigned char c) {
        return static_cast<char>(std::tolower(c));
    });
    return s;
}

// Build the producer subject from source_name -- same sanitisation feed_controller applies for
// its own (scalar tick) subjects, kept in sync here since curve family subjects share the same
// NATS token constraints.
std::string curve_producer_subject(const std::string& source_name) {
    std::string token;
    token.reserve(source_name.size());
    for (unsigned char c : source_name) {
        const bool safe = std::isalnum(c) || c == '.' || c == '_' || c == '-';
        token += safe ? static_cast<char>(c) : '_';
    }
    return "synthetic.v1.curve_family." + token;
}

// Start every enabled ir_curve_generation_config as its own ir_curve_feed producer. Phase-1,
// auto-start only (see the tick-batch-publishing task) -- no NATS control-plane for curve feeds
// yet, mirroring how auto_start_enabled_feeds() above bootstraps FX feeds from config tables.
void auto_start_enabled_ir_curve_feeds(ores::nats::service::client& nats,
                                      ores::synthetic::service::curve_feed_controller& ctrl,
                                      const ores::database::context& ctx) {
    namespace synth_repo = ores::synthetic::repository;
    namespace refdata_repo = ores::refdata::repository;
    using ores::synthetic::service::ir_curve_feed;
    using ores::synthetic::service::ir_curve_refdata_context;

    synth_repo::ir_curve_generation_config_repository config_repo;
    synth_repo::ir_curve_template_entry_repository entry_repo;
    refdata_repo::instrument_code_repository instrument_code_repo;
    refdata_repo::payment_frequency_repository payment_frequency_repo;
    refdata_repo::tenor_repository tenor_repo;
    refdata_repo::tenor_convention_repository convention_repo;
    refdata_repo::tenor_convention_resolution_repository resolution_repo(ctx);

    const auto configs = config_repo.read_latest(ctx);
    const auto entries = entry_repo.read_latest(ctx);
    const auto conventions = convention_repo.read_latest(ctx, "RATES_SPOT_FORWARD");
    if (conventions.empty()) {
        BOOST_LOG_SEV(auto_start_lg(), error)
            << "RATES_SPOT_FORWARD tenor convention not found — no IR curve feeds started.";
        return;
    }

    ir_curve_refdata_context refctx;
    refctx.convention = conventions.front();
    for (const auto& t : tenor_repo.read_latest(ctx))
        refctx.tenors_by_code.emplace(t.code, t);
    for (const auto& ic : instrument_code_repo.read_latest(ctx, 0, 10000))
        refctx.instrument_codes_by_code.emplace(ic.code, ic);
    for (const auto& pf : payment_frequency_repo.read_latest(ctx))
        refctx.payment_frequencies_by_code.emplace(pf.code, pf);
    for (const auto& r : resolution_repo.read_latest_by_convention(refctx.convention.code))
        refctx.resolutions_by_tenor.emplace(r.tenor_code, r);
    refctx.horizon = std::chrono::floor<std::chrono::days>(std::chrono::system_clock::now());
    refctx.spot = refctx.horizon; // T+0: see ir_curve_template_resolver.hpp's resolve() doc.

    std::map<boost::uuids::uuid, std::vector<ores::synthetic::domain::ir_curve_template_entry>>
        entries_by_config;
    for (const auto& e : entries)
        entries_by_config[e.ir_curve_config_id].push_back(e);

    int started = 0;
    for (const auto& cfg : configs) {
        if (!cfg.enabled)
            continue;
        const auto it = entries_by_config.find(cfg.id);
        if (it == entries_by_config.end() || it->second.empty()) {
            BOOST_LOG_SEV(auto_start_lg(), warn)
                << "Skipping enabled IR curve config " << cfg.currency_code << "/" << cfg.index_name
                << " — no template entries.";
            continue;
        }

        try {
            auto resolved = ores::synthetic::service::resolve(
                it->second, refctx, cfg.fixed_leg_payment_frequency_code);

            auto process = ores::analytics::quant::service::process_factory::make_yield_curve_process(
                lowercase(cfg.process_type), cfg.kappa, {cfg.theta}, cfg.sigma, cfg.initial_rate);

            const std::string source_name = "ir_curve." + lowercase(cfg.currency_code) + "." +
                                            lowercase(cfg.index_name);
            auto feed = std::make_shared<ir_curve_feed>(nats,
                                                        cfg.tenant_id,
                                                        cfg.party_id,
                                                        source_name,
                                                        curve_producer_subject(source_name),
                                                        "RATES",
                                                        "YIELD",
                                                        cfg.currency_code + "/" + cfg.index_name,
                                                        std::move(process),
                                                        static_cast<double>(cfg.ticks_per_hour),
                                                        std::move(resolved));
            ctrl.add(std::move(feed));
            ++started;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(auto_start_lg(), error)
                << "Failed to start IR curve feed for " << cfg.currency_code << "/"
                << cfg.index_name << ": " << e.what();
        }
    }
    BOOST_LOG_SEV(auto_start_lg(), info) << "Auto-started " << started << " enabled IR curve feed(s).";
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
                            {nats.make_subject("synthetic.v1.tick.>"),
                             nats.make_subject("synthetic.v1.curve_family.>")});
        BOOST_LOG_SEV(lg(), info) << "JetStream stream ready: synthetic_ticks";
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
        [ctrl](auto& n, auto c, auto v) {
            auto subs = ores::synthetic::messaging::registrar::register_handlers(n, c, v);
            auto market_subs =
                ores::synthetic::service::registrar::register_handlers(n, ctrl, c, v);
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
