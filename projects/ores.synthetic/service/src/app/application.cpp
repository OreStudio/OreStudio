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
#include "../registrar.hpp"
#include "ores.database/service/context_factory.hpp"
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
#include "ores.synthetic.core/repository/market_data_generation_config_repository.hpp"
#include "ores.synthetic.service/app/application_exception.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/version/version.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <map>
#include <memory>
#include <rfl/json.hpp>
#include <set>
#include <span>
#include <vector>

namespace ores::synthetic::service::app {

using namespace ores::logging;

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
        if (ctrl.start(fx.ore_key,
                       fx.source_name,
                       std::move(means),
                       std::move(stdevs),
                       std::move(weights),
                       fx.gmm_initial_price,
                       static_cast<double>(fx.ticks_per_hour),
                       fx.process_type))
            ++started;
    }
    BOOST_LOG_SEV(auto_start_lg(), info) << "Auto-started " << started << " enabled feed(s).";
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

    auto ctrl = std::make_shared<feed_controller>(nats, svc_nats, db_ctx.tenant_id());

    // Autonomous, config-driven generation: start every enabled FX rate across
    // enabled configs. Each feed resolves its own series and publishes on its
    // synthetic producer channel.
    auto_start_enabled_feeds(*ctrl, db_ctx);
    BOOST_LOG_SEV(lg(), info) << "Feed controller ready — " << ctrl->running_count()
                              << " feed(s) auto-started; waiting for control signals";

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
    co_return;
}

}
