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
#include "ores.synthetic.core/messaging/registrar.hpp"
#include "ores.synthetic.service/app/application_exception.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/version/version.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <memory>
#include <rfl/json.hpp>
#include <span>

namespace ores::synthetic::service::app {

using namespace ores::logging;

namespace {
constexpr std::string_view service_name = "ores.synthetic.service";
constexpr std::string_view service_version = ORES_VERSION;
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
    ores::marketdata::client::market_data_client md_client(svc_nats);

    auto db_ctx = make_context(cfg.database);

    // Look up or create the EUR/USD market series; the resolved series_id is
    // passed into the feed_controller and stamped on every market_observation.
    boost::uuids::uuid series_id{};
    {
        using namespace ores::marketdata::domain;

        auto existing = md_client.list_series("FX");
        if (!existing)
            throw application_exception("Failed to list FX market series: " + existing.error());

        for (const auto& s : *existing) {
            if (s.metric == "RATE" && s.qualifier == "EUR/USD") {
                series_id = s.id;
                BOOST_LOG_SEV(lg(), info)
                    << "Found existing EUR/USD series: " << boost::uuids::to_string(series_id);
                break;
            }
        }

        if (series_id.is_nil()) {
            boost::uuids::random_generator gen;
            series_id = gen();

            market_series s;
            s.id = series_id;
            s.tenant_id = db_ctx.tenant_id();
            s.series_type = "FX";
            s.metric = "RATE";
            s.qualifier = "EUR/USD";
            s.asset_class = asset_class::fx;
            s.subclass = series_subclass::spot;
            s.is_scalar = true;
            s.modified_by = "ores.synthetic.service";
            s.performed_by = "ores.synthetic.service";
            s.change_reason_code = "system.initial_load";
            s.change_commentary = "EUR/USD FX spot synthetic feed initialisation";

            const auto saved = md_client.save_series({s});
            if (!saved)
                throw application_exception("Failed to create EUR/USD market series: " +
                                            saved.error());
            BOOST_LOG_SEV(lg(), info)
                << "Created EUR/USD market series: " << boost::uuids::to_string(series_id);
        }
    }

    auto ctrl = std::make_shared<feed_controller>(nats, svc_nats, series_id, db_ctx.tenant_id());
    BOOST_LOG_SEV(lg(), info) << "Feed controller ready — waiting for start signal";

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
