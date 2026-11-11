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
#include "ores.marketdata.official/app/application.hpp"
#include "ores.http.client/net/transport.hpp"
#include "ores.marketdata.official/app/official_adapter.hpp"
#include "ores.marketdata.official/domain/banxico.hpp"
#include "ores.marketdata.official/domain/bcb.hpp"
#include "ores.marketdata.official/domain/bcch.hpp"
#include "ores.marketdata.official/domain/boc.hpp"
#include "ores.marketdata.official/domain/boe.hpp"
#include "ores.marketdata.official/domain/boj.hpp"
#include "ores.marketdata.official/domain/ecb.hpp"
#include "ores.marketdata.official/domain/frankfurter.hpp"
#include "ores.marketdata.official/domain/norges_bank.hpp"
#include "ores.marketdata.official/domain/nyfed.hpp"
#include "ores.marketdata.official/domain/rba.hpp"
#include "ores.marketdata.official/domain/riksbank.hpp"
#include "ores.marketdata.official/domain/secret.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.service/service/domain_service_runner.hpp"
#include "ores.service/service/heartbeat_publisher.hpp"
#include "ores.utility/version/version.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <utility>

namespace ores::marketdata::official::app {

using namespace ores::logging;

namespace {

constexpr std::string_view service_name = "ores.marketdata.official";
constexpr std::string_view service_version = ORES_VERSION;

}

boost::asio::awaitable<void> application::run(boost::asio::io_context& io_ctx,
                                              const config::options& cfg) const {
    BOOST_LOG_SEV(lg(), info) << ores::utility::version::format_startup_message(
        std::string(service_name), 0, 1);
    BOOST_LOG_SEV(lg(), info) << "Frankfurter : " << cfg.frankfurter_url;
    BOOST_LOG_SEV(lg(), info) << "New York Fed: " << cfg.nyfed_url;
    BOOST_LOG_SEV(lg(), info) << "ECB         : " << cfg.ecb_url;
    BOOST_LOG_SEV(lg(), info) << "BoE         : " << cfg.boe_url;
    BOOST_LOG_SEV(lg(), info) << "BoC         : " << cfg.boc_url;
    BOOST_LOG_SEV(lg(), info) << "BoJ         : " << cfg.boj_url;
    BOOST_LOG_SEV(lg(), info) << "Norges Bank : " << cfg.norges_bank_url;
    BOOST_LOG_SEV(lg(), info) << "Riksbank    : " << cfg.riksbank_url;
    BOOST_LOG_SEV(lg(), info) << "RBA         : " << cfg.rba_url;
    BOOST_LOG_SEV(lg(), info) << "BCB         : " << cfg.bcb_url;
    BOOST_LOG_SEV(lg(), info) << "Banxico     : " << cfg.banxico_url;
    BOOST_LOG_SEV(lg(), info) << "BCCh        : " << cfg.bcch_url;
    BOOST_LOG_SEV(lg(), info) << "Sources     : " << cfg.sources.size();
    BOOST_LOG_SEV(lg(), info) << "Backfill    : " << cfg.backfill_days << " days";
    BOOST_LOG_SEV(lg(), info) << "Poll        : every " << cfg.poll_minutes << " minutes";

    ores::nats::service::client nats(cfg.nats);
    nats.connect();

    http::client::net::beast_transport transport;
    domain::frankfurter_fetcher frankfurter(transport, cfg.frankfurter_url);
    domain::nyfed_fetcher nyfed(transport, cfg.nyfed_url);
    domain::ecb_fetcher ecb(transport, cfg.ecb_url);
    domain::boe_fetcher boe(transport, cfg.boe_url);
    domain::boc_fetcher boc(transport, cfg.boc_url);
    domain::boj_fetcher boj(transport, cfg.boj_url);
    domain::norges_bank_fetcher norges_bank(transport, cfg.norges_bank_url);
    auto riksbank_key = domain::resolve_secret("env:ORES_MARKETDATA_RIKSBANK_API_KEY");
    if (!riksbank_key)
        BOOST_LOG_SEV(lg(), info) << "The Riksbank is read without a key, at its lower quota: "
                                  << riksbank_key.error();
    domain::riksbank_fetcher riksbank(
        transport,
        cfg.riksbank_url,
        riksbank_key ? std::optional<std::string>(std::move(*riksbank_key)) : std::nullopt);
    domain::rba_fetcher rba(transport, cfg.rba_url);
    domain::bcb_fetcher bcb(transport, cfg.bcb_url);
    domain::banxico_fetcher banxico(
        transport, cfg.banxico_url, domain::resolve_secret("env:ORES_MARKETDATA_BANXICO_API_KEY"));
    const auto bcch_account = []() -> std::expected<domain::bcch_account, std::string> {
        auto user = domain::resolve_secret("env:ORES_MARKETDATA_BCCH_USER");
        if (!user)
            return std::unexpected(user.error());
        auto password = domain::resolve_secret("env:ORES_MARKETDATA_BCCH_PASSWORD");
        if (!password)
            return std::unexpected(password.error());
        return domain::bcch_account{std::move(*user), std::move(*password)};
    };
    domain::bcch_fetcher bcch(transport, cfg.bcch_url, bcch_account());
    adapter_timing timing;
    timing.poll_interval = std::chrono::minutes(cfg.poll_minutes);
    official_adapter adapter(nats,
                             cfg.sources,
                             {&frankfurter,
                              &nyfed,
                              &ecb,
                              &boe,
                              &boc,
                              &boj,
                              &norges_bank,
                              &riksbank,
                              &rba,
                              &bcb,
                              &banxico,
                              &bcch},
                             static_cast<int>(cfg.backfill_days),
                             timing);

    // Bound before the await, so no lambda of the call lives in this frame; see run().
    auto service_run = ores::service::service::run(
        io_ctx,
        nats,
        service_name,
        [&adapter](auto& /*n*/, auto /*verifier*/) { return adapter.subscribe(); },
        [&](boost::asio::io_context& ioc) {
            auto hb = std::make_shared<ores::service::service::heartbeat_publisher>(
                std::string(service_name), std::string(service_version), nats, std::string());
            boost::asio::co_spawn(ioc, [hb]() { return hb->run(); }, boost::asio::detached);
            adapter.start();
        },
        [&]() { adapter.stop(); });
    co_await std::move(service_run);
    co_return;
}

}
