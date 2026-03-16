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
#include "ores.variability.service/app/application.hpp"

#include <chrono>
#include <span>
#include <csignal>
#include <functional>
#include <optional>
#include <boost/asio/signal_set.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/throw_exception.hpp>
#include <boost/uuid/string_generator.hpp>
#include "ores.database/service/context_factory.hpp"
#include "ores.utility/version/version.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.variability.service/app/application_exception.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.variability/messaging/registrar.hpp"
#include <boost/json.hpp>
#include "ores.security/jwt/jwt_authenticator.hpp"

namespace ores::variability::service::app {

using namespace ores::logging;

ores::database::context application::make_context(
    const ores::database::database_options& db_opts) {
    using ores::database::context_factory;

    context_factory::configuration cfg {
        .database_options = db_opts,
        .pool_size = 4,
        .num_attempts = 10,
        .wait_time_in_seconds = 1
    };

    return context_factory::make_context(cfg);
}

application::application() = default;

boost::asio::awaitable<void>
application::run(boost::asio::io_context& io_ctx,
    const config::options& cfg) const {

    BOOST_LOG_SEV(lg(), info) << ores::utility::version::format_startup_message(
        "ores.variability.service", 0, 1);

    ores::nats::service::client nats(cfg.nats);
    nats.connect();
    BOOST_LOG_SEV(lg(), info) << "Connected to NATS: " << cfg.nats.url
                              << " (namespace: '"
                              << (cfg.nats.subject_prefix.empty() ? "(none)" : cfg.nats.subject_prefix)
                              << "')";

    auto ctx = make_context(cfg.database);
    std::optional<ores::security::jwt::jwt_authenticator> verifier;
    {
        std::string pub_key;
        try {
            const std::string jwks_req = "{}";
            const auto* p =
                reinterpret_cast<const std::byte*>(jwks_req.data());
            auto reply_msg = nats.request_sync("iam.v1.auth.jwks",
                std::span<const std::byte>(p, jwks_req.size()),
                {}, std::chrono::seconds(10));
            const std::string_view sv(
                reinterpret_cast<const char*>(reply_msg.data.data()),
                reply_msg.data.size());
            auto json = boost::json::parse(sv);
            pub_key = std::string(json.at("public_key").as_string());
            BOOST_LOG_SEV(lg(), info) << "Fetched JWKS public key from IAM";
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn)
                << "Failed to fetch JWKS, JWT validation disabled: "
                << e.what();
        }
        if (!pub_key.empty()) {
            verifier =
                ores::security::jwt::jwt_authenticator::create_rs256_verifier(
                    pub_key);
        }
    }
    using context_extractor_fn = ores::variability::messaging::registrar::context_extractor_fn;
    context_extractor_fn context_extractor;
    if (verifier.has_value()) {
        context_extractor = [verifier, base_ctx = ctx](
            const ores::nats::message& msg) -> std::optional<ores::database::context> {
            auto it = msg.headers.find("Authorization");
            if (it == msg.headers.end()) return base_ctx;
            const auto& val = it->second;
            if (!val.starts_with("Bearer ")) return base_ctx;
            const auto token = val.substr(7);
            auto claims = verifier->validate(token);
            if (!claims) return std::nullopt;
            const auto tenant_id_str = claims->tenant_id.value_or("");
            if (tenant_id_str.empty()) return base_ctx;
            auto tid_result =
                ores::utility::uuid::tenant_id::from_string(tenant_id_str);
            if (!tid_result) return base_ctx;
            const auto& tid = *tid_result;
            if (!claims->party_id || claims->party_id->empty())
                return base_ctx.with_tenant(tid, claims->username.value_or(""));
            try {
                boost::uuids::string_generator sg;
                boost::uuids::uuid party_id = sg(*claims->party_id);
                std::vector<boost::uuids::uuid> visible_ids;
                for (const auto& pid_str : claims->visible_party_ids)
                    visible_ids.push_back(sg(pid_str));
                return base_ctx.with_party(tid, party_id,
                    std::move(visible_ids), claims->username.value_or(""));
            } catch (...) {
                return base_ctx.with_tenant(tid, claims->username.value_or(""));
            }
        };
    }
    auto subs = ores::variability::messaging::registrar::register_handlers(
        nats, std::move(ctx), std::move(context_extractor));
    BOOST_LOG_SEV(lg(), info) << "Registered " << subs.size() << " subscription(s).";

    BOOST_LOG_SEV(lg(), info) << "Service ready. Waiting for requests...";
    boost::asio::signal_set signals(io_ctx, SIGINT, SIGTERM);
    co_await signals.async_wait(boost::asio::use_awaitable);

    BOOST_LOG_SEV(lg(), info) << "Shutdown signal received. Draining...";
    nats.drain();
    BOOST_LOG_SEV(lg(), info) << "Shutdown complete: ores.variability.service";
    co_return;
}

}
