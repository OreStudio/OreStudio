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
#ifndef ORES_SYNTHETIC_SERVICE_FEED_CONFIG_HANDLER_HPP
#define ORES_SYNTHETIC_SERVICE_FEED_CONFIG_HANDLER_HPP

#include "client_message.hpp"
#include "feed_kind_registry.hpp"
#include "generator_fleet.hpp"
#include "ores.analytics.quant/service/tick_scheduler.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/feed_start.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.synthetic.api/feeds/ir_curve_feed.hpp"
#include "ores.synthetic.api/messaging/feed_config_protocol.hpp"
#include <memory>
#include <optional>
#include <string>

namespace ores::synthetic::service {

namespace {
inline auto& feed_config_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.synthetic.service.feed_config_handler");
    return instance;
}

/// A run's start in the words of the wire, field for field.
inline ores::synthetic::messaging::feed_start
to_wire(const ores::marketdata::domain::feed_start& start) {
    return {.from_fixing = start.from_fixing,
            .item = start.item,
            .provider_code = start.provider_code,
            .vendor_id = start.vendor_id,
            .value = start.from_fixing ? start.value.to_string() : std::string{},
            .inverted = start.inverted,
            .fixing_date = start.fixing_date,
            .received_at = start.received_at,
            .window_days = start.window_days,
            .used = start.used,
            .hourly_spread = start.hourly_spread,
            .note = start.note};
}
} // namespace

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::log_handler_entry;
using ores::service::messaging::reply;
using namespace ores::logging;

/**
 * @brief NATS handler for per-config feed start/stop/list control messages, one for every asset
 * class.
 *
 * Replaces the per-kind handlers (market_feed_config_handler and ir_curve_feed_config_handler):
 * the client sends only a config_id and the feed kind registry resolves the config — whichever
 * kind it is, its children, and the refdata context — checks the resolved kind's permission, and
 * builds the producer. The runner then runs it as a manual run, which no demand set reconciles
 * away. Missing and disabled outcomes use one message shape regardless of
 * kind, and a start of a feed that already runs answers the same success its kind does.
 */
class feed_config_handler {
public:
    feed_config_handler(ores::nats::service::client& nats,
                        ores::nats::service::nats_client& auth_nats,
                        generator_fleet& generators,
                        ores::database::context ctx,
                        std::optional<ores::security::jwt::jwt_authenticator> verifier,
                        const feed_kind_registry& registry,
                        ores::analytics::quant::service::tick_scheduler& scheduler)
        : nats_(nats)
        , auth_nats_(auth_nats)
        , generators_(generators)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier))
        , registry_(registry)
        , scheduler_(scheduler) {}

    void start(ores::nats::message msg) {
        using namespace ores::synthetic::messaging;
        [[maybe_unused]] const auto cid = log_handler_entry(feed_config_handler_lg(), msg);

        auto ctx_expected = authenticated_context("start", msg);
        if (!ctx_expected)
            return;
        const auto& req_ctx = *ctx_expected;

        auto req = decode<start_feed_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(feed_config_handler_lg(), warn)
                << msg.subject << " — empty or malformed start body; rejecting";
            reply(nats_,
                  msg,
                  start_feed_response{.success = false, .message = "Malformed start request"});
            return;
        }

        const auto target = registry_.resolve(req_ctx, req->config_id);
        if (!target) {
            reply(nats_,
                  msg,
                  start_feed_response{.success = false,
                                      .message = "Feed config not found: " + req->config_id});
            return;
        }

        if (!has_permission(req_ctx, target->row.kind->config_permission) ||
            !feed_kind_registry::permits_run(req_ctx)) {
            BOOST_LOG_SEV(feed_config_handler_lg(), warn)
                << "Rejecting start request: missing permission "
                << target->row.kind->config_permission << " or " << run_permission << ".";
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        if (target->container_is_system && !feed_kind_registry::is_system_caller(req_ctx)) {
            reply(
                nats_,
                msg,
                start_feed_response{.success = false,
                                    .message = "A system feed starts only in the system tenant."});
            return;
        }

        start_feed_response resp;
        try {
            const ores::synthetic::feed::feed_build_context bctx{
                .nats = nats_, .auth_nats = auth_nats_, .scheduler = scheduler_};
            auto attempt = registry_.make_feed(*target, bctx);
            if (!attempt.feed) {
                resp.success = false;
                resp.message = std::move(attempt.failure);
                reply(nats_, msg, resp);
                return;
            }
            reply_start_outcome(msg, resp, req->config_id, std::move(attempt.feed));
        } catch (const std::exception& e) {
            resp.success = false;
            resp.message = client_message(e);
            BOOST_LOG_SEV(feed_config_handler_lg(), error)
                << msg.subject << " — failed to start: " << e.what();
            reply(nats_, msg, resp);
        }
    }

    void stop(ores::nats::message msg) {
        using namespace ores::synthetic::messaging;
        [[maybe_unused]] const auto cid = log_handler_entry(feed_config_handler_lg(), msg);

        auto ctx_expected = authenticated_context("stop", msg);
        if (!ctx_expected)
            return;
        const auto& req_ctx = *ctx_expected;

        if (!feed_kind_registry::permits_run(req_ctx)) {
            BOOST_LOG_SEV(feed_config_handler_lg(), warn)
                << "Rejecting stop request: missing permission " << run_permission << ".";
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        auto req = decode<stop_feed_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(feed_config_handler_lg(), warn)
                << msg.subject << " — empty or malformed stop body; rejecting";
            reply(nats_,
                  msg,
                  stop_feed_response{.success = false, .message = "Malformed stop request"});
            return;
        }

        // The registry reads the configured feeds and the runners stop them, so
        // either can throw. An exception leaving this handler reaches the
        // transport, which contains it but sends no reply, and the caller then
        // waits out its deadline for an answer this handler could have given.
        try {
            std::string source_name = req->source_name;
            if (!req->config_id.empty()) {
                // The registry resolves the config_id to its source_name server-side
                // so the client never needs the naming conventions.
                const auto target = registry_.resolve(req_ctx, req->config_id);
                if (!target) {
                    reply(
                        nats_,
                        msg,
                        stop_feed_response{.success = false,
                                           .message = "Feed config not found: " + req->config_id});
                    return;
                }
                source_name = target->row.candidate.source_name;
            }

            // Naming no feed stops every feed the service runs, those of other tenants included,
            // so only the system tenant may ask for it.
            if (source_name.empty() && !feed_kind_registry::is_system_caller(req_ctx)) {
                reply(nats_,
                      msg,
                      stop_feed_response{.success = false, .message = "Name the feed to stop."});
                return;
            }

            // A system feed runs once for every tenant, so only the system tenant stops it,
            // whether the caller names it by its config or by its source.
            if (!feed_kind_registry::is_system_caller(req_ctx)) {
                const auto system = registry_.system_containers(req_ctx);
                for (const auto& row : registry_.rows(req_ctx)) {
                    if (row.candidate.source_name == source_name &&
                        system.contains(row.candidate.container_id)) {
                        reply(nats_,
                              msg,
                              stop_feed_response{
                                  .success = false,
                                  .message = "A system feed stops only in the system tenant."});
                        return;
                    }
                }
            }

            const auto stopped =
                source_name.empty() ? generators_.stop_all() : generators_.stop_source(source_name);
            stop_feed_response resp;
            resp.success = true; // idempotent — 0 stopped means it was already stopped
            resp.message = std::to_string(stopped) + " feed(s) stopped";
            BOOST_LOG_SEV(feed_config_handler_lg(), info)
                << msg.subject << " — " << resp.message
                << (source_name.empty() ? " (all)" : " (" + source_name + ")");
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(feed_config_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, stop_feed_response{.success = false, .message = e.what()});
        }
    }

    void list(ores::nats::message msg) {
        using namespace ores::synthetic::messaging;
        [[maybe_unused]] const auto cid = log_handler_entry(feed_config_handler_lg(), msg);

        auto ctx_expected = authenticated_context("list", msg);
        if (!ctx_expected)
            return;
        const auto& req_ctx = *ctx_expected;

        // Every kind is listed, so the gate is the same uniform one the folder
        // cascade requires — the caller holds every registered kind's config
        // read permission.
        if (!registry_.permits_all_configs(req_ctx)) {
            BOOST_LOG_SEV(feed_config_handler_lg(), warn)
                << "Rejecting list request: missing a registered kind's config read permission.";
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        try {
            list_feeds_response resp;
            for (const auto& feed : generators_.running_feeds()) {
                running_feed one;
                one.source_name = feed.source_name;
                if (feed.start)
                    one.start = to_wire(*feed.start);
                resp.running_feeds.push_back(std::move(one));
            }
            resp.success = true;
            BOOST_LOG_SEV(feed_config_handler_lg(), info)
                << msg.subject << " — " << resp.running_feeds.size() << " feed(s) running";
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(feed_config_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            // This response carries no message field, so the reason is only in
            // the log; success=false is what the caller can act on.
            reply(nats_, msg, list_feeds_response{.success = false});
        }
    }

private:
    // Auth gate shared by every verb; the per-kind permission check happens
    // after resolution, since the caller's kind is not known until the
    // config_id is resolved.
    std::optional<ores::database::context> authenticated_context(std::string_view verb,
                                                                 const ores::nats::message& msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            BOOST_LOG_SEV(feed_config_handler_lg(), warn)
                << "Rejecting " << verb
                << " request: auth failed: " << static_cast<int>(ctx_expected.error());
            error_reply(nats_, msg, ctx_expected.error());
            return std::nullopt;
        }
        return std::optional<ores::database::context>(std::move(*ctx_expected));
    }

    // One start outcome for every kind: a started feed and a feed that already runs
    // are both a success, so the dispatch needs no per-kind branching.
    void reply_start_outcome(const ores::nats::message& msg,
                             ores::synthetic::messaging::start_feed_response& resp,
                             const std::string& feed_config_id,
                             std::shared_ptr<ores::marketdata::domain::i_feed> feed) {
        const auto source_name = feed->source_name();
        const auto started = generators_.start_manual(feed_config_id, std::move(feed));
        resp.success = true;
        resp.message =
            started ? "Feed started: " + source_name : "Feed already running: " + source_name;
        BOOST_LOG_SEV(feed_config_handler_lg(), info) << msg.subject << " — " << resp.message;
        reply(nats_, msg, resp);
    }

    ores::nats::service::client& nats_;
    ores::nats::service::nats_client& auth_nats_;
    generator_fleet& generators_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
    const feed_kind_registry& registry_;
    ores::analytics::quant::service::tick_scheduler& scheduler_;
};

}

#endif
