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

#include "feed_controller.hpp"
#include "feed_kind_registry.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.logging/make_logger.hpp"
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
 * starts the producer via the factory. Missing, disabled, and vintage-data-missing outcomes use
 * one message shape regardless of kind.
 */
class feed_config_handler {
public:
    feed_config_handler(ores::nats::service::client& nats,
                        ores::nats::service::nats_client& auth_nats,
                        std::shared_ptr<feed_controller> ctrl,
                        ores::database::context ctx,
                        std::optional<ores::security::jwt::jwt_authenticator> verifier,
                        const feed_kind_registry& registry)
        : nats_(nats)
        , auth_nats_(auth_nats)
        , ctrl_(std::move(ctrl))
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier))
        , registry_(registry) {}

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

        if (!has_permission(req_ctx, target->row.kind->config_permission)) {
            BOOST_LOG_SEV(feed_config_handler_lg(), warn)
                << "Rejecting start request: missing permission "
                << target->row.kind->config_permission << ".";
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        start_feed_response resp;
        const auto bearer = ores::nats::service::extract_bearer(msg);
        const ores::synthetic::feed::feed_build_context bctx{nats_, auth_nats_, bearer};
        try {
            auto attempt = registry_.make_feed(*target, bctx);
            if (!attempt.feed) {
                resp.success = false;
                resp.message = std::move(attempt.failure);
                reply(nats_, msg, resp);
                return;
            }
            reply_start_outcome(msg, resp, std::move(attempt.feed), target->binding_mode, bearer);
        } catch (const ores::synthetic::feed::vintage_data_missing_error& e) {
            resp.success = false;
            resp.message = e.what();
            BOOST_LOG_SEV(feed_config_handler_lg(), warn)
                << msg.subject << " — feed rejected: " << resp.message;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            resp.success = false;
            resp.message = std::string("Failed to start feed: ") + e.what();
            BOOST_LOG_SEV(feed_config_handler_lg(), error) << msg.subject << " — " << resp.message;
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

        auto req = decode<stop_feed_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(feed_config_handler_lg(), warn)
                << msg.subject << " — empty or malformed stop body; rejecting";
            reply(nats_,
                  msg,
                  stop_feed_response{.success = false, .message = "Malformed stop request"});
            return;
        }

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

        const auto stopped = ctrl_->stop(source_name);
        stop_feed_response resp;
        resp.success = true; // idempotent — 0 stopped means it was already stopped
        resp.message = std::to_string(stopped) + " feed(s) stopped";
        BOOST_LOG_SEV(feed_config_handler_lg(), info)
            << msg.subject << " — " << resp.message
            << (source_name.empty() ? " (all)" : " (" + source_name + ")");
        reply(nats_, msg, resp);
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

        list_feeds_response resp;
        // The empty kind scopes to nothing: every running feed, every kind.
        resp.running_source_names = ctrl_->list();
        resp.success = true;
        BOOST_LOG_SEV(feed_config_handler_lg(), info)
            << msg.subject << " — " << resp.running_source_names.size() << " feed(s) running";
        reply(nats_, msg, resp);
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

    // One start-result switch for every kind: the message shapes (started,
    // already running, conflict with the holding source_name) are uniform, so
    // the dispatch needs no per-kind branching.
    void reply_start_outcome(const ores::nats::message& msg,
                             ores::synthetic::messaging::start_feed_response& resp,
                             std::shared_ptr<ores::marketdata::domain::IFeed> feed,
                             ores::synthetic::domain::binding_mode binding_mode,
                             const std::string& bearer) {
        const auto source_name = feed->source_name();
        const auto conflict_key = feed->conflict_key();
        const auto result = ctrl_->start(std::move(feed), binding_mode, bearer);

        switch (result) {
            case feed_controller::start_result::started:
                resp.success = true;
                resp.message = "Feed started: " + source_name;
                break;
            case feed_controller::start_result::already_running:
                resp.success = true;
                resp.message = "Feed already running: " + source_name;
                break;
            case feed_controller::start_result::qualifier_conflict: {
                const auto conflicting = ctrl_->running_source_name_for_conflict_key(conflict_key);
                resp.success = false;
                resp.message = "Already running as '" + conflicting.value_or("<unknown>") +
                               "' — stop it first before starting '" + source_name + "'.";
                break;
            }
            case feed_controller::start_result::vintage_data_missing:
                // The builders resolve vintage at construction and throw
                // vintage_data_missing_error; the on-demand start() itself
                // never returns this result.
                resp.success = false;
                resp.message = "Vintage data missing for: " + source_name;
                break;
        }
        BOOST_LOG_SEV(feed_config_handler_lg(), info) << msg.subject << " — " << resp.message;
        reply(nats_, msg, resp);
    }

    ores::nats::service::client& nats_;
    ores::nats::service::nats_client& auth_nats_;
    std::shared_ptr<feed_controller> ctrl_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
    const feed_kind_registry& registry_;
};

}

#endif
