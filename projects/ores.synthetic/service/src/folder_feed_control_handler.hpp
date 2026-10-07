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
#ifndef ORES_SYNTHETIC_SERVICE_FOLDER_FEED_CONTROL_HANDLER_HPP
#define ORES_SYNTHETIC_SERVICE_FOLDER_FEED_CONTROL_HANDLER_HPP

#include "feed_controller.hpp"
#include "feed_kind_registry.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <map>
#include <memory>
#include <optional>
#include <set>

namespace ores::synthetic::service {

namespace {
inline auto& folder_feed_control_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.synthetic.service.folder_feed_control_handler");
    return instance;
}
} // namespace

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::log_handler_entry;
using ores::service::messaging::reply;
using namespace ores::logging;

/**
 * @brief NATS handler for folder-scoped feed start/stop control messages.
 *
 * The single place that turns "start everything under this folder" into a
 * sequence of producer starts: it resolves the folder subtree once and
 * dispatches every config row beneath it — of every asset class — through
 * the feed kind registry's factory to the feed controller, so Qt,
 * ores.shell, and a wt workflow step all get the same behaviour from one
 * request instead of each re-implementing the tree-walk-and-fan-out
 * themselves.
 *
 * Requires a valid JWT: the folder/feed rows are tenant+party scoped (RLS),
 * so the queries must run in the caller's own tenant context via
 * make_request_context — the service's own startup ctx is tenant-neutral
 * and would silently see nothing for any real tenant.
 */
class folder_feed_control_handler {
public:
    folder_feed_control_handler(ores::nats::service::client& nats,
                                std::shared_ptr<feed_controller> ctrl,
                                ores::nats::service::nats_client& auth_nats,
                                ores::database::context ctx,
                                std::optional<ores::security::jwt::jwt_authenticator> verifier,
                                const feed_kind_registry& registry)
        : nats_(nats)
        , ctrl_(std::move(ctrl))
        , auth_nats_(auth_nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier))
        , registry_(registry) {}

    void start(ores::nats::message msg) {
        using namespace ores::marketdata::messaging;
        [[maybe_unused]] const auto cid = log_handler_entry(folder_feed_control_handler_lg(), msg);

        auto ctx_expected = authenticated_context("start_folder", msg);
        if (!ctx_expected)
            return;
        const auto& ctx = *ctx_expected;

        auto req = decode<start_feeds_under_folder_request>(msg);
        boost::uuids::uuid folder_id;
        if (!req || !parse_folder_id(req->folder_id, folder_id)) {
            reply(nats_,
                  msg,
                  start_feeds_under_folder_response{.success = false,
                                                    .message = "Malformed or missing folder_id"});
            return;
        }

        const auto folder_ids = registry_.folder_subtree(ctx, folder_id);
        start_feeds_under_folder_response resp;
        resp.success = true;
        // Every registered kind is always present, whether or not this folder
        // holds a row of it.
        for (const auto* k : registry_.all())
            resp.by_kind.emplace(k->kind, feed_kind_counts{});

        // Forwarded so delegated service-to-service lookups (vintage
        // resolution inside the IR producer builder) run in the caller's
        // own tenant/party context — without it, the lookups silently run
        // as this service's own system-tenant identity, which cannot see
        // another tenant's market_observation rows (RLS).
        const auto bearer = ores::nats::service::extract_bearer(msg);
        const ores::synthetic::feed::feed_build_context bctx{nats_, auth_nats_, bearer};

        for (const auto& target : registry_.targets(ctx)) {
            const auto& c = target.row.candidate;
            auto& counts = resp.by_kind.at(target.row.kind->kind);

            if (!c.folder_id || !folder_ids.contains(*c.folder_id))
                continue;
            const auto skip = [&](const std::string& reason) {
                ++counts.skipped;
                BOOST_LOG_SEV(folder_feed_control_handler_lg(), warn)
                    << "Skipping " << c.display_name << " under folder " << req->folder_id << " — "
                    << reason;
            };
            try {
                auto attempt = registry_.make_feed(target, bctx);
                if (!attempt.feed) {
                    skip(attempt.failure);
                    continue;
                }
                std::string conflicting_source_name;
                if (ctrl_->add(std::move(attempt.feed),
                               target.binding_mode,
                               bearer,
                               &conflicting_source_name))
                    ++counts.started;
                else if (conflicting_source_name.empty())
                    // A concurrent cascade started the same config between
                    // this loop's checks and the add.
                    ++counts.already_running;
                else
                    skip("already held by running feed '" + conflicting_source_name + "'.");
            } catch (const std::exception& e) {
                skip(std::string("failed to start: ") + e.what());
            }
        }

        for (const auto& [kind, counts] : resp.by_kind) {
            resp.started += counts.started;
            resp.already_running += counts.already_running;
            resp.skipped += counts.skipped;
        }
        resp.message = std::to_string(resp.started) + " started, " +
                       std::to_string(resp.already_running) + " already running, " +
                       std::to_string(resp.skipped) + " skipped";
        BOOST_LOG_SEV(folder_feed_control_handler_lg(), info)
            << msg.subject << " (folder=" << req->folder_id << ") — " << resp.message;
        reply(nats_, msg, resp);
    }

    void stop(ores::nats::message msg) {
        using namespace ores::marketdata::messaging;
        [[maybe_unused]] const auto cid = log_handler_entry(folder_feed_control_handler_lg(), msg);

        auto ctx_expected = authenticated_context("stop_folder", msg);
        if (!ctx_expected)
            return;
        const auto& ctx = *ctx_expected;

        auto req = decode<stop_feeds_under_folder_request>(msg);
        boost::uuids::uuid folder_id;
        if (!req || !parse_folder_id(req->folder_id, folder_id)) {
            reply(nats_,
                  msg,
                  stop_feeds_under_folder_response{.success = false,
                                                   .message = "Malformed or missing folder_id"});
            return;
        }

        const auto folder_ids = registry_.folder_subtree(ctx, folder_id);

        stop_feeds_under_folder_response resp;
        resp.success = true;
        for (const auto* k : registry_.all())
            resp.stopped_by_kind.emplace(k->kind, 0);

        // rows(), not targets(): stopping reads no container.
        for (const auto& row : registry_.rows(ctx)) {
            const auto& c = row.candidate;
            if (!c.folder_id || !folder_ids.contains(*c.folder_id))
                continue;
            const auto stopped = static_cast<int>(ctrl_->stop(c.source_name));
            resp.stopped_by_kind.at(row.kind->kind) += stopped;
            resp.stopped += stopped;
        }

        resp.message = std::to_string(resp.stopped) + " feed(s) stopped";
        BOOST_LOG_SEV(folder_feed_control_handler_lg(), info)
            << msg.subject << " (folder=" << req->folder_id << ") — " << resp.message;
        reply(nats_, msg, resp);
    }

private:
    // Auth + the uniform permission gate shared by both verbs: every kind's
    // config family is readable by the caller, so one check covers the
    // whole subtree walk — no per-kind permission branching.
    std::optional<ores::database::context> authenticated_context(std::string_view verb,
                                                                 const ores::nats::message& msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            BOOST_LOG_SEV(folder_feed_control_handler_lg(), warn)
                << "Rejecting " << verb
                << " request: auth failed: " << static_cast<int>(ctx_expected.error());
            error_reply(nats_, msg, ctx_expected.error());
            return std::nullopt;
        }
        if (!registry_.permits_all_configs(*ctx_expected)) {
            BOOST_LOG_SEV(folder_feed_control_handler_lg(), warn)
                << "Rejecting " << verb
                << " request: missing a registered kind's config read permission.";
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return std::nullopt;
        }
        return std::optional<ores::database::context>(std::move(*ctx_expected));
    }

    static bool parse_folder_id(const std::string& s, boost::uuids::uuid& out) {
        if (s.empty())
            return false;
        try {
            out = boost::lexical_cast<boost::uuids::uuid>(s);
            return true;
        } catch (...) {
            return false;
        }
    }

    ores::nats::service::client& nats_;
    std::shared_ptr<feed_controller> ctrl_;
    ores::nats::service::nats_client& auth_nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
    const feed_kind_registry& registry_;
};

}

#endif
