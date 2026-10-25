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
#ifndef ORES_SYNTHETIC_SERVICE_FEED_SELECTION_HANDLER_HPP
#define ORES_SYNTHETIC_SERVICE_FEED_SELECTION_HANDLER_HPP

#include "feed_controller.hpp"
#include "feed_kind_registry.hpp"
#include "feed_run_control.hpp"
#include "ores.analytics.quant/domain/market_regime.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.synthetic.api/feeds/feed_filter.hpp"
#include "ores.synthetic.api/messaging/feed_selection_protocol.hpp"
#include <exception>
#include <memory>
#include <optional>
#include <string>
#include <unordered_set>

namespace ores::synthetic::service {

namespace {
inline auto& feed_selection_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.synthetic.service.feed_selection_handler");
    return instance;
}
} // namespace

/**
 * @brief Starts and stops the feeds a person picked in the feed table, or the feeds its
 * filter matches.
 *
 * The request carries a filter and ids, never the feeds. The handler resolves them in
 * the caller's session with the filter that pages the table, so a start applies to what
 * the table shows. It then runs each feed through the same start and stop the folder
 * verbs use. Requires the read of every registered kind's feed configs, as they do.
 */
class feed_selection_handler {
public:
    feed_selection_handler(ores::nats::service::client& nats,
                           std::shared_ptr<feed_controller> ctrl,
                           ores::nats::service::nats_client& auth_nats,
                           ores::database::context ctx,
                           std::optional<ores::security::jwt::jwt_authenticator> verifier,
                           const feed_kind_registry& registry,
                           ores::analytics::quant::domain::market_regime* market = nullptr)
        : nats_(nats)
        , ctrl_(std::move(ctrl))
        , auth_nats_(auth_nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier))
        , registry_(registry)
        , market_(market) {}

    void start(ores::nats::message msg) {
        using namespace ores::synthetic::messaging;
        run<start_selected_feeds_request, start_selected_feeds_response>(std::move(msg), true);
    }

    void stop(ores::nats::message msg) {
        using namespace ores::synthetic::messaging;
        run<stop_selected_feeds_request, stop_selected_feeds_response>(std::move(msg), false);
    }

private:
    template <typename Request, typename Response>
    void run(ores::nats::message msg, bool starting) {
        using ores::service::messaging::decode;
        using ores::service::messaging::error_reply;
        using ores::service::messaging::reply;
        using namespace ores::logging;

        auto ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx) {
            error_reply(nats_, msg, ctx.error());
            return;
        }
        if (!registry_.permits_all_configs(*ctx) || !feed_kind_registry::permits_run(*ctx)) {
            BOOST_LOG_SEV(feed_selection_handler_lg(), warn)
                << "Rejecting " << msg.subject
                << ": missing a feed config read permission or the run permission.";
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        Response resp;
        const auto req = decode<Request>(msg);
        if (!req) {
            resp.message = "Failed to decode the request.";
            reply(nats_, msg, resp);
            return;
        }
        const ores::synthetic::feed::feed_filter filter{.folder_id = req->folder_id,
                                                        .provider_code = req->provider_code,
                                                        .asset_class = req->asset_class,
                                                        .search = req->search,
                                                        .state = req->state,
                                                        .should_run = req->should_run,
                                                        .feed_ids = req->feed_ids,
                                                        .excluded_ids = req->excluded_ids};
        if (const auto problem = ores::synthetic::feed::selection_problem(filter);
            !problem.empty()) {
            resp.message = problem;
            reply(nats_, msg, resp);
            return;
        }

        try {
            const auto wanted = matching_ids(*ctx, filter);
            const auto label = std::to_string(wanted.size()) + " selected feeds";
            if (starting) {
                const ores::synthetic::feed::feed_build_context build{
                    .nats = nats_, .auth_nats = auth_nats_, .market = market_};
                const auto outcome = start_feeds(
                    registry_,
                    *ctrl_,
                    *ctx,
                    build,
                    [&](const feed_start_target& t) {
                        return wanted.contains(t.row.candidate.feed_config_id);
                    },
                    label);
                resp.started = outcome.started;
                resp.already_running = outcome.already_running;
                resp.skipped = outcome.skipped;
                resp.message = std::to_string(resp.started) + " started, " +
                               std::to_string(resp.already_running) + " already running, " +
                               std::to_string(resp.skipped) + " skipped";
            } else {
                const auto outcome =
                    stop_feeds(registry_, *ctrl_, *ctx, [&](const feed_kind_row& row) {
                        return wanted.contains(row.candidate.feed_config_id);
                    });
                resp.stopped = outcome.stopped;
                resp.skipped = outcome.refused;
                resp.message = std::to_string(resp.stopped) + " stopped, " +
                               std::to_string(resp.skipped) + " skipped";
            }
            resp.success = true;
            BOOST_LOG_SEV(feed_selection_handler_lg(), info)
                << msg.subject << " (" << label << ") — " << resp.message;
        } catch (const std::exception& e) {
            resp.message = "The request failed. The service log says why.";
            BOOST_LOG_SEV(feed_selection_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
        }
        reply(nats_, msg, resp);
    }

    /// The ids of the feeds the filter means, as the caller's session sees them.
    std::unordered_set<std::string>
    matching_ids(const ores::database::context& ctx,
                 const ores::synthetic::feed::feed_filter& filter) const {
        const auto running = ctrl_->list();
        const auto rows = ores::database::repository::execute_raw_multi_column_query(
            ctx,
            "select id from (" + ores::synthetic::feed::feeds_query() + ") f where " +
                ores::synthetic::feed::where_clause(
                    filter, running, feed_kind_registry::is_system_caller(ctx)),
            feed_selection_handler_lg(),
            "Reading the selected feeds");
        std::unordered_set<std::string> out;
        out.reserve(rows.size());
        for (const auto& r : rows)
            if (r.front())
                out.insert(*r.front());
        return out;
    }

    ores::nats::service::client& nats_;
    std::shared_ptr<feed_controller> ctrl_;
    ores::nats::service::nats_client& auth_nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
    const feed_kind_registry& registry_;
    ores::analytics::quant::domain::market_regime* market_;
};

}

#endif
