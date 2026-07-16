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
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/market_feed_config_protocol.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.synthetic.core/repository/folder_repository.hpp"
#include "ores.synthetic.core/repository/fx_spot_generation_config_repository.hpp"
#include "ores.synthetic.core/repository/gmm_component_repository.hpp"
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
using ores::service::messaging::has_permission;
using ores::service::messaging::log_handler_entry;
using ores::service::messaging::reply;
using namespace ores::logging;

/**
 * @brief NATS handler for folder-scoped feed start/stop control messages.
 *
 * Unlike market_feed_config_handler (pure control-plane, no DB access),
 * this handler needs the database to resolve a folder subtree and the
 * fx_spot_generation_config/gmm_component rows beneath it -- it is the
 * single place that turns "start everything under this folder" into a
 * sequence of feed_controller::start() calls, so Qt, ores.shell, and a wt
 * workflow step all get the same behaviour from one request instead of
 * each re-implementing the tree-walk-and-fan-out themselves.
 *
 * Requires a valid JWT: the folder/feed rows are tenant+party scoped (RLS),
 * so the query must run in the caller's own tenant context via
 * make_request_context -- the service's own startup ctx is tenant-neutral
 * and would silently see nothing for any real tenant.
 */
class folder_feed_control_handler {
public:
    folder_feed_control_handler(ores::nats::service::client& nats,
                                std::shared_ptr<feed_controller> ctrl,
                                ores::database::context ctx,
                                std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctrl_(std::move(ctrl))
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void start(ores::nats::message msg) {
        using namespace ores::marketdata::messaging;
        [[maybe_unused]] const auto cid = log_handler_entry(folder_feed_control_handler_lg(), msg);

        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            BOOST_LOG_SEV(folder_feed_control_handler_lg(), warn)
                << "Rejecting start_folder request: auth failed: "
                << static_cast<int>(ctx_expected.error());
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "synthetic::fx_spot_generation_configs:read")) {
            BOOST_LOG_SEV(folder_feed_control_handler_lg(), warn)
                << "Rejecting start_folder request: missing permission "
                   "synthetic::fx_spot_generation_configs:read.";
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        auto req = decode<start_feeds_under_folder_request>(msg);
        boost::uuids::uuid folder_id;
        if (!req || !parse_folder_id(req->folder_id, folder_id)) {
            reply(nats_,
                  msg,
                  start_feeds_under_folder_response{.success = false,
                                                    .message = "Malformed or missing folder_id"});
            return;
        }

        const auto folder_ids = resolve_subtree(ctx, folder_id);
        start_feeds_under_folder_response resp;
        resp.success = true;

        namespace repo = ores::synthetic::repository;
        repo::fx_spot_generation_config_repository fx_repo;
        repo::gmm_component_repository comp_repo;

        const auto fxs = fx_repo.read_latest(ctx);
        const auto comps = comp_repo.read_latest(ctx);
        std::map<boost::uuids::uuid, std::vector<ores::synthetic::domain::gmm_component>> by_fx;
        for (const auto& c : comps)
            by_fx[c.fx_spot_config_id].push_back(c);

        for (const auto& fx : fxs) {
            if (!fx.folder_id.has_value() || !folder_ids.contains(*fx.folder_id))
                continue;

            const auto it = by_fx.find(fx.id);
            if (it == by_fx.end() || it->second.empty()) {
                BOOST_LOG_SEV(folder_feed_control_handler_lg(), warn)
                    << "Skipping " << fx.ore_key << " under folder " << req->folder_id
                    << " — no GMM components.";
                ++resp.skipped;
                continue;
            }
            std::vector<double> means, stdevs, weights;
            for (const auto& c : it->second) {
                means.push_back(c.mean);
                stdevs.push_back(c.stdev);
                weights.push_back(c.weight);
            }
            const auto r = ctrl_->start(fx.ore_key,
                                        fx.source_name,
                                        std::move(means),
                                        std::move(stdevs),
                                        std::move(weights),
                                        fx.gmm_initial_price,
                                        static_cast<double>(fx.ticks_per_hour),
                                        fx.process_type,
                                        fx.vintage_source,
                                        fx.vintage_date);
            using sr = feed_controller::start_result;
            switch (r) {
                case sr::started: ++resp.started; break;
                case sr::already_running: ++resp.already_running; break;
                case sr::vintage_data_missing: ++resp.skipped; break;
            }
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

        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            BOOST_LOG_SEV(folder_feed_control_handler_lg(), warn)
                << "Rejecting stop_folder request: auth failed: "
                << static_cast<int>(ctx_expected.error());
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "synthetic::fx_spot_generation_configs:read")) {
            BOOST_LOG_SEV(folder_feed_control_handler_lg(), warn)
                << "Rejecting stop_folder request: missing permission "
                   "synthetic::fx_spot_generation_configs:read.";
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        auto req = decode<stop_feeds_under_folder_request>(msg);
        boost::uuids::uuid folder_id;
        if (!req || !parse_folder_id(req->folder_id, folder_id)) {
            reply(nats_,
                  msg,
                  stop_feeds_under_folder_response{.success = false,
                                                   .message = "Malformed or missing folder_id"});
            return;
        }

        const auto folder_ids = resolve_subtree(ctx, folder_id);

        namespace repo = ores::synthetic::repository;
        repo::fx_spot_generation_config_repository fx_repo;
        const auto fxs = fx_repo.read_latest(ctx);

        stop_feeds_under_folder_response resp;
        resp.success = true;
        for (const auto& fx : fxs) {
            if (!fx.folder_id.has_value() || !folder_ids.contains(*fx.folder_id))
                continue;
            resp.stopped += static_cast<int>(ctrl_->stop(fx.source_name));
        }

        resp.message = std::to_string(resp.stopped) + " feed(s) stopped";
        BOOST_LOG_SEV(folder_feed_control_handler_lg(), info)
            << msg.subject << " (folder=" << req->folder_id << ") — " << resp.message;
        reply(nats_, msg, resp);
    }

private:
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

    // Every folder id in the subtree rooted at root_id, including root_id
    // itself — the set fx_spot_generation_config::folder_id is matched
    // against.
    static std::set<boost::uuids::uuid>
    resolve_subtree(const ores::database::context& ctx, const boost::uuids::uuid& root_id) {
        namespace repo = ores::synthetic::repository;
        repo::folder_repository folder_repo;
        const auto rows = folder_repo.get_hierarchy(ctx, root_id, false);
        std::set<boost::uuids::uuid> ids;
        for (const auto& row : rows)
            ids.insert(row.id);
        return ids;
    }

    ores::nats::service::client& nats_;
    std::shared_ptr<feed_controller> ctrl_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
