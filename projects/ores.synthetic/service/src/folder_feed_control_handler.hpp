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

#include "curve_feed_controller.hpp"
#include "feed_controller.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/market_feed_config_protocol.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.synthetic.api/feeds/feed_factory.hpp"
#include "ores.synthetic.api/feeds/ir_curve_template_resolver.hpp"
#include "ores.synthetic.core/repository/folder_repository.hpp"
#include "ores.synthetic.core/repository/fx_spot_generation_config_repository.hpp"
#include "ores.synthetic.core/repository/gmm_component_repository.hpp"
#include "ores.synthetic.core/repository/ir_curve_generation_config_process_parameter_value_repository.hpp"
#include "ores.synthetic.core/repository/ir_curve_generation_config_repository.hpp"
#include "ores.synthetic.core/repository/ir_curve_template_entry_repository.hpp"
#include "ores.synthetic.core/repository/market_data_generation_config_repository.hpp"
#include "ores.synthetic.core/repository/yield_curve_process_parameter_definition_repository.hpp"
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
 * The single place that turns "start everything under this folder" into a
 * sequence of producer starts: it resolves the folder subtree once and
 * dispatches every config row beneath it — of every asset class — through
 * the producer factory (feed_factory) to the per-kind controller, so Qt,
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
                                std::shared_ptr<curve_feed_controller> curve_ctrl,
                                ores::nats::service::nats_client& auth_nats,
                                ores::database::context ctx,
                                std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctrl_(std::move(ctrl))
        , curve_ctrl_(std::move(curve_ctrl))
        , auth_nats_(auth_nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

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

        const auto folder_ids = resolve_subtree(ctx, folder_id);
        start_feeds_under_folder_response resp;
        resp.success = true;

        // Forwarded so delegated service-to-service lookups (vintage
        // resolution inside the IR producer builder) run in the caller's
        // own tenant/party context — without it, the lookups silently run
        // as this service's own system-tenant identity, which cannot see
        // another tenant's market_observation rows (RLS).
        const auto bearer = ores::nats::service::extract_bearer(msg);
        const ores::synthetic::feed::feed_build_context bctx{nats_, auth_nats_, bearer};
        const auto factory = ores::synthetic::feed::make_default_feed_factory();

        namespace repo = ores::synthetic::repository;
        repo::fx_spot_generation_config_repository fx_repo;
        repo::gmm_component_repository comp_repo;
        repo::ir_curve_generation_config_repository ir_repo;
        repo::ir_curve_template_entry_repository entry_repo;
        repo::ir_curve_generation_config_process_parameter_value_repository value_repo;
        repo::yield_curve_process_parameter_definition_repository definition_repo;
        repo::market_data_generation_config_repository feed_repo;

        const auto fxs = fx_repo.read_latest(ctx);
        const auto comps = comp_repo.read_latest(ctx);
        const auto ir_configs = ir_repo.read_latest(ctx);
        const auto entries = entry_repo.read_latest(ctx);
        const auto values = value_repo.read_latest(ctx);
        const auto definitions = definition_repo.read_latest(ctx);
        // Keyed by container id, not just checked for enabled/existence, so
        // each feed's binding_mode (bound/sandboxed) can be forwarded and
        // the container's enabled state checked — mirrors application.cpp's
        // auto_start_enabled_feeds.
        std::map<boost::uuids::uuid, ores::synthetic::domain::market_data_generation_config>
            containers;
        for (const auto& f : feed_repo.read_latest(ctx))
            containers.emplace(f.id, f);
        std::map<boost::uuids::uuid, std::vector<ores::synthetic::domain::gmm_component>> by_fx;
        for (const auto& c : comps)
            by_fx[c.fx_spot_config_id].push_back(c);
        std::map<boost::uuids::uuid, std::vector<ores::synthetic::domain::ir_curve_template_entry>>
            entries_by_config;
        for (const auto& e : entries)
            entries_by_config[e.ir_curve_config_id].push_back(e);
        std::map<
            boost::uuids::uuid,
            std::vector<ores::synthetic::domain::ir_curve_generation_config_process_parameter_value>>
            values_by_config;
        for (const auto& v : values)
            values_by_config[v.config_id].push_back(v);

        using ores::synthetic::feed::fx_spot_feed_build_input;
        using ores::synthetic::feed::ir_curve_feed_build_input;
        using ores::synthetic::feed::ir_curve_feed_kind;
        using ores::synthetic::feed::ir_curve_qualifier;
        using ores::synthetic::feed::ir_curve_tenor_convention_code;
        using ores::synthetic::feed::fx_spot_feed_kind;

        feed_kind_counts fx_counts;
        for (const auto& fx : fxs) {
            if (!fx.folder_id.has_value() || !folder_ids.contains(*fx.folder_id))
                continue;

            // Uniform startability across kinds: the row itself must be
            // enabled and its container must exist and be enabled — the
            // same gate the auto-start walk applies.
            const auto container = containers.find(fx.config_id);
            if (!fx.enabled || container == containers.end() || !container->second.enabled) {
                ++fx_counts.skipped;
                BOOST_LOG_SEV(folder_feed_control_handler_lg(), warn)
                    << "Skipping " << fx.ore_key << " under folder " << req->folder_id
                    << " — not enabled (config or container).";
                continue;
            }
            const auto it = by_fx.find(fx.id);
            if (it == by_fx.end() || it->second.empty()) {
                ++fx_counts.skipped;
                BOOST_LOG_SEV(folder_feed_control_handler_lg(), warn)
                    << "Skipping " << fx.ore_key << " under folder " << req->folder_id
                    << " — no GMM components.";
                continue;
            }
            const auto feed = factory.make(
                std::string(fx_spot_feed_kind),
                bctx,
                fx_spot_feed_build_input{fx, it->second, container->second.binding_mode});
            if (ctrl_->add(std::move(feed), container->second.binding_mode))
                ++fx_counts.started;
            else
                ++fx_counts.already_running;
        }

        feed_kind_counts ir_counts;
        for (const auto& cfg : ir_configs) {
            if (!cfg.folder_id.has_value() || !folder_ids.contains(*cfg.folder_id))
                continue;

            const auto container = containers.find(cfg.config_id);
            if (!cfg.enabled || container == containers.end() || !container->second.enabled) {
                ++ir_counts.skipped;
                BOOST_LOG_SEV(folder_feed_control_handler_lg(), warn)
                    << "Skipping IR curve config " << cfg.currency_code << "/"
                    << cfg.index_family << " under folder " << req->folder_id
                    << " — not enabled (config or container).";
                continue;
            }
            const auto it = entries_by_config.find(cfg.id);
            if (it == entries_by_config.end() || it->second.empty()) {
                ++ir_counts.skipped;
                BOOST_LOG_SEV(folder_feed_control_handler_lg(), warn)
                    << "Skipping IR curve config " << cfg.currency_code << "/"
                    << cfg.index_family << " under folder " << req->folder_id
                    << " — no template entries.";
                continue;
            }
            const auto vit = values_by_config.find(cfg.id);
            if (vit == values_by_config.end() || vit->second.empty()) {
                ++ir_counts.skipped;
                BOOST_LOG_SEV(folder_feed_control_handler_lg(), warn)
                    << "Skipping IR curve config " << cfg.currency_code << "/"
                    << cfg.index_family << " under folder " << req->folder_id
                    << " — no parameter value rows.";
                continue;
            }

            // The context is per config: the series qualifier selects the
            // tenor convention (the FOMC grid resolves under
            // RATES_SPOT_FOMC).
            const auto refctx = ores::synthetic::feed::build_ir_curve_refdata_context(
                ctx, ir_curve_tenor_convention_code(ir_curve_qualifier(cfg)));
            if (!refctx) {
                ++ir_counts.skipped;
                BOOST_LOG_SEV(folder_feed_control_handler_lg(), warn)
                    << "Skipping IR curve config " << cfg.currency_code << "/"
                    << cfg.index_family << " under folder " << req->folder_id
                    << " — tenor convention not found.";
                continue;
            }

            try {
                const auto feed = factory.make(
                    std::string(ir_curve_feed_kind),
                    bctx,
                    ir_curve_feed_build_input{
                        cfg, it->second, vit->second, definitions, *refctx});
                const auto holder =
                    curve_ctrl_->running_source_name_for_qualifier(feed->qualifier(),
                                                                   feed->role());
                if (holder && *holder == cfg.source_name) {
                    ++ir_counts.already_running;
                    continue;
                }
                if (holder) {
                    ++ir_counts.skipped;
                    BOOST_LOG_SEV(folder_feed_control_handler_lg(), warn)
                        << "Skipping IR curve config " << cfg.currency_code << "/"
                        << cfg.index_family << " under folder " << req->folder_id
                        << " — qualifier already held by running feed '" << *holder << "'.";
                    continue;
                }
                if (curve_ctrl_->add(std::move(feed)))
                    ++ir_counts.started;
            } catch (const std::exception& e) {
                ++ir_counts.skipped;
                BOOST_LOG_SEV(folder_feed_control_handler_lg(), warn)
                    << "Skipping IR curve config " << cfg.currency_code << "/"
                    << cfg.index_family << " under folder " << req->folder_id
                    << " — failed to start: " << e.what();
            }
        }

        resp.started = fx_counts.started + ir_counts.started;
        resp.already_running = fx_counts.already_running + ir_counts.already_running;
        resp.skipped = fx_counts.skipped + ir_counts.skipped;
        resp.by_kind.emplace(std::string(fx_spot_feed_kind), fx_counts);
        resp.by_kind.emplace(std::string(ir_curve_feed_kind), ir_counts);
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

        const auto folder_ids = resolve_subtree(ctx, folder_id);

        namespace repo = ores::synthetic::repository;
        repo::fx_spot_generation_config_repository fx_repo;
        repo::ir_curve_generation_config_repository ir_repo;
        const auto fxs = fx_repo.read_latest(ctx);
        const auto ir_configs = ir_repo.read_latest(ctx);

        stop_feeds_under_folder_response resp;
        resp.success = true;
        int fx_stopped = 0;
        for (const auto& fx : fxs) {
            if (!fx.folder_id.has_value() || !folder_ids.contains(*fx.folder_id))
                continue;
            fx_stopped += static_cast<int>(ctrl_->stop(fx.source_name));
        }
        int ir_stopped = 0;
        for (const auto& cfg : ir_configs) {
            if (!cfg.folder_id.has_value() || !folder_ids.contains(*cfg.folder_id))
                continue;
            ir_stopped += static_cast<int>(curve_ctrl_->stop(cfg.source_name));
        }

        resp.stopped = fx_stopped + ir_stopped;
        resp.stopped_by_kind.emplace(std::string(ores::synthetic::feed::fx_spot_feed_kind),
                                     fx_stopped);
        resp.stopped_by_kind.emplace(std::string(ores::synthetic::feed::ir_curve_feed_kind),
                                     ir_stopped);
        resp.message = std::to_string(resp.stopped) + " feed(s) stopped";
        BOOST_LOG_SEV(folder_feed_control_handler_lg(), info)
            << msg.subject << " (folder=" << req->folder_id << ") — " << resp.message;
        reply(nats_, msg, resp);
    }

private:
    // Auth + the uniform permission gate shared by both verbs: every kind's
    // config family is readable by the caller, so one check covers the
    // whole subtree walk — no per-kind permission branching.
    std::optional<ores::database::context>
    authenticated_context(std::string_view verb, const ores::nats::message& msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            BOOST_LOG_SEV(folder_feed_control_handler_lg(), warn)
                << "Rejecting " << verb << " request: auth failed: "
                << static_cast<int>(ctx_expected.error());
            error_reply(nats_, msg, ctx_expected.error());
            return std::nullopt;
        }
        if (!has_permission(*ctx_expected, "synthetic::fx_spot_generation_configs:read") ||
            !has_permission(*ctx_expected, "synthetic::ir_curve_generation_configs:read")) {
            BOOST_LOG_SEV(folder_feed_control_handler_lg(), warn)
                << "Rejecting " << verb << " request: missing permission "
                   "synthetic::fx_spot_generation_configs:read or "
                   "synthetic::ir_curve_generation_configs:read.";
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

    // Every folder id in the subtree rooted at root_id, including root_id
    // itself — the set each config row's folder_id is matched against.
    static std::set<boost::uuids::uuid> resolve_subtree(const ores::database::context& ctx,
                                                        const boost::uuids::uuid& root_id) {
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
    std::shared_ptr<curve_feed_controller> curve_ctrl_;
    ores::nats::service::nats_client& auth_nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
