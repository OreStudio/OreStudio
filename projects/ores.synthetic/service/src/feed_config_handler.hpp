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
#include "ores.database/service/tenant_context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.synthetic.api/feeds/feed_factory.hpp"
#include "ores.synthetic.api/feeds/ir_curve_feed.hpp"
#include "ores.synthetic.api/feeds/ir_curve_template_resolver.hpp"
#include "ores.synthetic.api/messaging/feed_config_protocol.hpp"
#include "ores.synthetic.core/repository/fx_spot_generation_config_repository.hpp"
#include "ores.synthetic.core/repository/gmm_component_repository.hpp"
#include "ores.synthetic.core/repository/ir_curve_generation_config_process_parameter_value_repository.hpp"
#include "ores.synthetic.core/repository/ir_curve_generation_config_repository.hpp"
#include "ores.synthetic.core/repository/ir_curve_template_entry_repository.hpp"
#include "ores.synthetic.core/repository/market_data_generation_config_repository.hpp"
#include "ores.synthetic.core/repository/yield_curve_process_parameter_definition_repository.hpp"
#include <boost/uuid/uuid_io.hpp>
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
 * the client sends only a config_id and the server resolves the config — whichever kind it is,
 * its children, and the refdata context — checks permissions, and starts the producer via the
 * factory. The kind is discovered by probing the per-kind config repositories, since the
 * market_data_generation_config container carries no kind discriminator. Missing, disabled, and
 * vintage-data-missing outcomes use one message shape regardless of kind.
 */
class feed_config_handler {
public:
    feed_config_handler(ores::nats::service::client& nats,
                        ores::nats::service::nats_client& auth_nats,
                        std::shared_ptr<feed_controller> ctrl,
                        ores::database::context ctx,
                        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , auth_nats_(auth_nats)
        , ctrl_(std::move(ctrl))
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

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

        // Kind resolution: probe the FX repository first, then the IR
        // repository, by the config's own id — the container has no kind
        // discriminator, so the probe is the resolution. FX wins on the
        // (invalid) row set that names one id in both families.
        namespace repo = ores::synthetic::repository;
        repo::fx_spot_generation_config_repository fx_repo;
        const auto fxs = fx_repo.read_latest(req_ctx, req->config_id);
        if (!fxs.empty()) {
            start_fx(req_ctx, msg, fxs.front());
            return;
        }
        repo::ir_curve_generation_config_repository ir_repo;
        const auto configs = ir_repo.read_latest(req_ctx, req->config_id);
        if (!configs.empty()) {
            start_ir(req_ctx, msg, configs.front());
            return;
        }
        reply(nats_,
              msg,
              start_feed_response{.success = false,
                                  .message = "Feed config not found: " + req->config_id});
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
            // Same probe as start(): resolve the config_id to its source_name
            // server-side so the client never needs the naming conventions.
            namespace repo = ores::synthetic::repository;
            repo::fx_spot_generation_config_repository fx_repo;
            const auto fxs = fx_repo.read_latest(req_ctx, req->config_id);
            if (!fxs.empty())
                source_name = fxs.front().source_name;
            else {
                repo::ir_curve_generation_config_repository ir_repo;
                const auto configs = ir_repo.read_latest(req_ctx, req->config_id);
                if (!configs.empty())
                    source_name = configs.front().source_name;
                else {
                    reply(nats_,
                          msg,
                          stop_feed_response{.success = false,
                                             .message = "Feed config not found: " +
                                                        req->config_id});
                    return;
                }
            }
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

        // Every kind is listed, so the gate is the same uniform pair the
        // folder cascade requires — the per-kind config families are both
        // visible to the caller.
        if (!has_permission(req_ctx, "synthetic::fx_spot_generation_configs:read") ||
            !has_permission(req_ctx, "synthetic::ir_curve_generation_configs:read")) {
            BOOST_LOG_SEV(feed_config_handler_lg(), warn)
                << "Rejecting list request: missing permission "
                   "synthetic::fx_spot_generation_configs:read or "
                   "synthetic::ir_curve_generation_configs:read.";
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        list_feeds_response resp;
        // The empty kind scopes to nothing: every running feed, both kinds.
        resp.running_source_names = ctrl_->list();
        resp.success = true;
        BOOST_LOG_SEV(feed_config_handler_lg(), info)
            << msg.subject << " — " << resp.running_source_names.size() << " feed(s) running";
        reply(nats_, msg, resp);
    }

private:
    // Auth gate shared by every verb; the per-kind permission check happens
    // after resolution, in start_fx/start_ir, since the caller's kind is not
    // known until the config_id is resolved.
    std::optional<ores::database::context>
    authenticated_context(std::string_view verb, const ores::nats::message& msg) {
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            BOOST_LOG_SEV(feed_config_handler_lg(), warn)
                << "Rejecting " << verb << " request: auth failed: "
                << static_cast<int>(ctx_expected.error());
            error_reply(nats_, msg, ctx_expected.error());
            return std::nullopt;
        }
        return std::optional<ores::database::context>(std::move(*ctx_expected));
    }

    void start_fx(const ores::database::context& req_ctx,
                  const ores::nats::message& msg,
                  const ores::synthetic::domain::fx_spot_generation_config& cfg) {
        using namespace ores::synthetic::messaging;
        start_feed_response resp;

        if (!has_permission(req_ctx, "synthetic::fx_spot_generation_configs:read")) {
            BOOST_LOG_SEV(feed_config_handler_lg(), warn)
                << "Rejecting start request: missing permission "
                   "synthetic::fx_spot_generation_configs:read.";
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        // Uniform startability across kinds: the row itself must be enabled
        // and its container must exist and be enabled — the same gate the
        // auto-start walk and the folder cascade apply. The container also
        // carries the feed's binding_mode.
        namespace repo = ores::synthetic::repository;
        repo::market_data_generation_config_repository feed_repo;
        const auto containers = feed_repo.read_latest(req_ctx, boost::uuids::to_string(cfg.config_id));
        if (!cfg.enabled || containers.empty() || !containers.front().enabled) {
            resp.message = "Feed config is not enabled: " + boost::uuids::to_string(cfg.id);
            reply(nats_, msg, resp);
            return;
        }

        repo::gmm_component_repository comp_repo;
        std::vector<ores::synthetic::domain::gmm_component> components;
        for (auto& c : comp_repo.read_latest(req_ctx))
            if (c.fx_spot_config_id == cfg.id)
                components.push_back(std::move(c));
        if (components.empty()) {
            resp.message = "Feed config has no GMM components: " + boost::uuids::to_string(cfg.id);
            reply(nats_, msg, resp);
            return;
        }

        const auto bearer = ores::nats::service::extract_bearer(msg);
        const ores::synthetic::feed::feed_build_context bctx{nats_, auth_nats_, bearer};
        const auto factory = ores::synthetic::feed::make_default_feed_factory();
        try {
            const auto feed = factory.make(
                std::string(ores::synthetic::feed::fx_spot_feed_kind),
                bctx,
                ores::synthetic::feed::fx_spot_feed_build_input{
                    cfg, std::move(components), containers.front().binding_mode});
            reply_start_outcome(msg, resp, std::move(feed));
        } catch (const std::exception& e) {
            resp.success = false;
            resp.message = std::string("Failed to start FX feed: ") + e.what();
            BOOST_LOG_SEV(feed_config_handler_lg(), error) << msg.subject << " — " << resp.message;
            reply(nats_, msg, resp);
        }
    }

    void start_ir(const ores::database::context& req_ctx,
                  const ores::nats::message& msg,
                  const ores::synthetic::domain::ir_curve_generation_config& cfg) {
        using namespace ores::synthetic::messaging;
        start_feed_response resp;

        if (!has_permission(req_ctx, "synthetic::ir_curve_generation_configs:read")) {
            BOOST_LOG_SEV(feed_config_handler_lg(), warn)
                << "Rejecting start request: missing permission "
                   "synthetic::ir_curve_generation_configs:read.";
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        namespace repo = ores::synthetic::repository;
        repo::market_data_generation_config_repository feed_repo;
        const auto containers = feed_repo.read_latest(req_ctx, boost::uuids::to_string(cfg.config_id));
        if (!cfg.enabled || containers.empty() || !containers.front().enabled) {
            resp.message = "Feed config is not enabled: " + boost::uuids::to_string(cfg.id);
            reply(nats_, msg, resp);
            return;
        }

        repo::ir_curve_template_entry_repository entry_repo;
        std::vector<ores::synthetic::domain::ir_curve_template_entry> entries;
        for (auto& e : entry_repo.read_latest(req_ctx))
            if (e.ir_curve_config_id == cfg.id)
                entries.push_back(std::move(e));
        if (entries.empty()) {
            resp.message =
                "Feed config has no Curve Template entries: " + boost::uuids::to_string(cfg.id);
            reply(nats_, msg, resp);
            return;
        }

        // Row-based parameters: the config's own value rows (filtered to cfg.id -- the
        // generated repository has no parent-scoped read) plus the system-tenant definitions
        // catalogue; make_ir_curve_feed joins and validates the two.
        repo::ir_curve_generation_config_process_parameter_value_repository value_repo;
        std::vector<ores::synthetic::domain::ir_curve_generation_config_process_parameter_value>
            values;
        for (auto& v : value_repo.read_latest(req_ctx))
            if (v.config_id == cfg.id)
                values.push_back(std::move(v));
        if (values.empty()) {
            resp.message =
                "Feed config has no parameter value rows: " + boost::uuids::to_string(cfg.id);
            reply(nats_, msg, resp);
            return;
        }

        // The definitions catalogue is system-tenant owned: the publish
        // path resolves each value's parameter_definition_id from the
        // system tenant, so a read scoped to the caller's tenant returns
        // nothing for any real tenant. Read with a system-tenant context.
        repo::yield_curve_process_parameter_definition_repository definition_repo;
        const auto sys_ctx = ores::database::service::tenant_context::with_system_tenant(req_ctx);
        const auto definitions = definition_repo.read_latest(sys_ctx);

        const auto convention_code =
            ores::synthetic::feed::ir_curve_tenor_convention_code(
                ores::synthetic::feed::ir_curve_qualifier(cfg));
        auto refctx = ores::synthetic::feed::build_ir_curve_refdata_context(req_ctx, convention_code);
        if (!refctx) {
            resp.message = "Tenor convention not found: " + convention_code;
            reply(nats_, msg, resp);
            return;
        }

        const auto bearer = ores::nats::service::extract_bearer(msg);
        const ores::synthetic::feed::feed_build_context bctx{nats_, auth_nats_, bearer};
        const auto factory = ores::synthetic::feed::make_default_feed_factory();
        try {
            const auto feed = factory.make(
                std::string(ores::synthetic::feed::ir_curve_feed_kind),
                bctx,
                ores::synthetic::feed::ir_curve_feed_build_input{
                    cfg, std::move(entries), std::move(values), definitions, *refctx});
            reply_start_outcome(msg, resp, std::move(feed));
        } catch (const ores::synthetic::feed::vintage_data_missing_error& e) {
            resp.success = false;
            resp.message = e.what();
            BOOST_LOG_SEV(feed_config_handler_lg(), warn)
                << msg.subject << " — feed rejected: " << resp.message;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            resp.success = false;
            resp.message = std::string("Failed to start IR curve feed: ") + e.what();
            BOOST_LOG_SEV(feed_config_handler_lg(), error) << msg.subject << " — " << resp.message;
            reply(nats_, msg, resp);
        }
    }

    // One start-result switch for every kind: the message shapes (started,
    // already running, conflict with the holding source_name) are uniform, so
    // the dispatch needs no per-kind branching.
    void reply_start_outcome(const ores::nats::message& msg,
                             ores::synthetic::messaging::start_feed_response& resp,
                             std::shared_ptr<ores::marketdata::domain::IFeed> feed) {
        const auto source_name = feed->source_name();
        const auto conflict_key = feed->conflict_key();
        const auto result = ctrl_->start(std::move(feed));

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
                // The IR builder resolves vintage at construction and throws
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
};

}
#endif
