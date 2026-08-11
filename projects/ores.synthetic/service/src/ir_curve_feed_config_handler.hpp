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
#ifndef ORES_SYNTHETIC_SERVICE_IR_CURVE_FEED_CONFIG_HANDLER_HPP
#define ORES_SYNTHETIC_SERVICE_IR_CURVE_FEED_CONFIG_HANDLER_HPP

#include "feed_controller.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.synthetic.api/feeds/ir_curve_feed.hpp"
#include "ores.synthetic.api/messaging/ir_curve_feed_config_protocol.hpp"
#include "ores.synthetic.core/repository/ir_curve_generation_config_process_parameter_value_repository.hpp"
#include "ores.synthetic.core/repository/ir_curve_generation_config_repository.hpp"
#include "ores.synthetic.core/repository/ir_curve_template_entry_repository.hpp"
#include "ores.synthetic.core/repository/yield_curve_process_parameter_definition_repository.hpp"
#include <memory>
#include <optional>

namespace ores::synthetic::service {

namespace {
inline auto& ir_curve_feed_config_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.synthetic.service.ir_curve_feed_config_handler");
    return instance;
}
} // namespace

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::log_handler_entry;
using ores::service::messaging::reply;
using namespace ores::logging;
using ores::synthetic::feed::build_ir_curve_refdata_context;
using ores::synthetic::feed::ir_curve_qualifier;
using ores::synthetic::feed::ir_curve_tenor_convention_code;
using ores::synthetic::feed::make_ir_curve_feed;
using ores::synthetic::feed::vintage_data_missing_error;

/**
 * @brief NATS handler for IR curve feed start/stop/list control messages -- the on-demand
 * counterpart to the boot-time auto_start_feeds() walk, mirroring market_feed_config_handler for
 * FX. Unlike auto-start (which only ever reads the service's own unscoped, system-tenant
 * context), start() derives a request-scoped context from the caller's own JWT via
 * make_request_context() -- the same pattern every other synthetic entity handler uses -- so a
 * config published to the caller's own tenant/party is reachable, not just system's.
 */
class ir_curve_feed_config_handler {
public:
    ir_curve_feed_config_handler(ores::nats::service::client& nats,
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
        [[maybe_unused]] const auto cid = log_handler_entry(ir_curve_feed_config_handler_lg(), msg);

        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;

        auto req = decode<start_ir_curve_feed_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(ir_curve_feed_config_handler_lg(), warn)
                << msg.subject << " — empty or malformed start body; rejecting";
            reply(nats_,
                  msg,
                  start_ir_curve_feed_response{.success = false,
                                               .message = "Malformed start request"});
            return;
        }

        start_ir_curve_feed_response resp;
        try {
            repository::ir_curve_generation_config_repository config_repo;
            auto configs = config_repo.read_latest(req_ctx, req->config_id);
            if (configs.empty()) {
                resp.message = "IR curve config not found: " + req->config_id;
                reply(nats_, msg, resp);
                return;
            }
            const auto& cfg = configs.front();
            if (!cfg.enabled) {
                resp.message = "IR curve config is not enabled: " + req->config_id;
                reply(nats_, msg, resp);
                return;
            }

            repository::ir_curve_template_entry_repository entry_repo;
            std::vector<ores::synthetic::domain::ir_curve_template_entry> entries;
            for (auto& e : entry_repo.read_latest(req_ctx))
                if (e.ir_curve_config_id == cfg.id)
                    entries.push_back(std::move(e));

            if (entries.empty()) {
                resp.message = "IR curve config has no Curve Template entries: " + req->config_id;
                reply(nats_, msg, resp);
                return;
            }

            // Row-based parameters: the config's own value rows (filtered to cfg.id -- the
            // generated repository has no parent-scoped read) plus the system-tenant definitions
            // catalogue; make_ir_curve_feed joins and validates the two.
            repository::ir_curve_generation_config_process_parameter_value_repository value_repo;
            std::vector<ores::synthetic::domain::ir_curve_generation_config_process_parameter_value>
                values;
            for (auto& v : value_repo.read_latest(req_ctx))
                if (v.config_id == cfg.id)
                    values.push_back(std::move(v));

            if (values.empty()) {
                resp.message = "IR curve config has no parameter value rows: " + req->config_id;
                reply(nats_, msg, resp);
                return;
            }

            // The definitions catalogue is system-tenant owned: the publish
            // path resolves each value's parameter_definition_id from the
            // system tenant, so a read scoped to the caller's tenant (the
            // acme tenant here) returns nothing and every value row fails to
            // join. Read with a system-tenant context instead.
            repository::yield_curve_process_parameter_definition_repository definition_repo;
            const auto sys_ctx =
                ores::database::service::tenant_context::with_system_tenant(req_ctx);
            const auto definitions = definition_repo.read_latest(sys_ctx);

            const auto convention_code = ir_curve_tenor_convention_code(ir_curve_qualifier(cfg));
            auto refctx = build_ir_curve_refdata_context(req_ctx, convention_code);
            if (!refctx) {
                resp.message = "Tenor convention not found: " + convention_code;
                reply(nats_, msg, resp);
                return;
            }

            const auto bearer = ores::nats::service::extract_bearer(msg);
            auto feed = make_ir_curve_feed(
                nats_, auth_nats_, cfg, entries, values, definitions, *refctx, bearer);
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
                    const auto conflicting =
                        ctrl_->running_source_name_for_conflict_key(conflict_key);
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
            BOOST_LOG_SEV(ir_curve_feed_config_handler_lg(), info)
                << msg.subject << " — " << resp.message;
        } catch (const vintage_data_missing_error& e) {
            resp.success = false;
            resp.message = e.what();
            BOOST_LOG_SEV(ir_curve_feed_config_handler_lg(), warn)
                << msg.subject << " — feed rejected: " << resp.message;
        } catch (const std::exception& e) {
            resp.success = false;
            resp.message = std::string("Failed to start IR curve feed: ") + e.what();
            BOOST_LOG_SEV(ir_curve_feed_config_handler_lg(), error)
                << msg.subject << " — " << resp.message;
        }
        reply(nats_, msg, resp);
    }

    void stop(ores::nats::message msg) {
        using namespace ores::synthetic::messaging;
        [[maybe_unused]] const auto cid = log_handler_entry(ir_curve_feed_config_handler_lg(), msg);

        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;

        auto req = decode<stop_ir_curve_feed_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(ir_curve_feed_config_handler_lg(), warn)
                << msg.subject << " — empty or malformed stop body; rejecting";
            reply(
                nats_,
                msg,
                stop_ir_curve_feed_response{.success = false, .message = "Malformed stop request"});
            return;
        }

        std::string source_name = req->source_name;
        if (!req->config_id.empty()) {
            repository::ir_curve_generation_config_repository config_repo;
            auto configs = config_repo.read_latest(req_ctx, req->config_id);
            if (configs.empty()) {
                reply(nats_,
                      msg,
                      stop_ir_curve_feed_response{.success = false,
                                                  .message = "IR curve config not found: " +
                                                             req->config_id});
                return;
            }
            source_name = configs.front().source_name;
        }

        const auto stopped = ctrl_->stop(source_name);
        stop_ir_curve_feed_response resp;
        resp.success = true; // idempotent — 0 stopped means it was already stopped
        resp.message = std::to_string(stopped) + " feed(s) stopped";
        BOOST_LOG_SEV(ir_curve_feed_config_handler_lg(), info)
            << msg.subject << " — " << resp.message
            << (source_name.empty() ? " (all)" : " (" + source_name + ")");
        reply(nats_, msg, resp);
    }

    void list(ores::nats::message msg) {
        using namespace ores::synthetic::messaging;
        [[maybe_unused]] const auto cid = log_handler_entry(ir_curve_feed_config_handler_lg(), msg);
        list_ir_curve_feed_configs_response resp;
        // The collapsed controller owns every feed kind; this handler lists
        // only its own (IR curves), as the per-kind controller it replaced did.
        resp.running_source_names =
            ctrl_->list(std::string(ores::synthetic::feed::ir_curve_feed_kind));
        resp.success = true;
        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    ores::nats::service::nats_client& auth_nats_;
    std::shared_ptr<feed_controller> ctrl_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
