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
#ifndef ORES_SCHEDULER_MESSAGING_JOB_DEFINITION_HANDLER_HPP
#define ORES_SCHEDULER_MESSAGING_JOB_DEFINITION_HANDLER_HPP

#include <optional>
#include <stdexcept>
#include <rfl/json.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.scheduler.api/domain/cron_expression.hpp"
#include "ores.scheduler.api/messaging/scheduler_protocol.hpp"
#include "ores.scheduler.core/service/job_definition_service.hpp"

namespace rfl {
template<>
struct Reflector<ores::scheduler::domain::cron_expression> {
    using ReflType = std::string;
    static ores::scheduler::domain::cron_expression to(const ReflType& str) {
        if (str.empty()) return {};
        auto r = ores::scheduler::domain::cron_expression::from_string(str);
        if (!r) throw std::runtime_error("Invalid cron expression: " + r.error());
        return *r;
    }
    static ReflType from(const ores::scheduler::domain::cron_expression& v) {
        return v.to_string();
    }
};
} // namespace rfl

namespace ores::scheduler::messaging {

namespace {
inline auto& job_definition_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.scheduler.messaging.job_definition_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::delegated_actor;
using ores::service::service::make_context_from_jwt;
using namespace ores::logging;

class job_definition_handler {
public:
    job_definition_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(job_definition_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::job_definition_service svc(ctx);
        get_job_definitions_response resp;
        try {
            if (auto req = decode<get_job_definitions_request>(msg)) {
                resp.definitions = svc.list_definitions();
                resp.total_available_count =
                    static_cast<int>(resp.definitions.size());
            }
        } catch (...) {}
        reply(nats_, msg, resp);
        BOOST_LOG_SEV(job_definition_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    void schedule(ores::nats::message msg) {
        BOOST_LOG_SEV(job_definition_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "scheduler::job_definitions:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        if (auto req = decode<schedule_job_request>(msg)) {
            try {
                auto op_ctx = resolve_context(req->on_behalf_of,
                    req->definition.tenant_id);
                if (!op_ctx) {
                    reply(nats_, msg, schedule_job_response{
                        .success = false, .message = "Delegated token expired or invalid"});
                    return;
                }
                req->definition.modified_by = delegated_actor(*op_ctx);
                req->definition.performed_by = op_ctx->service_account();
                service::job_definition_service svc(*op_ctx);
                svc.save_definition(req->definition);
                reply(nats_, msg, schedule_job_response{.success = true});
            } catch (const std::exception& e) {
                reply(nats_, msg, schedule_job_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(job_definition_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            reply(nats_, msg, schedule_job_response{
                .success = false, .message = "Failed to decode request"});
        }
        BOOST_LOG_SEV(job_definition_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    void schedule_batch(ores::nats::message msg) {
        BOOST_LOG_SEV(job_definition_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "scheduler::job_definitions:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        if (auto req = decode<schedule_jobs_batch_request>(msg)) {
            schedule_jobs_batch_response resp;
            resp.success = true;
            for (auto& def : req->definitions) {
                try {
                    auto op_ctx = resolve_context(req->on_behalf_of,
                        def.tenant_id);
                    if (!op_ctx) {
                        BOOST_LOG_SEV(job_definition_handler_lg(), error)
                            << "Delegated token expired for job " << def.job_name;
                        resp.failed_ids.push_back(boost::uuids::to_string(def.id));
                        continue;
                    }
                    def.modified_by = delegated_actor(*op_ctx);
                    def.performed_by = op_ctx->service_account();
                    service::job_definition_service svc(*op_ctx);
                    svc.save_definition(def);
                    ++resp.scheduled_count;
                } catch (const std::exception& e) {
                    BOOST_LOG_SEV(job_definition_handler_lg(), error)
                        << "Failed to schedule job " << def.job_name
                        << ": " << e.what();
                    resp.failed_ids.push_back(
                        boost::uuids::to_string(def.id));
                }
            }
            reply(nats_, msg, resp);
        } else {
            BOOST_LOG_SEV(job_definition_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            schedule_jobs_batch_response resp;
            resp.success = false;
            resp.message = "Failed to decode request";
            reply(nats_, msg, resp);
        }
        BOOST_LOG_SEV(job_definition_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    void unschedule(ores::nats::message msg) {
        BOOST_LOG_SEV(job_definition_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "scheduler::job_definitions:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        if (auto req = decode<unschedule_job_request>(msg)) {
            try {
                auto op_ctx = resolve_context(req->on_behalf_of, std::nullopt);
                if (!op_ctx) {
                    reply(nats_, msg, unschedule_job_response{
                        .success = false, .message = "Delegated token expired or invalid"});
                    return;
                }
                service::job_definition_service svc(*op_ctx);
                svc.remove_definition(req->job_definition_id);
                reply(nats_, msg, unschedule_job_response{.success = true});
            } catch (const std::exception& e) {
                reply(nats_, msg, unschedule_job_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(job_definition_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            reply(nats_, msg, unschedule_job_response{
                .success = false, .message = "Failed to decode request"});
        }
        BOOST_LOG_SEV(job_definition_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    void history(ores::nats::message msg) {
        BOOST_LOG_SEV(job_definition_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (auto req = decode<get_job_history_request>(msg)) {
            try {
                service::job_definition_service svc(ctx);
                // Returns definition version history; instance
                // execution history is not yet implemented.
                svc.get_definition_history(req->job_definition_id);
                reply(nats_, msg, get_job_history_response{.success = true});
            } catch (const std::exception& e) {
                reply(nats_, msg, get_job_history_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(job_definition_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
        BOOST_LOG_SEV(job_definition_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

private:
    /**
     * @brief Derive the DB context for a scheduler write operation.
     *
     * If @p on_behalf_of (a forwarded user JWT) is present, validates it and
     * returns a fully-scoped user context (tenant_id, actor, party_ids).
     * An expired token returns nullopt — the caller must reject the request.
     *
     * If @p on_behalf_of is empty (system-initiated calls such as
     * reconciliation), falls back to a service-account context scoped to
     * @p tenant_id from the job definition payload.
     */
    std::optional<ores::database::context> resolve_context(
        const std::string& on_behalf_of,
        const std::optional<boost::uuids::uuid>& tenant_id) const {

        if (!on_behalf_of.empty() && verifier_) {
            auto result = make_context_from_jwt(ctx_, on_behalf_of, *verifier_);
            if (!result) return std::nullopt;
            return *result;
        }

        // System path: scope to the job definition's tenant_id.
        if (tenant_id) {
            auto tid = ores::utility::uuid::tenant_id::from_uuid(*tenant_id);
            if (tid) return ctx_.with_tenant(*tid, ctx_.service_account());
        }
        return ctx_;
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::scheduler::messaging

#endif
