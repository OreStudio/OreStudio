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
        if (auto req = decode<schedule_job_request>(msg)) {
            try {
                service::job_definition_service svc(ctx);
                stamp(req->definition, ctx);
                svc.save_definition(req->definition);
                reply(nats_, msg, schedule_job_response{.success = true});
            } catch (const std::exception& e) {
                reply(nats_, msg, schedule_job_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(job_definition_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
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
        if (auto req = decode<schedule_jobs_batch_request>(msg)) {
            service::job_definition_service svc(ctx);
            schedule_jobs_batch_response resp;
            resp.success = true;
            for (auto& def : req->definitions) {
                try {
                    stamp(def, ctx);
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
        if (auto req = decode<unschedule_job_request>(msg)) {
            try {
                service::job_definition_service svc(ctx);
                svc.remove_definition(req->job_definition_id);
                reply(nats_, msg, unschedule_job_response{.success = true});
            } catch (const std::exception& e) {
                reply(nats_, msg, unschedule_job_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(job_definition_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
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
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::scheduler::messaging

#endif
