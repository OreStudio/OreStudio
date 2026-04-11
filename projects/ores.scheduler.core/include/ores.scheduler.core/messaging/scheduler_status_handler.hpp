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
#ifndef ORES_SCHEDULER_MESSAGING_SCHEDULER_STATUS_HANDLER_HPP
#define ORES_SCHEDULER_MESSAGING_SCHEDULER_STATUS_HANDLER_HPP

#include <optional>
#include <rfl/json.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.scheduler.api/messaging/scheduler_protocol.hpp"
#include "ores.scheduler.core/repository/job_definition_repository.hpp"
#include "ores.scheduler.core/repository/job_instance_repository.hpp"

namespace ores::scheduler::messaging {

namespace {
inline auto& scheduler_status_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.scheduler.messaging.scheduler_status_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using namespace ores::logging;

class scheduler_status_handler {
public:
    scheduler_status_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void status(ores::nats::message msg) {
        BOOST_LOG_SEV(scheduler_status_handler_lg(), debug)
            << "Handling " << msg.subject;

        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;

        if (!decode<get_scheduler_status_request>(msg)) {
            BOOST_LOG_SEV(scheduler_status_handler_lg(), warn)
                << "Failed to decode get_scheduler_status_request";
            reply(nats_, msg, get_scheduler_status_response{
                .success = false, .message = "Failed to decode request"});
            return;
        }

        get_scheduler_status_response resp;
        try {
            repository::job_definition_repository def_repo;
            const auto defs = def_repo.read_latest(ctx);

            repository::job_instance_repository inst_repo;

            for (const auto& def : defs) {
                job_schedule_status jss;
                jss.job_definition_id = boost::uuids::to_string(def.id);
                jss.job_name = def.job_name;
                jss.description = def.description;
                jss.schedule_expression = def.schedule_expression.to_string();
                jss.is_active = def.is_active;

                // Last run info: most recent instance (limit=1).
                const auto recent = inst_repo.read_latest(ctx, def.id, 1);
                if (!recent.empty()) {
                    const auto& last = recent.front();
                    jss.last_run_at =
                        ores::platform::time::datetime::to_iso8601_utc(last.triggered_at);
                    switch (last.status) {
                    case domain::job_status::succeeded:
                        jss.last_run_status = "succeeded"; break;
                    case domain::job_status::failed:
                        jss.last_run_status = "failed"; break;
                    default:
                        jss.last_run_status = "starting";
                        jss.running_count = 1;
                        ++resp.total_running;
                        break;
                    }
                }

                // Next fire time from cron expression (only if active).
                if (def.is_active) {
                    try {
                        const auto next = def.schedule_expression.next_occurrence();
                        jss.next_fire_at =
                            ores::platform::time::datetime::to_iso8601_utc(next);
                    } catch (...) {}
                    ++resp.total_active;
                }

                resp.jobs.push_back(std::move(jss));
            }
            resp.success = true;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(scheduler_status_handler_lg(), error)
                << "Error computing scheduler status: " << e.what();
            reply(nats_, msg, get_scheduler_status_response{
                .success = false, .message = e.what()});
            return;
        }

        reply(nats_, msg, resp);
        BOOST_LOG_SEV(scheduler_status_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::scheduler::messaging

#endif
