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
#ifndef ORES_SCHEDULER_MESSAGING_JOB_INSTANCE_HANDLER_HPP
#define ORES_SCHEDULER_MESSAGING_JOB_INSTANCE_HANDLER_HPP

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
inline auto& job_instance_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.scheduler.messaging.job_instance_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using namespace ores::logging;

class job_instance_handler {
public:
    job_instance_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(job_instance_handler_lg(), debug)
            << "Handling " << msg.subject;

        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;

        get_job_instances_response resp;
        try {
            if (auto req = decode<get_job_instances_request>(msg)) {
                // Load all job definitions to build a name look-up map.
                repository::job_definition_repository def_repo;
                const auto defs = def_repo.read_latest(ctx);
                std::unordered_map<std::string, std::string> id_to_name;
                for (const auto& d : defs)
                    id_to_name[boost::uuids::to_string(d.id)] = d.job_name;

                // Load recent instances across all jobs.
                repository::job_instance_repository inst_repo;
                const auto limit = static_cast<std::size_t>(
                    req->limit > 0 ? req->limit : 100);
                const auto instances = inst_repo.read_all_latest(ctx, limit);

                resp.instances.reserve(instances.size());
                for (const auto& inst : instances) {
                    job_instance_summary s;
                    s.id = inst.id;
                    s.job_definition_id = boost::uuids::to_string(inst.job_definition_id);
                    const auto it = id_to_name.find(s.job_definition_id);
                    s.job_name = (it != id_to_name.end()) ? it->second : s.job_definition_id;
                    s.action_type = inst.action_type;
                    s.status = [&]() -> std::string {
                        switch (inst.status) {
                        case domain::job_status::succeeded: return "succeeded";
                        case domain::job_status::failed:    return "failed";
                        default:                            return "starting";
                        }
                    }();
                    s.triggered_at =
                        ores::platform::time::datetime::to_iso8601_utc(inst.triggered_at);
                    s.started_at =
                        ores::platform::time::datetime::to_iso8601_utc(inst.started_at);
                    if (inst.completed_at)
                        s.completed_at =
                            ores::platform::time::datetime::to_iso8601_utc(*inst.completed_at);
                    s.duration_ms = inst.duration_ms;
                    s.error_message = inst.error_message;
                    resp.instances.push_back(std::move(s));
                }
                resp.total_available_count = static_cast<int>(resp.instances.size());
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(job_instance_handler_lg(), error)
                << "Error listing job instances: " << e.what();
        }

        reply(nats_, msg, resp);
        BOOST_LOG_SEV(job_instance_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::scheduler::messaging

#endif
