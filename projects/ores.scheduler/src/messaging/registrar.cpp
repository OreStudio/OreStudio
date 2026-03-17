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
#include "ores.scheduler/messaging/registrar.hpp"

#include <optional>
#include <span>
#include <stdexcept>
#include <string_view>
#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp"
#include <boost/uuid/string_generator.hpp>
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.scheduler/domain/cron_expression.hpp"
#include "ores.scheduler/messaging/scheduler_protocol.hpp"
#include "ores.scheduler/service/job_definition_service.hpp"
#include "ores.service/service/request_context.hpp"

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
}

namespace ores::scheduler::messaging {

namespace {

template<typename Resp>
void reply(ores::nats::service::client& nats,
           const ores::nats::message& msg,
           const Resp& resp) {
    if (msg.reply_subject.empty())
        return;
    const auto json = rfl::json::write(resp);
    const auto* p = reinterpret_cast<const std::byte*>(json.data());
    nats.publish(msg.reply_subject, std::span<const std::byte>(p, json.size()));
}

template<typename Req>
std::optional<Req> decode(const ores::nats::message& msg) {
    const std::string_view sv(
        reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto r = rfl::json::read<Req>(sv);
    if (!r)
        return std::nullopt;
    return *r;
}

} // namespace


std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;

    subs.push_back(nats.queue_subscribe(
        "scheduler.v1.>", "ores.scheduler.service",
        [&nats, base_ctx = ctx, verifier](ores::nats::message msg) mutable {
            const auto ctx = ores::service::service::make_request_context(base_ctx, msg, verifier);
            const auto& subj = msg.subject;

            // ----------------------------------------------------------------
            // Job definitions — list
            // ----------------------------------------------------------------
            if (subj.ends_with(".job-definitions.list")) {
                service::job_definition_service svc(ctx);
                get_job_definitions_response resp;
                try {
                    if (auto req = decode<get_job_definitions_request>(msg)) {
                        resp.definitions = svc.list_definitions();
                        resp.total_available_count =
                            static_cast<int>(resp.definitions.size());
                    }
                } catch (...) {}
                reply(nats, msg, resp);

            // ----------------------------------------------------------------
            // Job definitions — schedule
            // ----------------------------------------------------------------
            } else if (subj.ends_with(".job-definitions.schedule")) {
                if (auto req = decode<schedule_job_request>(msg)) {
                    try {
                        service::job_definition_service svc(ctx);
                        svc.save_definition(req->definition);
                        reply(nats, msg, schedule_job_response{
                            .success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, schedule_job_response{
                            .success = false, .message = e.what()});
                    }
                }

            // ----------------------------------------------------------------
            // Job definitions — unschedule
            // ----------------------------------------------------------------
            } else if (subj.ends_with(".job-definitions.unschedule")) {
                if (auto req = decode<unschedule_job_request>(msg)) {
                    try {
                        service::job_definition_service svc(ctx);
                        svc.remove_definition(req->job_definition_id);
                        reply(nats, msg, unschedule_job_response{
                            .success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, unschedule_job_response{
                            .success = false, .message = e.what()});
                    }
                }

            // ----------------------------------------------------------------
            // Job definitions — history
            // ----------------------------------------------------------------
            } else if (subj.ends_with(".job-definitions.history")) {
                if (auto req = decode<get_job_history_request>(msg)) {
                    try {
                        service::job_definition_service svc(ctx);
                        // Returns definition version history; instance
                        // execution history is not yet implemented.
                        svc.get_definition_history(req->job_definition_id);
                        reply(nats, msg, get_job_history_response{
                            .success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, get_job_history_response{
                            .success = false, .message = e.what()});
                    }
                }
            }
        }));

    return subs;
}

} // namespace ores::scheduler::messaging
