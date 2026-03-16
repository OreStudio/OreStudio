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
#include "ores.reporting/messaging/registrar.hpp"

#include <optional>
#include <span>
#include <string_view>
#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp"
#include <boost/uuid/string_generator.hpp>
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.reporting/messaging/report_type_protocol.hpp"
#include "ores.reporting/messaging/report_definition_protocol.hpp"
#include "ores.reporting/messaging/report_instance_protocol.hpp"
#include "ores.reporting/messaging/concurrency_policy_protocol.hpp"
#include "ores.reporting/service/report_type_service.hpp"
#include "ores.reporting/service/report_definition_service.hpp"
#include "ores.reporting/service/report_instance_service.hpp"
#include "ores.reporting/service/concurrency_policy_service.hpp"
#include "ores.service/service/request_context.hpp"

namespace ores::reporting::messaging {

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
        "reporting.v1.>", "ores.reporting.service",
        [&nats, base_ctx = ctx, verifier](ores::nats::message msg) mutable {
            const auto ctx = ores::service::service::make_request_context(base_ctx, msg, verifier);
            const auto& subj = msg.subject;

            // ----------------------------------------------------------------
            // Report types
            // ----------------------------------------------------------------
            if (subj.ends_with(".report-types.list")) {
                service::report_type_service svc(ctx);
                get_report_types_response resp;
                try {
                    resp.types = svc.list_types();
                    resp.total_available_count =
                        static_cast<int>(resp.types.size());
                } catch (...) {}
                reply(nats, msg, resp);

            } else if (subj.ends_with(".report-types.save")) {
                service::report_type_service svc(ctx);
                if (auto req = decode<save_report_type_request>(msg)) {
                    try {
                        svc.save_type(req->type);
                        reply(nats, msg,
                            save_report_type_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, save_report_type_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".report-types.delete")) {
                service::report_type_service svc(ctx);
                if (auto req = decode<delete_report_type_request>(msg)) {
                    try {
                        for (const auto& code : req->codes)
                            svc.remove_type(code);
                        reply(nats, msg,
                            delete_report_type_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, delete_report_type_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".report-types.history")) {
                service::report_type_service svc(ctx);
                if (auto req = decode<get_report_type_history_request>(msg)) {
                    try {
                        auto history = svc.get_type_history(req->code);
                        reply(nats, msg, get_report_type_history_response{
                            .success = true,
                            .history = std::move(history)});
                    } catch (const std::exception& e) {
                        reply(nats, msg, get_report_type_history_response{
                            .success = false, .message = e.what()});
                    }
                }

            // ----------------------------------------------------------------
            // Report definitions
            // ----------------------------------------------------------------
            } else if (subj.ends_with(".report-definitions.list")) {
                service::report_definition_service svc(ctx);
                get_report_definitions_response resp;
                try {
                    resp.definitions = svc.list_definitions();
                    resp.total_available_count =
                        static_cast<int>(resp.definitions.size());
                } catch (...) {}
                reply(nats, msg, resp);

            } else if (subj.ends_with(".report-definitions.save")) {
                service::report_definition_service svc(ctx);
                if (auto req = decode<save_report_definition_request>(msg)) {
                    try {
                        svc.save_definition(req->definition);
                        reply(nats, msg,
                            save_report_definition_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, save_report_definition_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".report-definitions.delete")) {
                service::report_definition_service svc(ctx);
                if (auto req = decode<delete_report_definition_request>(msg)) {
                    try {
                        for (const auto& id : req->ids)
                            svc.remove_definition(id);
                        reply(nats, msg,
                            delete_report_definition_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, delete_report_definition_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".report-definitions.history")) {
                service::report_definition_service svc(ctx);
                if (auto req =
                        decode<get_report_definition_history_request>(msg)) {
                    try {
                        auto history = svc.get_definition_history(req->id);
                        reply(nats, msg, get_report_definition_history_response{
                            .success = true,
                            .history = std::move(history)});
                    } catch (const std::exception& e) {
                        reply(nats, msg, get_report_definition_history_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".report-definitions.schedule")) {
                if (auto req =
                        decode<schedule_report_definitions_request>(msg)) {
                    reply(nats, msg, schedule_report_definitions_response{
                        .success = false,
                        .message = "schedule not yet implemented"});
                }

            } else if (subj.ends_with(".report-definitions.unschedule")) {
                if (auto req =
                        decode<unschedule_report_definitions_request>(msg)) {
                    reply(nats, msg, unschedule_report_definitions_response{
                        .success = false,
                        .message = "unschedule not yet implemented"});
                }

            // ----------------------------------------------------------------
            // Report instances
            // ----------------------------------------------------------------
            } else if (subj.ends_with(".report-instances.list")) {
                service::report_instance_service svc(ctx);
                get_report_instances_response resp;
                try {
                    resp.instances = svc.list_instances();
                    resp.total_available_count =
                        static_cast<int>(resp.instances.size());
                } catch (...) {}
                reply(nats, msg, resp);

            } else if (subj.ends_with(".report-instances.save")) {
                service::report_instance_service svc(ctx);
                if (auto req = decode<save_report_instance_request>(msg)) {
                    try {
                        svc.save_instance(req->instance);
                        reply(nats, msg,
                            save_report_instance_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, save_report_instance_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".report-instances.delete")) {
                service::report_instance_service svc(ctx);
                if (auto req = decode<delete_report_instance_request>(msg)) {
                    try {
                        for (const auto& id : req->ids)
                            svc.remove_instance(id);
                        reply(nats, msg,
                            delete_report_instance_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, delete_report_instance_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".report-instances.history")) {
                service::report_instance_service svc(ctx);
                if (auto req =
                        decode<get_report_instance_history_request>(msg)) {
                    try {
                        auto history = svc.get_instance_history(req->id);
                        reply(nats, msg, get_report_instance_history_response{
                            .success = true,
                            .history = std::move(history)});
                    } catch (const std::exception& e) {
                        reply(nats, msg, get_report_instance_history_response{
                            .success = false, .message = e.what()});
                    }
                }

            // ----------------------------------------------------------------
            // Concurrency policies
            // ----------------------------------------------------------------
            } else if (subj.ends_with(".concurrency-policies.list")) {
                service::concurrency_policy_service svc(ctx);
                get_concurrency_policies_response resp;
                try {
                    resp.policies = svc.list_policies();
                    resp.total_available_count =
                        static_cast<int>(resp.policies.size());
                } catch (...) {}
                reply(nats, msg, resp);

            } else if (subj.ends_with(".concurrency-policies.save")) {
                service::concurrency_policy_service svc(ctx);
                if (auto req = decode<save_concurrency_policy_request>(msg)) {
                    try {
                        svc.save_policy(req->policy);
                        reply(nats, msg,
                            save_concurrency_policy_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, save_concurrency_policy_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".concurrency-policies.delete")) {
                service::concurrency_policy_service svc(ctx);
                if (auto req =
                        decode<delete_concurrency_policy_request>(msg)) {
                    try {
                        for (const auto& code : req->codes)
                            svc.remove_policy(code);
                        reply(nats, msg,
                            delete_concurrency_policy_response{
                                .success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, delete_concurrency_policy_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".concurrency-policies.history")) {
                service::concurrency_policy_service svc(ctx);
                if (auto req =
                        decode<get_concurrency_policy_history_request>(msg)) {
                    try {
                        auto history = svc.get_policy_history(req->code);
                        reply(nats, msg,
                            get_concurrency_policy_history_response{
                                .success = true,
                                .history = std::move(history)});
                    } catch (const std::exception& e) {
                        reply(nats, msg,
                            get_concurrency_policy_history_response{
                                .success = false, .message = e.what()});
                    }
                }
            }
        }));

    return subs;
}

}
