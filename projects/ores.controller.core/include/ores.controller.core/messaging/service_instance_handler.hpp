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
#ifndef ORES_CONTROLLER_CORE_MESSAGING_SERVICE_INSTANCE_HANDLER_HPP
#define ORES_CONTROLLER_CORE_MESSAGING_SERVICE_INSTANCE_HANDLER_HPP

#include <optional>
#include <chrono>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.controller.api/messaging/service_instance_protocol.hpp"
#include "ores.controller.core/repository/service_instance_repository.hpp"
#include "ores.controller.core/repository/service_event_repository.hpp"
#include "ores.controller.core/service/process_supervisor.hpp"

namespace ores::controller::messaging {

namespace {
inline auto& service_instance_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.controller.messaging.service_instance_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::log_handler_entry;
using namespace ores::logging;

class service_instance_handler {
public:
    service_instance_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier,
        service::process_supervisor* supervisor = nullptr)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier))
        , supervisor_(supervisor) {}

    void list(ores::nats::message msg) {
        [[maybe_unused]] const auto cid =
            log_handler_entry(service_instance_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        auto req = decode<api::messaging::list_service_instances_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(service_instance_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        repository::service_instance_repository repo;
        api::messaging::list_service_instances_response resp;
        try {
            resp.service_instances = repo.read_all(ctx_, req->service_name);
            resp.success = true;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(service_instance_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.message = e.what();
        }
        reply(nats_, msg, resp);
    }

    void start(ores::nats::message msg) {
        [[maybe_unused]] const auto cid =
            log_handler_entry(service_instance_handler_lg(), msg);
        if (supervisor_) {
            dispatch_to_supervisor<api::messaging::start_service_request,
                api::messaging::start_service_response>(
                std::move(msg),
                [this](const std::string& svc, int r) {
                    supervisor_->request_launch(svc, r);
                });
        } else {
            handle_lifecycle<api::messaging::start_service_request,
                api::messaging::start_service_response>(
                std::move(msg), "running", "started");
        }
    }

    void stop(ores::nats::message msg) {
        [[maybe_unused]] const auto cid =
            log_handler_entry(service_instance_handler_lg(), msg);
        if (supervisor_) {
            dispatch_to_supervisor<api::messaging::stop_service_request,
                api::messaging::stop_service_response>(
                std::move(msg),
                [this](const std::string& svc, int r) {
                    supervisor_->request_stop(svc, r);
                });
        } else {
            handle_lifecycle<api::messaging::stop_service_request,
                api::messaging::stop_service_response>(
                std::move(msg), "stopped", "stopped");
        }
    }

    void restart(ores::nats::message msg) {
        [[maybe_unused]] const auto cid =
            log_handler_entry(service_instance_handler_lg(), msg);
        if (supervisor_) {
            dispatch_to_supervisor<api::messaging::restart_service_request,
                api::messaging::restart_service_response>(
                std::move(msg),
                [this](const std::string& svc, int r) {
                    supervisor_->request_restart(svc, r);
                });
        } else {
            handle_lifecycle<api::messaging::restart_service_request,
                api::messaging::restart_service_response>(
                std::move(msg), "running", "restarted");
        }
    }

private:
    template<typename Req, typename Resp>
    void handle_lifecycle(ores::nats::message msg,
        const std::string& new_phase, const std::string& event_type) {
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "controller::instances:manage")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<Req>(msg);
        if (!req) {
            BOOST_LOG_SEV(service_instance_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        repository::service_instance_repository inst_repo;
        repository::service_event_repository event_repo;
        Resp resp;
        try {
            apply_phase(inst_repo, event_repo, req->service_name,
                req->replica_index, new_phase, event_type);
            resp.success = true;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(service_instance_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.message = e.what();
        }
        reply(nats_, msg, resp);
    }

    void apply_phase(repository::service_instance_repository& inst_repo,
        repository::service_event_repository& event_repo,
        const std::string& service_name,
        std::optional<int> replica_index_filter,
        const std::string& new_phase,
        const std::string& event_type) {

        if (replica_index_filter) {
            transition_instance(inst_repo, event_repo, service_name,
                *replica_index_filter, new_phase, event_type);
        } else {
            auto instances = inst_repo.read_all(ctx_, service_name);
            if (instances.empty()) {
                // No instances yet — create replica 0.
                transition_instance(inst_repo, event_repo, service_name,
                    0, new_phase, event_type);
            } else {
                for (auto& inst : instances)
                    transition_instance(inst_repo, event_repo, service_name,
                        inst.replica_index, new_phase, event_type);
            }
        }
    }

    void transition_instance(repository::service_instance_repository& inst_repo,
        repository::service_event_repository& event_repo,
        const std::string& service_name, int replica_index,
        const std::string& new_phase, const std::string& event_type) {

        const auto now = std::chrono::system_clock::now();
        auto existing = inst_repo.read(ctx_, service_name, replica_index);
        if (existing) {
            existing->phase = new_phase;
            if (new_phase == "running") existing->started_at = now;
            else if (new_phase == "stopped" || new_phase == "failed")
                existing->stopped_at = now;
            inst_repo.update_phase(ctx_, *existing);
        } else {
            api::domain::service_instance inst;
            inst.id = boost::uuids::random_generator()();
            inst.service_name = service_name;
            inst.replica_index = replica_index;
            inst.phase = new_phase;
            inst.created_at = now;
            if (new_phase == "running") inst.started_at = now;
            inst_repo.insert(ctx_, inst);
        }

        api::domain::service_event ev;
        ev.occurred_at = now;
        ev.event_id = boost::uuids::random_generator()();
        ev.service_name = service_name;
        ev.replica_index = replica_index;
        ev.event_type = event_type;
        ev.message = event_type + " requested via NATS";
        event_repo.insert(ctx_, ev);
    }

    template<typename Req, typename Resp, typename Fn>
    void dispatch_to_supervisor(ores::nats::message msg, Fn&& action) {
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "controller::instances:manage")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<Req>(msg);
        if (!req) {
            BOOST_LOG_SEV(service_instance_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        const int replica = req->replica_index.value_or(0);
        action(req->service_name, replica);
        Resp resp;
        resp.success = true;
        reply(nats_, msg, resp);
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
    service::process_supervisor* supervisor_;
};

} // namespace ores::controller::messaging
#endif
