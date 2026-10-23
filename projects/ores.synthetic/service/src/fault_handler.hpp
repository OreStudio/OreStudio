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
#ifndef ORES_SYNTHETIC_SERVICE_FAULT_HANDLER_HPP
#define ORES_SYNTHETIC_SERVICE_FAULT_HANDLER_HPP

#include "feed_kind_registry.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.synthetic.api/feeds/adapter_faults.hpp"
#include "ores.synthetic.api/messaging/fault_operation_protocol.hpp"
#include <chrono>
#include <memory>
#include <optional>
#include <utility>

namespace ores::synthetic::service {

namespace {
inline auto& fault_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.synthetic.service.fault_handler");
    return instance;
}
} // namespace

/**
 * @brief Sets the faults the simulator injects.
 *
 * The request replaces the plan as a whole. Only a session of the system tenant that may run
 * feeds sets it, because the faults change what the adapter shows to every tenant.
 */
class fault_handler {
public:
    fault_handler(ores::nats::service::client& nats,
                  std::shared_ptr<ores::synthetic::feed::fault_state> faults,
                  ores::database::context ctx,
                  std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , faults_(std::move(faults))
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void set_faults(ores::nats::message msg) {
        using ores::service::messaging::error_reply;
        using ores::service::messaging::reply;
        using namespace ores::logging;

        auto ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx) {
            BOOST_LOG_SEV(fault_handler_lg(), warn)
                << "Rejecting set_faults request: auth failed: " << static_cast<int>(ctx.error());
            error_reply(nats_, msg, ctx.error());
            return;
        }
        if (!feed_kind_registry::permits_run(*ctx) || !feed_kind_registry::is_system_caller(*ctx)) {
            BOOST_LOG_SEV(fault_handler_lg(), warn)
                << "Rejecting set_faults request: the caller is not the system tenant or may not "
                   "run feeds.";
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        auto request =
            ores::service::messaging::decode<ores::synthetic::messaging::set_faults_request>(msg);
        if (!request) {
            reply(nats_,
                  msg,
                  ores::synthetic::messaging::set_faults_response{
                      .success = false, .message = "Failed to decode set_faults request."});
            return;
        }

        faults_->set(plan_of(*request));
        BOOST_LOG_SEV(fault_handler_lg(), info) << "Fault plan set: " << describe(*request);
        reply(nats_,
              msg,
              ores::synthetic::messaging::set_faults_response{.success = true,
                                                              .message = describe(*request)});
    }

    /// The plan a request states.
    static ores::synthetic::feed::fault_plan
    plan_of(const ores::synthetic::messaging::set_faults_request& request) {
        ores::synthetic::feed::fault_plan plan;
        plan.connection_lost = request.connection_lost;
        plan.tick_delay = std::chrono::milliseconds(request.tick_delay_ms);
        plan.repeat_ticks = request.repeat_ticks;
        plan.out_of_range = request.out_of_range;
        plan.not_entitled = request.not_entitled;
        return plan;
    }

private:
    static std::string describe(const ores::synthetic::messaging::set_faults_request& request) {
        return plan_of(request).any() ? "faults on" : "no fault on";
    }

    ores::nats::service::client& nats_;
    std::shared_ptr<ores::synthetic::feed::fault_state> faults_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
