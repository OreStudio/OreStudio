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
#ifndef ORES_VARIABILITY_MESSAGING_OPERATIONS_HANDLER_HPP
#define ORES_VARIABILITY_MESSAGING_OPERATIONS_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/change_reason_constants.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.variability.api/messaging/operations_protocol.hpp"
#include "ores.variability.core/service/system_settings_service.hpp"
#include <optional>

namespace ores::variability::messaging {

namespace {
inline auto& operations_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.variability.messaging.operations_handler");
    return instance;
}
} // namespace

/**
 * @brief Serves the component's own operations, which are not entity verbs.
 *
 * Neither operation is a write of one setting. Clearing bootstrap mode ends a
 * tenant's bootstrap window by changing two settings at once, and completing a
 * party's onboarding names a party that is not the caller's own. Neither can be
 * an ordinary entity write, so both are domain operations and both live in the
 * reserved =ops= namespace.
 *
 * Neither checks a permission, deliberately. Clearing bootstrap mode has to
 * work for the tenant that is activating whether or not the activating account
 * holds the permission an ordinary settings write needs, and completing a
 * party's onboarding has the same trust model. Authentication is still
 * required: the tenant comes from the validated token below and never from the
 * request, so an operation can only ever affect a party inside the caller's own
 * tenant.
 */
class operations_handler {
public:
    operations_handler(ores::nats::service::client& nats,
                       ores::database::context ctx,
                       std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void clear_bootstrap_mode(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(operations_handler_lg(), debug) << "Handling " << msg.subject;

        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            ores::service::messaging::error_reply(nats_, msg, ctx_expected.error());
            return;
        }

        const auto& ctx = *ctx_expected;
        try {
            service::system_settings_service svc(ctx, ctx.tenant_id().to_string());
            svc.refresh();
            svc.set_bootstrap_mode(
                false,
                ctx.service_account(),
                std::string(ores::dq::domain::change_reason_constants::codes::new_record),
                "Bootstrap mode cleared on tenant activation");
            // The tenant's onboarding flag rides the same activation event:
            // same scope, same trust model, same caller.
            svc.set_onboarding_tenant_complete(
                true,
                ctx.service_account(),
                std::string(ores::dq::domain::change_reason_constants::codes::new_record),
                "Tenant onboarding completed on tenant activation");
            ores::service::messaging::reply(nats_, msg, clear_bootstrap_mode_response{});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(operations_handler_lg(), error) << msg.subject << " failed: " << e.what();
            clear_bootstrap_mode_response resp;
            resp.result.outcome = ores::utility::domain::outcome::failed;
            resp.result.code = "operation_failed";
            resp.result.message = e.what();
            ores::service::messaging::reply(nats_, msg, resp);
        }
    }

    void complete_party_onboarding(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(operations_handler_lg(), debug) << "Handling " << msg.subject;

        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            ores::service::messaging::error_reply(nats_, msg, ctx_expected.error());
            return;
        }

        auto req = ores::service::messaging::decode<complete_party_onboarding_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(operations_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            ores::service::messaging::error_reply(
                nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        const auto& ctx = *ctx_expected;
        try {
            service::system_settings_service svc(
                ctx, ctx.tenant_id().to_string(), boost::uuids::to_string(req->party_id));
            svc.refresh();
            svc.set_onboarding_party_complete(
                true,
                ctx.service_account(),
                std::string(ores::dq::domain::change_reason_constants::codes::new_record),
                "Party onboarding completed on party activation");
            ores::service::messaging::reply(nats_, msg, complete_party_onboarding_response{});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(operations_handler_lg(), error) << msg.subject << " failed: " << e.what();
            complete_party_onboarding_response resp;
            resp.result.outcome = ores::utility::domain::outcome::failed;
            resp.result.code = "operation_failed";
            resp.result.message = e.what();
            ores::service::messaging::reply(nats_, msg, resp);
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::variability::messaging

#endif
