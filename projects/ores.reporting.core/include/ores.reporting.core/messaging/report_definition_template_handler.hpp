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
#ifndef ORES_REPORTING_MESSAGING_REPORT_DEFINITION_TEMPLATE_HANDLER_HPP
#define ORES_REPORTING_MESSAGING_REPORT_DEFINITION_TEMPLATE_HANDLER_HPP

#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.reporting.api/messaging/report_definition_protocol.hpp"
#include "ores.reporting.core/service/report_definition_template_service.hpp"

namespace ores::reporting::messaging {

namespace {
inline auto& report_definition_template_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.reporting.messaging.report_definition_template_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using namespace ores::logging;

class report_definition_template_handler {
public:
    report_definition_template_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(report_definition_template_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        // Template data is system-level seed data; query under the system tenant.
        const auto sys_ctx = ores::database::service::tenant_context::with_system_tenant(
            *ctx_expected);
        get_report_definition_templates_response resp;
        try {
            std::string bundle_code = "ore_analytics";
            if (auto req = decode<get_report_definition_templates_request>(msg))
                bundle_code = req->bundle_code;
            service::report_definition_template_service svc(sys_ctx);
            resp.templates = svc.list_templates(bundle_code);
            resp.success = true;
        } catch (const std::exception& e) {
            resp.success = false;
            resp.message = e.what();
        }
        reply(nats_, msg, resp);
        BOOST_LOG_SEV(report_definition_template_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::reporting::messaging

#endif
