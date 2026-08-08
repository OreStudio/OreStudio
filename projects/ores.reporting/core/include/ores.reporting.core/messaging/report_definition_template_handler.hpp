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

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/messaging/report_definition_template_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>

namespace ores::reporting::messaging {

struct get_report_definition_templates_request {
    using response_type = struct get_report_definition_templates_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.report-definition-templates.list";
    std::string bundle_code = "risk_management";
};

struct get_report_definition_templates_response {
    bool success = false;
    std::string message;
    std::vector<ores::dq::messaging::dq_report_definition_template> templates;
};

namespace {
inline auto& report_definition_template_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.reporting.messaging.report_definition_template_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using namespace ores::logging;

class ORES_REPORTING_CORE_EXPORT report_definition_template_handler {
public:
    report_definition_template_handler(
        ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(report_definition_template_handler_lg(), debug) << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        ores::dq::messaging::list_dq_report_definition_templates_response resp;
        try {
            std::string bundle_code = "risk_management";
            if (auto req =
                    decode<ores::dq::messaging::list_dq_report_definition_templates_request>(msg))
                bundle_code = req->bundle_code;

            ores::dq::messaging::list_dq_report_definition_templates_request dq_req;
            dq_req.bundle_code = bundle_code;
            const auto& codec = ores::nats::default_wire_codec();
            const auto dq_reply = nats_.request_sync(
                ores::dq::messaging::list_dq_report_definition_templates_request::nats_subject,
                codec.encode(dq_req));
            const auto dq_resp =
                codec.decode<ores::dq::messaging::list_dq_report_definition_templates_response>(
                    dq_reply.data);
            if (!dq_resp || !dq_resp->success) {
                resp.success = false;
                resp.message = dq_resp ? dq_resp->message : dq_resp.error().what();
                reply(nats_, msg, resp);
                return;
            }
            resp.templates = dq_resp->templates;
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
