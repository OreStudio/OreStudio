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
#ifndef ORES_DQ_CORE_MESSAGING_REPORT_DEFINITION_TEMPLATE_HANDLER_HPP
#define ORES_DQ_CORE_MESSAGING_REPORT_DEFINITION_TEMPLATE_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.dq.api/messaging/report_definition_template_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>

namespace ores::dq::messaging {

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using namespace ores::logging;

namespace {
inline auto& dq_report_definition_template_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.dq.messaging.report_definition_template_handler");
    return instance;
}
} // namespace

class report_definition_template_handler {
public:
    report_definition_template_handler(
        ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(dq_report_definition_template_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req = decode<list_dq_report_definition_templates_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(dq_report_definition_template_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        // Template data is system-level seed data; query under the system tenant.
        const auto sys_ctx =
            ores::database::service::tenant_context::with_system_tenant(*ctx_expected);
        list_dq_report_definition_templates_response resp;
        try {
            using namespace ores::database::repository;
            const auto rows = execute_parameterized_multi_column_query(
                sys_ctx,
                "SELECT a.name, a.description, a.report_type,"
                "       a.schedule_expression, a.concurrency_policy, a.display_order"
                " FROM ores_dq_report_definitions_artefact_tbl a"
                " JOIN ores_dq_datasets_tbl d ON d.id = a.dataset_id"
                "   AND d.valid_to = ores_utility_infinity_timestamp_fn()"
                "   AND d.tenant_id = ores_utility_system_tenant_id_fn()"
                " JOIN ores_dq_dataset_bundle_members_tbl m ON m.dataset_code = d.code"
                "   AND m.tenant_id = ores_utility_system_tenant_id_fn()"
                " WHERE m.bundle_code = $1"
                " ORDER BY a.display_order, a.name",
                {req->bundle_code},
                dq_report_definition_template_handler_lg(),
                "listing report definition templates for bundle: " + req->bundle_code);
            for (const auto& row : rows) {
                dq_report_definition_template t;
                if (row.size() > 0 && row[0])
                    t.name = *row[0];
                if (row.size() > 1 && row[1])
                    t.description = *row[1];
                if (row.size() > 2 && row[2])
                    t.report_type = *row[2];
                if (row.size() > 3 && row[3])
                    t.schedule_expression = *row[3];
                if (row.size() > 4 && row[4])
                    t.concurrency_policy = *row[4];
                if (row.size() > 5 && row[5])
                    t.display_order = std::stoi(*row[5]);
                resp.templates.push_back(std::move(t));
            }
            resp.success = true;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dq_report_definition_template_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
        }
        BOOST_LOG_SEV(dq_report_definition_template_handler_lg(), debug)
            << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::dq::messaging

#endif
