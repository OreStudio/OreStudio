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
#include "ores.iam.core/messaging/publish_from_dq_handler.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.dq.api/messaging/publish_from_dq_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include <algorithm>
#include <format>
#include <rfl/json.hpp>
#include <string>

namespace ores::iam::messaging {

using namespace ores::logging;
using namespace ores::database::repository;
using ores::service::messaging::workflow_step_context;

namespace {

inline std::string_view logger_name = "ores.iam.messaging.publish_from_dq_handler";
auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

/**
 * @brief Derives the SQL function name from the NATS subject.
 *
 * Converts "iam.v1.ops.publish_accounts_from_dq" to
 * "ores_iam_publish_accounts_from_dq_fn", same convention as
 * ores.refdata's handler.
 */
std::string subject_to_fn(std::string_view subject) {
    // The operation is the last segment of a reserved-namespace subject,
    // so the function it expands through is named for the operation itself.
    constexpr std::string_view ops = "v1.ops.";
    const auto pos = subject.find(ops);
    if (pos == std::string_view::npos)
        return {};
    const auto operation = subject.substr(pos + ops.size());
    if (operation.empty())
        return {};
    return "ores_iam_" + std::string(operation) + "_fn";
}

} // namespace

publish_from_dq_handler::publish_from_dq_handler(ores::nats::service::client& nats,
                                                 ores::database::context ctx)
    : nats_(nats)
    , ctx_(std::move(ctx)) {}

void publish_from_dq_handler::handle(ores::nats::message msg) {
    // A durable consumer answers every delivery, so the handler states the
    // outcome on each path. The guard retries anything that returns without
    // deciding, which is the one thing that must not happen silently.
    ores::nats::ack_guard ack(msg);

    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf) {
        // Not a step command at all, so there is no step to fail and nothing
        // a second delivery would achieve.
        ack.reject();
        return;
    }

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = rfl::json::read<ores::dq::messaging::publish_from_dq_command>(sv);
    if (!parsed) {
        wf->fail("Failed to decode publish_from_dq_command: " + std::string(parsed.error().what()));
        ack.accept();
        return;
    }
    const auto& cmd = *parsed;

    const auto fn_name = subject_to_fn(msg.subject);
    if (fn_name.empty()) {
        wf->fail("Cannot derive SQL function from subject: " + std::string(msg.subject));
        ack.accept();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "publish_from_dq: fn=" << fn_name << " dataset=" << cmd.dataset_id
                              << " mode=" << cmd.mode << " step=" << wf->step_id << " params="
                              << (cmd.params_json.empty() ? "(empty)" : cmd.params_json);

    try {
        auto tenant_ctx = ores::database::service::tenant_context::with_tenant(ctx_, wf->tenant_id);

        const auto sql = std::format(
            "SELECT action, record_count FROM {}($1::uuid, $2::uuid, $3::text, $4::jsonb)",
            fn_name);
        const std::string params_json = cmd.params_json.empty() ? "{}" : cmd.params_json;

        auto rows = execute_parameterized_multi_column_query(
            tenant_ctx, sql, {cmd.dataset_id, cmd.tenant_id, cmd.mode, params_json}, lg(), fn_name);

        ores::dq::messaging::publish_from_dq_result result;
        result.success = true;
        for (const auto& row : rows) {
            if (row.size() >= 2 && row[0].has_value() && row[1].has_value()) {
                const auto count = static_cast<std::uint64_t>(std::stoll(*row[1]));
                if (*row[0] == "inserted")
                    result.records_inserted += count;
                else if (*row[0] == "updated")
                    result.records_updated += count;
                else if (*row[0] == "skipped")
                    result.records_skipped += count;
                else if (*row[0] == "deleted")
                    result.records_deleted += count;
            }
        }

        BOOST_LOG_SEV(lg(), info) << "publish_from_dq complete: fn=" << fn_name
                                  << " inserted=" << result.records_inserted
                                  << " updated=" << result.records_updated;

        wf->complete(rfl::json::write(result));
        ack.accept();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error)
            << "publish_from_dq failed: fn=" << fn_name << " error=" << e.what();
        wf->fail(e.what());
        // The failure reaches the engine, so the delivery is done with;
        // redelivering would only report the same failure again.
        ack.accept();
    }
}

}
