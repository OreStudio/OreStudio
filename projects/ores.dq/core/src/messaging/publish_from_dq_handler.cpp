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
#include "ores.dq.core/messaging/publish_from_dq_handler.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.dq.api/messaging/publish_from_dq_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include <format>
#include <rfl/json.hpp>
#include <string>
#include <unordered_map>

namespace ores::dq::messaging {

using namespace ores::logging;
using namespace ores::database::repository;
using ores::service::messaging::workflow_step_context;

namespace {

inline std::string_view logger_name = "ores.dq.messaging.publish_from_dq_handler";
auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

const std::unordered_map<std::string, std::string>& subject_fn_map() {
    static const std::unordered_map<std::string, std::string> m{
        {"dq.v1.ip2country.publish-from-dq", "ores_dq_ip2country_publish_fn"},
        {"dq.v1.coding-schemes.publish-from-dq", "ores_dq_coding_schemes_publish_fn"},
        {"dq.v1.badge-severities.publish-from-dq", "ores_dq_badge_severities_publish_fn"},
    };
    return m;
}

} // namespace

publish_from_dq_handler::publish_from_dq_handler(ores::nats::service::client& nats,
                                                 ores::database::context ctx)
    : nats_(nats)
    , ctx_(std::move(ctx)) {}

void publish_from_dq_handler::handle(ores::nats::message msg) {
    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf)
        return;

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = rfl::json::read<ores::dq::messaging::publish_from_dq_command>(sv);
    if (!parsed) {
        wf->fail("Failed to decode publish_from_dq_command: " + std::string(parsed.error().what()));
        return;
    }
    const auto& cmd = *parsed;

    // Strip namespace prefix (e.g. "ores.dev.local3.") before map lookup.
    const auto v1_pos = msg.subject.find("dq.v1.");
    const std::string bare_subject = v1_pos != std::string_view::npos ?
                                         std::string(msg.subject.substr(v1_pos)) :
                                         std::string(msg.subject);

    const auto& m = subject_fn_map();
    const auto it = m.find(bare_subject);
    if (it == m.end()) {
        wf->fail("Unknown DQ publish-from-dq subject: " + std::string(msg.subject));
        return;
    }
    const auto& fn_name = it->second;

    BOOST_LOG_SEV(lg(), info) << "publish_from_dq: fn=" << fn_name << " dataset=" << cmd.dataset_id
                              << " mode=" << cmd.mode << " step=" << wf->step_id;

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

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error)
            << "publish_from_dq failed: fn=" << fn_name << " error=" << e.what();
        wf->fail(e.what());
    }
}

}
