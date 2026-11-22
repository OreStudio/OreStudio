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
#include "ores.orgmode.core/messaging/publish_from_dq_handler.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.dq.api/messaging/publish_from_dq_protocol.hpp"
#include "ores.logging/logger.hpp"
#include "ores.orgmode.core/service/knowledge_indexer.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <cstdint>
#include <exception>
#include <string>
#include <string_view>

namespace ores::orgmode::messaging {

using namespace ores::logging;
using ores::service::messaging::workflow_step_context;

namespace {

inline std::string_view logger_name = "ores.orgmode.messaging.publish_from_dq_handler";
auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

constexpr std::string_view fn_name = "ores_orgmode_publish_knowledge_pages_from_dq_fn";

} // namespace

publish_from_dq_handler::publish_from_dq_handler(ores::nats::service::client& nats,
                                                 const ores::database::context& ctx)
    : nats_(nats)
    , ctx_(ctx) {}

void publish_from_dq_handler::handle(ores::nats::delivery delivery) {
    const auto& msg = delivery.msg();

    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf) {
        // Not a step command at all, so there is no step to fail and nothing
        // a second delivery would achieve.
        std::move(delivery).reject();
        return;
    }

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    ores::dq::messaging::publish_from_dq_command cmd;
    try {
        cmd = ores::utility::codec::json::read<ores::dq::messaging::publish_from_dq_command>(sv);
    } catch (const std::exception& e) {
        wf->fail("Failed to decode publish_from_dq_command: " + std::string(e.what()));
        std::move(delivery).commit();
        return;
    }

    ORES_LOG_SEV(lg(), info) << "publish_from_dq: dataset=" << cmd.dataset_id
                             << " mode=" << cmd.mode << " step=" << wf->step_id;

    try {
        auto tenant_ctx = ores::database::service::tenant_context::with_tenant(ctx_, wf->tenant_id);

        const std::string sql = "SELECT action, record_count FROM " + std::string(fn_name) +
                                "($1::uuid, $2::uuid, $3::text, $4::jsonb)";
        const std::string params_json = cmd.params_json.empty() ? "{}" : cmd.params_json;

        auto rows = ores::database::query_builder::execute_parameterized_multi_column_query(
            tenant_ctx,
            sql,
            {cmd.dataset_id, cmd.tenant_id, cmd.mode, params_json},
            lg(),
            std::string(fn_name));

        ores::dq::messaging::publish_from_dq_result result;
        result.success = true;
        for (const auto& row : rows) {
            if (row.size() < 2 || !row[0].has_value() || !row[1].has_value())
                continue;
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

        ORES_LOG_SEV(lg(), info) << "publish_from_dq complete: inserted=" << result.records_inserted
                                 << " updated=" << result.records_updated
                                 << " skipped=" << result.records_skipped
                                 << " deleted=" << result.records_deleted;

        // A publish that changed pages leaves the index describing the old
        // ones, so rebuild it before reporting the step complete.
        if (result.records_inserted + result.records_updated + result.records_deleted > 0)
            service::knowledge_indexer(ctx_).reindex();

        wf->complete(ores::utility::codec::json::write(result));
        std::move(delivery).commit();

    } catch (const std::exception& e) {
        ORES_LOG_SEV(lg(), error) << "publish_from_dq failed: " << e.what();
        wf->fail(e.what());
        // The failure reaches the engine, so the delivery is done with;
        // redelivering would only report the same failure again.
        std::move(delivery).commit();
    }
}

}
