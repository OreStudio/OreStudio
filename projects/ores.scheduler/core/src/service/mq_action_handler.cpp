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
#include "ores.scheduler.core/service/mq_action_handler.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.scheduler.core/service/action_payloads.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::scheduler::service {

using namespace ores::logging;
using namespace ores::database::repository;

namespace {

auto& lg() {
    static auto instance = make_logger("ores.scheduler.service.mq_action_handler");
    return instance;
}


} // anonymous namespace

boost::asio::awaitable<std::expected<void, std::string>>
mq_action_handler::execute(const action_context& ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Executing MQ action for job: " << ctx.job.job_name;
    try {
        // Parse the action_payload JSON to extract required fields.
        mq_payload_fields fields;
        try {
            fields = utility::codec::object::read_json<mq_payload_fields>(ctx.job.action_payload);
        } catch (const std::exception& e) {
            const auto msg = "Failed to parse action_payload JSON for job '" + ctx.job.job_name +
                             "': " + e.what();
            BOOST_LOG_SEV(lg(), error) << msg;
            co_return std::unexpected(msg);
        }
        if (fields.queue_id.empty()) {
            const auto msg =
                std::string("action_payload missing queue_id for job '") + ctx.job.job_name + "'";
            BOOST_LOG_SEV(lg(), error) << msg;
            co_return std::unexpected(msg);
        }

        // Call the ores_mq_messages_send_fn SQL function.
        const std::string sql = "SELECT ores_mq_messages_send_fn("
                                "$1::uuid, $2, $3::jsonb, 0::integer)::bigint";

        execute_parameterized_command(ctx.db_ctx,
                                      sql,
                                      {fields.queue_id, fields.message_type, fields.payload},
                                      lg(),
                                      "Sending MQ message from scheduler.");

        BOOST_LOG_SEV(lg(), info) << "MQ action succeeded for job: " << ctx.job.job_name;
        co_return std::expected<void, std::string>{};
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error)
            << "MQ action failed for job: " << ctx.job.job_name << ": " << e.what();
        co_return std::unexpected(std::string(e.what()));
    }
}

} // namespace ores::scheduler::service
