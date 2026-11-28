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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.scheduler.core/service/capacity_action_handler.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.logging/logger.hpp"
#include "ores.scheduler.core/service/action_payloads.hpp"
#include "ores.scheduler.core/service/filesystem_capacity.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include <string>

namespace ores::scheduler::service {

namespace {

auto& capacity_action_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.scheduler.service.capacity_action_handler");
    return instance;
}

using namespace ores::logging;

/**
 * @brief The database this process is connected to, as the server names it.
 *
 * The row states which database it describes, and the connection is the only
 * place that says so: a configuration value could name a database the process is
 * not looking at.
 */
std::string current_database(const database::context& ctx) {
    const auto rows =
        database::query_builder::execute_raw_multi_column_query(ctx,
                                                                "select current_database()",
                                                                capacity_action_handler_lg(),
                                                                "Reading the database name");
    if (rows.empty() || rows.front().empty() || !rows.front().front())
        throw std::runtime_error("the connection did not name its database");
    return *rows.front().front();
}

/**
 * @brief Writes the row, with the database's size read by the function itself.
 *
 * The free space travels as text because the query helper takes text, and an
 * absent figure as an empty string, which the function reads as null.
 */
void record_capacity(const database::context& ctx,
                     const std::string& database_name,
                     const std::string& host_name,
                     const std::string& path,
                     const std::optional<filesystem_capacity>& capacity,
                     const std::string& note) {
    const auto number = [](std::uint64_t value) {
        return std::to_string(value);
    };
    database::query_builder::execute_parameterized_command(
        ctx,
        "select ores_scheduler_record_capacity_fn($1, $2, $3, "
        "nullif($4, '')::bigint, nullif($5, '')::bigint, $6)",
        {database_name,
         host_name,
         path,
         capacity ? number(capacity->free_bytes) : std::string{},
         capacity ? number(capacity->total_bytes) : std::string{},
         note},
        capacity_action_handler_lg(),
        "Recording a capacity sample");
}

}

boost::asio::awaitable<std::expected<void, std::string>>
capacity_action_handler::execute(const action_context& ctx) {
    ORES_LOG_SEV(capacity_action_handler_lg(), debug)
        << "Executing the capacity sample for job: " << ctx.job.job_name;

    capacity_payload payload;
    try {
        if (!ctx.job.action_payload.empty())
            payload = utility::codec::object::read_json<capacity_payload>(ctx.job.action_payload);
    } catch (const std::exception& e) {
        const auto msg =
            "Failed to parse action_payload for job '" + ctx.job.job_name + "': " + e.what();
        ORES_LOG_SEV(capacity_action_handler_lg(), error) << msg;
        co_return std::unexpected(msg);
    }
    if (payload.path.empty())
        payload.path = "/";

    std::string path_error;
    const auto capacity = filesystem_capacity_of(payload.path, path_error);
    if (!capacity) {
        ORES_LOG_SEV(capacity_action_handler_lg(), warn)
            << "The capacity sample measured no free space: " << path_error;
    }

    std::string database_name;
    try {
        database_name = current_database(ctx.db_ctx);
    } catch (const std::exception& e) {
        co_return std::unexpected(std::string("Naming the database failed: ") + e.what());
    }

    try {
        record_capacity(
            ctx.db_ctx, database_name, this_host_name(), payload.path, capacity, path_error);
    } catch (const std::exception& e) {
        co_return std::unexpected(std::string("Recording the capacity sample failed: ") + e.what());
    }

    co_return std::expected<void, std::string>{};
}

}
