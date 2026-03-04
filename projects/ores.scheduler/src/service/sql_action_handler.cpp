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
#include "ores.scheduler/service/sql_action_handler.hpp"

#include "ores.logging/make_logger.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"

namespace ores::scheduler::service {

using namespace ores::logging;
using namespace ores::database::repository;

namespace {

auto& lg() {
    static auto instance = make_logger("ores.scheduler.service.sql_action_handler");
    return instance;
}

} // anonymous namespace

boost::asio::awaitable<std::expected<void, std::string>>
sql_action_handler::execute(const action_context& ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Executing SQL action for job: "
                               << ctx.job.job_name;
    try {
        execute_parameterized_command(ctx.db_ctx, ctx.job.command, {},
            lg(), "executing scheduled SQL");
        BOOST_LOG_SEV(lg(), info) << "SQL action succeeded for job: "
                                  << ctx.job.job_name;
        co_return std::expected<void, std::string>{};
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "SQL action failed for job: "
                                   << ctx.job.job_name << ": " << e.what();
        co_return std::unexpected(std::string(e.what()));
    }
}

} // namespace ores::scheduler::service
