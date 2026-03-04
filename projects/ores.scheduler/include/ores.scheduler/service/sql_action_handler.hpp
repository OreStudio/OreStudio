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
#pragma once

#include "ores.scheduler/service/action_handler.hpp"

namespace ores::scheduler::service {

/**
 * @brief Executes a scheduled SQL command via the in-process database connection.
 *
 * Handles jobs with action_type == "execute_sql". The SQL to run comes from
 * job_definition::command.
 */
class sql_action_handler final : public action_handler {
public:
    [[nodiscard]] std::string_view action_type() const noexcept override {
        return "execute_sql";
    }

    boost::asio::awaitable<std::expected<void, std::string>>
    execute(const action_context& ctx) override;
};

} // namespace ores::scheduler::service
