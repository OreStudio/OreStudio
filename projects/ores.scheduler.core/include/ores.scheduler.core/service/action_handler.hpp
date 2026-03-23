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

#include <expected>
#include <string>
#include <boost/asio/awaitable.hpp>
#include "ores.database/domain/context.hpp"
#include "ores.scheduler.api/domain/job_definition.hpp"

namespace ores::scheduler::service {

/**
 * @brief Context passed to each action handler on job firing.
 */
struct action_context {
    const domain::job_definition& job;
    database::context db_ctx;
};

/**
 * @brief Abstract interface for job action execution.
 *
 * Implementations execute one action_type (e.g., "execute_sql", "send_mq_message").
 */
class action_handler {
public:
    virtual ~action_handler() = default;

    /**
     * @brief Returns the action_type string handled by this implementation.
     */
    [[nodiscard]] virtual std::string_view action_type() const noexcept = 0;

    /**
     * @brief Execute the action for the given job.
     *
     * @return An expected<void, string> where the string contains the error
     *         message on failure.
     */
    virtual boost::asio::awaitable<std::expected<void, std::string>>
    execute(const action_context& ctx) = 0;
};

} // namespace ores::scheduler::service
