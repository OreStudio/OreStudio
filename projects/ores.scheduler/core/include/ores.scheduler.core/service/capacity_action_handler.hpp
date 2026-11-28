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
#ifndef ORES_SCHEDULER_CORE_SERVICE_CAPACITY_ACTION_HANDLER_HPP
#define ORES_SCHEDULER_CORE_SERVICE_CAPACITY_ACTION_HANDLER_HPP

#include "ores.scheduler.core/export.hpp"
#include "ores.scheduler.core/service/action_handler.hpp"

namespace ores::scheduler::service {

/**
 * @brief Measures the installation's own size and the space it sits on.
 *
 * Handles jobs with action_type == "sample_capacity". No SQL can do this: the
 * database can state its own size, and only a process on the host can ask a
 * filesystem how much room is left. So the action is the scheduler's, which runs
 * on the host, and the database's size is read by the function that writes the
 * row, so neither half can be reported by a caller that guessed it.
 *
 * The payload names the path to measure, =/= by default: a mount is what fills,
 * and it is the deployment that knows whether the database sits on one of its
 * own.
 *
 * A path the host cannot measure is not a failed firing. The row is written with
 * the database's size and no free space, and its note says why: a person reading
 * the panel must be able to tell an unreadable disk from a full one.
 */
class ORES_SCHEDULER_CORE_EXPORT capacity_action_handler final : public action_handler {
public:
    capacity_action_handler() = default;

    [[nodiscard]] std::string_view action_type() const noexcept override {
        return "sample_capacity";
    }

    boost::asio::awaitable<std::expected<void, std::string>>
    execute(const action_context& ctx) override;
};

}

#endif
