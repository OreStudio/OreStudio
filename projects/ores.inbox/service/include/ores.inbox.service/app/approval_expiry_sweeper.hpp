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
#ifndef ORES_INBOX_SERVICE_APP_APPROVAL_EXPIRY_SWEEPER_HPP
#define ORES_INBOX_SERVICE_APP_APPROVAL_EXPIRY_SWEEPER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <boost/asio/awaitable.hpp>
#include <cstdint>

namespace ores::inbox::service::app {

/**
 * @brief Closes approval requests nobody answered, on a fixed interval.
 *
 * A request past its kind's deadline also closes when a decider reaches for
 * it, which is no use to a queue that nobody opens. The inbox owns the
 * requests, so it sweeps them: once at start, which catches what ran out
 * while the service was down, and then every @p interval_seconds.
 */
class approval_expiry_sweeper {
private:
    inline static std::string_view logger_name =
        "ores.inbox.service.app.approval_expiry_sweeper";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    approval_expiry_sweeper(std::uint32_t interval_seconds, ores::database::context ctx);

    /**
     * @brief Runs forever, sweeping once per interval, until the io_context
     * stops.
     */
    boost::asio::awaitable<void> run();

private:
    void sweep_once();

    std::uint32_t interval_seconds_;
    ores::database::context ctx_;
};

}

#endif
