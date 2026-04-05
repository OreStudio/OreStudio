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
#ifndef ORES_CONTROLLER_CORE_REPOSITORY_SERVICE_EVENT_REPOSITORY_HPP
#define ORES_CONTROLLER_CORE_REPOSITORY_SERVICE_EVENT_REPOSITORY_HPP

#include <string>
#include <vector>
#include <sqlgen/postgres.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.controller.api/domain/service_event.hpp"

namespace ores::controller::repository {

class service_event_repository {
private:
    inline static std::string_view logger_name =
        "ores.controller.repository.service_event_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Returns recent events ordered by occurred_at desc, optionally
     * filtered by service name. Returns at most @p limit events.
     */
    std::vector<api::domain::service_event>
    read_latest(context ctx, const std::string& service_name_filter = "",
        int limit = 100);

    /**
     * @brief Inserts a new service lifecycle event.
     */
    void insert(context ctx, const api::domain::service_event& ev);
};

}

#endif
