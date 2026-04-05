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
#ifndef ORES_CONTROLLER_CORE_REPOSITORY_SERVICE_INSTANCE_REPOSITORY_HPP
#define ORES_CONTROLLER_CORE_REPOSITORY_SERVICE_INSTANCE_REPOSITORY_HPP

#include <string>
#include <optional>
#include <vector>
#include <sqlgen/postgres.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.controller.api/domain/service_instance.hpp"

namespace ores::controller::repository {

class service_instance_repository {
private:
    inline static std::string_view logger_name =
        "ores.controller.repository.service_instance_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Returns all instances, optionally filtered by service name.
     */
    std::vector<api::domain::service_instance>
    read_all(context ctx, const std::string& service_name_filter = "");

    /**
     * @brief Returns a single instance by (service_name, replica_index), or
     * nullopt if not found.
     */
    std::optional<api::domain::service_instance>
    read(context ctx, const std::string& service_name, int replica_index);

    /**
     * @brief Inserts a new instance row.
     */
    void insert(context ctx, const api::domain::service_instance& v);

    /**
     * @brief Updates phase (and optionally pid/timestamps) for an existing
     * instance.
     */
    void update_phase(context ctx, const api::domain::service_instance& v);
};

}

#endif
