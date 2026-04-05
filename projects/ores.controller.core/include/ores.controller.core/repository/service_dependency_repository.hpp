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
#ifndef ORES_CONTROLLER_CORE_REPOSITORY_SERVICE_DEPENDENCY_REPOSITORY_HPP
#define ORES_CONTROLLER_CORE_REPOSITORY_SERVICE_DEPENDENCY_REPOSITORY_HPP

#include <string>
#include <utility>
#include <vector>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"

namespace ores::controller::repository {

/**
 * @brief Read-only access to the service startup dependency table.
 */
class service_dependency_repository {
private:
    inline static std::string_view logger_name =
        "ores.controller.repository.service_dependency_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Returns all rows as (service_name, depends_on) pairs.
     */
    std::vector<std::pair<std::string, std::string>>
    read_all(context ctx);
};

}

#endif
