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
#ifndef ORES_CONTROLLER_CORE_REPOSITORY_SERVICE_DEFINITION_MAPPER_HPP
#define ORES_CONTROLLER_CORE_REPOSITORY_SERVICE_DEFINITION_MAPPER_HPP

#include <vector>
#include "ores.logging/make_logger.hpp"
#include "ores.controller.api/domain/service_definition.hpp"
#include "ores.controller.core/repository/service_definition_entity.hpp"

namespace ores::controller::repository {

class service_definition_mapper {
private:
    inline static std::string_view logger_name =
        "ores.controller.repository.service_definition_mapper";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static api::domain::service_definition map(const service_definition_entity& v);
    static service_definition_entity map(const api::domain::service_definition& v);

    static std::vector<api::domain::service_definition>
    map(const std::vector<service_definition_entity>& v);
};

}

#endif
