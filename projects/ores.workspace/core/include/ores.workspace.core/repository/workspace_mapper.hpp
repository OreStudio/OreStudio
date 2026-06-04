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
#ifndef ORES_WORKSPACE_REPOSITORY_WORKSPACE_MAPPER_HPP
#define ORES_WORKSPACE_REPOSITORY_WORKSPACE_MAPPER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.workspace.api/domain/workspace.hpp"
#include "ores.workspace.core/repository/workspace_entity.hpp"

namespace ores::workspace::repository {

/**
 * @brief Maps workspace domain entities to data storage layer and vice-versa.
 */
class workspace_mapper {
private:
    inline static std::string_view logger_name = "ores.workspace.repository.workspace_mapper";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static domain::workspace map(const workspace_entity& v);
    static workspace_entity map(const domain::workspace& v);

    static std::vector<domain::workspace> map(const std::vector<workspace_entity>& v);
    static std::vector<workspace_entity> map(const std::vector<domain::workspace>& v);
};

}

#endif
