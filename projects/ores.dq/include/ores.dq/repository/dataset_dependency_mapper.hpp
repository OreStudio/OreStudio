/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_DQ_REPOSITORY_DATASET_DEPENDENCY_MAPPER_HPP
#define ORES_DQ_REPOSITORY_DATASET_DEPENDENCY_MAPPER_HPP

#include "ores.dq/domain/dataset_dependency.hpp"
#include "ores.dq/repository/dataset_dependency_entity.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::dq::repository {

/**
 * @brief Maps dataset_dependency domain entities to data storage and vice-versa.
 */
class dataset_dependency_mapper {
private:
    inline static std::string_view logger_name =
        "ores.dq.repository.dataset_dependency_mapper";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }
public:
    static domain::dataset_dependency map(const dataset_dependency_entity& v);

    static std::vector<domain::dataset_dependency>
    map(const std::vector<dataset_dependency_entity>& v);
};

}

#endif
