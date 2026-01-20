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
#ifndef ORES_DQ_REPOSITORY_DATASET_DEPENDENCY_REPOSITORY_HPP
#define ORES_DQ_REPOSITORY_DATASET_DEPENDENCY_REPOSITORY_HPP

#include <string>
#include <vector>
#include <sqlgen/postgres.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.dq/domain/dataset_dependency.hpp"

namespace ores::dq::repository {

/**
 * @brief Reads dataset dependencies from data storage.
 */
class dataset_dependency_repository {
private:
    inline static std::string_view logger_name =
        "ores.dq.repository.dataset_dependency_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit dataset_dependency_repository(context ctx);

    /**
     * @brief Reads all active dataset dependencies.
     */
    std::vector<domain::dataset_dependency> read_latest();

    /**
     * @brief Reads all active dependencies for a specific dataset.
     * @param dataset_code The code of the dataset to query dependencies for
     */
    std::vector<domain::dataset_dependency>
    read_latest_by_dataset(const std::string& dataset_code);

private:
    context ctx_;
};

}

#endif
