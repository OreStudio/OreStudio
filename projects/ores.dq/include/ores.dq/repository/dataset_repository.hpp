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
#ifndef ORES_DQ_REPOSITORY_DATASET_REPOSITORY_HPP
#define ORES_DQ_REPOSITORY_DATASET_REPOSITORY_HPP

#include <string>
#include <vector>
#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.dq/domain/dataset.hpp"

namespace ores::dq::repository {

/**
 * @brief Reads and writes datasets to data storage.
 */
class dataset_repository {
private:
    inline static std::string_view logger_name =
        "ores.dq.repository.dataset_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit dataset_repository(context ctx);

    std::string sql();

    void write(const domain::dataset& dataset);
    void write(const std::vector<domain::dataset>& datasets);

    std::vector<domain::dataset> read_latest();
    std::vector<domain::dataset> read_latest(const boost::uuids::uuid& id);
    std::vector<domain::dataset>
    read_latest(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Reads latest datasets by catalog.
     */
    std::vector<domain::dataset>
    read_latest_by_catalog(const std::string& catalog_name);

    /**
     * @brief Reads latest datasets by subject area.
     */
    std::vector<domain::dataset>
    read_latest_by_subject_area(const std::string& subject_area_name,
                                const std::string& domain_name);

    /**
     * @brief Reads latest datasets by origin.
     */
    std::vector<domain::dataset>
    read_latest_by_origin(const std::string& origin_code);

    std::uint32_t get_total_count();
    std::vector<domain::dataset> read_all(const boost::uuids::uuid& id);
    void remove(const boost::uuids::uuid& id);

private:
    context ctx_;
};

}

#endif
