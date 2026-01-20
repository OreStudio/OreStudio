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
#ifndef ORES_DQ_REPOSITORY_PUBLICATION_REPOSITORY_HPP
#define ORES_DQ_REPOSITORY_PUBLICATION_REPOSITORY_HPP

#include <string>
#include <vector>
#include <cstdint>
#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.dq/domain/publication.hpp"

namespace ores::dq::repository {

/**
 * @brief Repository for reading and writing publication audit records.
 */
class publication_repository {
private:
    inline static std::string_view logger_name =
        "ores.dq.repository.publication_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit publication_repository(context ctx);

    /**
     * @brief Reads all publication records for a dataset.
     * @param dataset_id ID of the dataset.
     * @return Publication records, newest first.
     */
    std::vector<domain::publication>
    read_by_dataset(const boost::uuids::uuid& dataset_id);

    /**
     * @brief Reads recent publication records across all datasets.
     * @param limit Maximum number of records to return.
     * @return Publication records, newest first.
     */
    std::vector<domain::publication>
    read_recent(std::uint32_t limit = 100);

    /**
     * @brief Inserts a new publication record.
     * @param pub The publication record to insert.
     */
    void insert(const domain::publication& pub);

private:
    context ctx_;
};

}

#endif
