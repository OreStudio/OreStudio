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
#ifndef ORES_DQ_DOMAIN_PUBLICATION_RESULT_HPP
#define ORES_DQ_DOMAIN_PUBLICATION_RESULT_HPP

#include <cstdint>
#include <string>
#include <boost/uuid/uuid.hpp>

namespace ores::dq::domain {

/**
 * @brief Result of publishing a single dataset to a target table.
 *
 * Contains the outcome of a publication operation, including counts
 * of records inserted, skipped, and deleted.
 */
struct publication_result final {
    /**
     * @brief ID of the dataset that was published.
     */
    boost::uuids::uuid dataset_id;

    /**
     * @brief Code of the dataset that was published.
     *
     * Examples: "iso.currencies", "assets.country_flags".
     */
    std::string dataset_code;

    /**
     * @brief Human-readable name of the dataset.
     */
    std::string dataset_name;

    /**
     * @brief Name of the production table that received the data.
     *
     * Examples: "refdata_currencies_tbl", "assets_images_tbl".
     */
    std::string target_table;

    /**
     * @brief Number of records inserted during publication.
     */
    std::uint64_t records_inserted = 0;

    /**
     * @brief Number of records skipped during publication.
     *
     * Records are skipped when they already exist and the mode
     * doesn't allow updates (insert_only mode).
     */
    std::uint64_t records_skipped = 0;

    /**
     * @brief Number of records deleted during publication.
     *
     * Only non-zero when using replace_all mode.
     */
    std::uint64_t records_deleted = 0;

    /**
     * @brief Whether the publication succeeded.
     */
    bool success = true;

    /**
     * @brief Error message if publication failed.
     */
    std::string error_message;
};

}

#endif
