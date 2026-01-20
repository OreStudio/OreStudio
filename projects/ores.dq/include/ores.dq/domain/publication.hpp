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
#ifndef ORES_DQ_DOMAIN_PUBLICATION_HPP
#define ORES_DQ_DOMAIN_PUBLICATION_HPP

#include <chrono>
#include <cstdint>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.dq/domain/publication_mode.hpp"

namespace ores::dq::domain {

/**
 * @brief Audit record for a dataset publication event.
 *
 * Records when a dataset was published, by whom, and the outcome.
 * Used for tracking publication history and auditing.
 */
struct publication final {
    /**
     * @brief Unique identifier for this publication record.
     */
    boost::uuids::uuid id;

    /**
     * @brief ID of the dataset that was published.
     */
    boost::uuids::uuid dataset_id;

    /**
     * @brief Code of the dataset that was published.
     *
     * Stored for convenience and historical reference even if
     * the dataset is later deleted.
     */
    std::string dataset_code;

    /**
     * @brief Publication mode used for this operation.
     */
    publication_mode mode = publication_mode::upsert;

    /**
     * @brief Name of the production table that received the data.
     */
    std::string target_table;

    /**
     * @brief Number of records inserted during publication.
     */
    std::uint64_t records_inserted = 0;

    /**
     * @brief Number of records skipped during publication.
     */
    std::uint64_t records_skipped = 0;

    /**
     * @brief Number of records deleted during publication.
     */
    std::uint64_t records_deleted = 0;

    /**
     * @brief Username of the person who initiated the publication.
     */
    std::string published_by;

    /**
     * @brief Timestamp when the publication occurred.
     */
    std::chrono::system_clock::time_point published_at;
};

}

#endif
