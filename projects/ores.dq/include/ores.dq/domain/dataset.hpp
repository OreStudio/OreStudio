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
#ifndef ORES_DQ_DOMAIN_DATASET_HPP
#define ORES_DQ_DOMAIN_DATASET_HPP

#include <chrono>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>

namespace ores::dq::domain {

/**
 * @brief Represents a data quality dataset with lineage tracking.
 *
 * A dataset captures metadata about a collection of data including its
 * origin, nature, treatment methodology, and lineage information.
 */
struct dataset final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief UUID uniquely identifying this dataset.
     *
     * This is the surrogate key for the dataset.
     */
    boost::uuids::uuid id;

    /**
     * @brief Unique code for stable referencing.
     *
     * Uses dot notation for namespacing (e.g., "iso.currencies",
     * "fpml.currencies", "crypto.large").
     */
    std::string code;

    /**
     * @brief Optional catalog this dataset belongs to.
     *
     * Links to catalog for organizational grouping.
     */
    std::optional<std::string> catalog_name;

    /**
     * @brief Subject area this dataset belongs to.
     *
     * Links to subject_area for organizational structure.
     */
    std::string subject_area_name;

    /**
     * @brief Data domain this dataset applies to.
     *
     * Links to data_domain for domain categorization.
     */
    std::string domain_name;

    /**
     * @brief Optional coding scheme used for identifiers in this dataset.
     *
     * Links to coding_scheme.
     */
    std::optional<std::string> coding_scheme_code;

    /**
     * @brief Code indicating the origin of the data.
     *
     * Links to origin_dimension.
     */
    std::string origin_code;

    /**
     * @brief Code indicating the nature of the data.
     *
     * Links to nature_dimension.
     */
    std::string nature_code;

    /**
     * @brief Code indicating how the data was treated or processed.
     *
     * Links to treatment_dimension.
     */
    std::string treatment_code;

    /**
     * @brief Optional methodology used to produce this dataset.
     *
     * Links to methodology by UUID.
     */
    std::optional<boost::uuids::uuid> methodology_id;

    /**
     * @brief Human-readable name for the dataset.
     */
    std::string name;

    /**
     * @brief Detailed description of the dataset's contents and purpose.
     */
    std::string description;

    /**
     * @brief Identifier of the source system where data originated.
     */
    std::string source_system_id;

    /**
     * @brief Business context describing the dataset's role and usage.
     */
    std::string business_context;

    /**
     * @brief Optional reference to an upstream dataset this was derived from.
     *
     * Links to another dataset by UUID for lineage tracking.
     */
    std::optional<boost::uuids::uuid> upstream_derivation_id;

    /**
     * @brief Depth in the derivation chain from the original source.
     *
     * 0 indicates an original source dataset.
     */
    int lineage_depth = 0;

    /**
     * @brief Business date the data represents.
     *
     * Stored as time_point, typically truncated to day precision.
     */
    std::chrono::system_clock::time_point as_of_date;

    /**
     * @brief Timestamp when the data was ingested into the system.
     */
    std::chrono::system_clock::time_point ingestion_timestamp;

    /**
     * @brief Optional license information for the data.
     */
    std::optional<std::string> license_info;

    /**
     * @brief Type of artefact this dataset populates.
     *
     * Used for categorization. Actual publication uses target_table and
     * populate_function fields.
     */
    std::optional<std::string> artefact_type;

    /**
     * @brief Target table where data is published.
     *
     * The production table that receives published data from the artefact table.
     * Example: "refdata_currencies_tbl", "assets_images_tbl".
     */
    std::optional<std::string> target_table;

    /**
     * @brief SQL function that publishes data from artefact to production.
     *
     * The function name (without schema) to call for publication.
     * Example: "dq_populate_currencies", "dq_populate_images".
     */
    std::optional<std::string> populate_function;

    /**
     * @brief Username of the person who last modified this dataset.
     */
    std::string recorded_by;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
