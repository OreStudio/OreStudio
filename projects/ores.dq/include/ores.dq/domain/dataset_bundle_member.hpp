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
#ifndef ORES_DQ_DOMAIN_DATASET_BUNDLE_MEMBER_HPP
#define ORES_DQ_DOMAIN_DATASET_BUNDLE_MEMBER_HPP

#include <chrono>
#include <optional>
#include <string>

namespace ores::dq::domain {

/**
 * @brief Links a dataset to a bundle.
 *
 * Junction table linking bundles to their constituent datasets. Uses codes
 * for loose coupling - allows declaring membership for datasets that may not
 * yet exist in the system.
 *
 * Examples:
 * - Bundle "slovaris" contains "slovaris.countries", "slovaris.currencies", etc.
 * - Bundle "base" contains "iso.countries", "iso.currencies", all FpML datasets
 */
struct dataset_bundle_member final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief Code of the bundle this membership belongs to.
     *
     * References dataset_bundle.code (soft FK).
     */
    std::string bundle_code;

    /**
     * @brief Code of the dataset that is a member of the bundle.
     *
     * References dataset.code (soft FK).
     */
    std::string dataset_code;

    /**
     * @brief Order in which this dataset should be displayed or processed.
     *
     * Lower numbers appear first. Used for controlling installation order
     * and UI presentation.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this membership.
     */
    std::string recorded_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Username of the account that performed this operation.
     */
    std::string performed_by;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
