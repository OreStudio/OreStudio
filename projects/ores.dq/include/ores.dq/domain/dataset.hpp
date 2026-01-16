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
    int version = 0;
    boost::uuids::uuid id;
    std::optional<std::string> catalog_name;
    std::string subject_area_name;
    std::string domain_name;
    std::optional<std::string> coding_scheme_code;
    std::string origin_code;
    std::string nature_code;
    std::string treatment_code;
    std::optional<boost::uuids::uuid> methodology_id;
    std::string name;
    std::string description;
    std::string source_system_id;
    std::string business_context;
    std::optional<boost::uuids::uuid> upstream_derivation_id;
    int lineage_depth = 0;
    std::chrono::system_clock::time_point as_of_date;
    std::chrono::system_clock::time_point ingestion_timestamp;
    std::optional<std::string> license_info;
    std::string recorded_by;
    std::string change_commentary;
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
