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
#include "ores.dq.core/service/data_organization_service.hpp"
#include <algorithm>
#include <stdexcept>

namespace ores::dq::service {

using namespace ores::logging;

data_organization_service::data_organization_service(context ctx)
    : ctx_(ctx)
    , dataset_dependency_repo_(ctx) {}

// ============================================================================
// Dataset Dependency Management
// ============================================================================

std::vector<domain::dataset_dependency> data_organization_service::list_dataset_dependencies() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all dataset dependencies";
    return dataset_dependency_repo_.read_latest();
}

std::vector<domain::dataset_dependency>
data_organization_service::list_dataset_dependencies_by_dataset(const std::string& dataset_code) {
    BOOST_LOG_SEV(lg(), debug) << "Listing dependencies for dataset: " << dataset_code;
    return dataset_dependency_repo_.read_latest_by_dataset(dataset_code);
}

// ============================================================================
// Subject Area Management
// ============================================================================

std::vector<domain::subject_area> data_organization_service::list_subject_areas() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all subject areas";
    return subject_area_repo_.read_latest(ctx_);
}

std::vector<domain::subject_area>
data_organization_service::list_subject_areas(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing subject areas with pagination: offset=" << offset
                               << ", limit=" << limit;
    return subject_area_repo_.read_latest(ctx_, offset, limit);
}

std::vector<domain::subject_area>
data_organization_service::list_subject_areas_by_domain(const std::string& domain_name) {
    BOOST_LOG_SEV(lg(), debug) << "Listing subject areas for domain: " << domain_name;
    auto areas = subject_area_repo_.read_latest(ctx_);
    std::erase_if(areas, [&](const auto& a) { return a.domain_name != domain_name; });
    return areas;
}

std::uint32_t data_organization_service::get_subject_area_count() {
    return subject_area_repo_.get_total_area_count(ctx_);
}

std::optional<domain::subject_area>
data_organization_service::find_subject_area(const std::string& name,
                                             const std::string& domain_name) {
    BOOST_LOG_SEV(lg(), debug) << "Finding subject area: " << name << " in domain: " << domain_name;
    auto subject_areas = subject_area_repo_.read_latest(ctx_, name, domain_name);
    if (subject_areas.empty()) {
        return std::nullopt;
    }
    return subject_areas.front();
}

void data_organization_service::save_subject_area(const domain::subject_area& subject_area) {
    if (subject_area.name.empty()) {
        throw std::invalid_argument("Subject area name cannot be empty.");
    }
    if (subject_area.domain_name.empty()) {
        throw std::invalid_argument("Subject area domain name cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving subject area: " << subject_area.name
                               << " in domain: " << subject_area.domain_name;
    subject_area_repo_.write(ctx_, subject_area);
    BOOST_LOG_SEV(lg(), info) << "Saved subject area: " << subject_area.name;
}

void data_organization_service::save_subject_areas(
    const std::vector<domain::subject_area>& subject_areas) {
    for (const auto& s : subject_areas) {
        if (s.name.empty()) {
            throw std::invalid_argument("Subject area name cannot be empty.");
        }
        if (s.domain_name.empty()) {
            throw std::invalid_argument("Subject area domain name cannot be empty.");
        }
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << subject_areas.size() << " subject areas";
    subject_area_repo_.write(ctx_, subject_areas);
}

void data_organization_service::remove_subject_area(const std::string& name,
                                                    const std::string& domain_name) {
    BOOST_LOG_SEV(lg(), debug) << "Removing subject area: " << name
                               << " from domain: " << domain_name;
    subject_area_repo_.remove(ctx_, name, domain_name);
    BOOST_LOG_SEV(lg(), info) << "Removed subject area: " << name;
}

std::vector<domain::subject_area>
data_organization_service::get_subject_area_history(const std::string& name,
                                                    const std::string& domain_name) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for subject area: " << name
                               << " in domain: " << domain_name;
    return subject_area_repo_.read_all(ctx_, name, domain_name);
}

}
