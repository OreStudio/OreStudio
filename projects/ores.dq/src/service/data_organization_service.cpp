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
#include "ores.dq/service/data_organization_service.hpp"

#include <stdexcept>

namespace ores::dq::service {

using namespace ores::logging;

data_organization_service::data_organization_service(context ctx)
    : catalog_repo_(ctx), subject_area_repo_(ctx) {}

// ============================================================================
// Catalog Management
// ============================================================================

std::vector<domain::catalog> data_organization_service::list_catalogs() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all catalogs";
    return catalog_repo_.read_latest();
}

std::vector<domain::catalog>
data_organization_service::list_catalogs(std::uint32_t offset,
                                          std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing catalogs with pagination: offset="
                               << offset << ", limit=" << limit;
    return catalog_repo_.read_latest(offset, limit);
}

std::uint32_t data_organization_service::get_catalog_count() {
    return catalog_repo_.get_total_count();
}

std::optional<domain::catalog>
data_organization_service::find_catalog(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Finding catalog: " << name;
    auto catalogs = catalog_repo_.read_latest(name);
    if (catalogs.empty()) {
        return std::nullopt;
    }
    return catalogs.front();
}

domain::catalog
data_organization_service::create_catalog(const domain::catalog& catalog) {
    BOOST_LOG_SEV(lg(), debug) << "Creating catalog: " << catalog.name;

    if (catalog.name.empty()) {
        throw std::invalid_argument("Catalog name cannot be empty");
    }

    auto existing = find_catalog(catalog.name);
    if (existing) {
        throw std::runtime_error("Catalog already exists: " + catalog.name);
    }

    catalog_repo_.write(catalog);
    BOOST_LOG_SEV(lg(), info) << "Created catalog: " << catalog.name;

    auto created = find_catalog(catalog.name);
    if (!created) {
        throw std::runtime_error("Failed to retrieve created catalog");
    }
    return *created;
}

void data_organization_service::update_catalog(const domain::catalog& catalog) {
    BOOST_LOG_SEV(lg(), debug) << "Updating catalog: " << catalog.name;

    if (catalog.name.empty()) {
        throw std::invalid_argument("Catalog name cannot be empty");
    }

    catalog_repo_.write(catalog);
    BOOST_LOG_SEV(lg(), info) << "Updated catalog: " << catalog.name;
}

void data_organization_service::remove_catalog(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Removing catalog: " << name;
    catalog_repo_.remove(name);
    BOOST_LOG_SEV(lg(), info) << "Removed catalog: " << name;
}

std::vector<domain::catalog>
data_organization_service::get_catalog_history(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for catalog: " << name;
    return catalog_repo_.read_all(name);
}

// ============================================================================
// Subject Area Management
// ============================================================================

std::vector<domain::subject_area>
data_organization_service::list_subject_areas() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all subject areas";
    return subject_area_repo_.read_latest();
}

std::vector<domain::subject_area>
data_organization_service::list_subject_areas(std::uint32_t offset,
                                               std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing subject areas with pagination: offset="
                               << offset << ", limit=" << limit;
    return subject_area_repo_.read_latest(offset, limit);
}

std::vector<domain::subject_area>
data_organization_service::list_subject_areas_by_domain(
    const std::string& domain_name) {
    BOOST_LOG_SEV(lg(), debug) << "Listing subject areas for domain: "
                               << domain_name;
    return subject_area_repo_.read_latest_by_domain(domain_name);
}

std::uint32_t data_organization_service::get_subject_area_count() {
    return subject_area_repo_.get_total_count();
}

std::optional<domain::subject_area>
data_organization_service::find_subject_area(const std::string& name,
                                              const std::string& domain_name) {
    BOOST_LOG_SEV(lg(), debug) << "Finding subject area: " << name
                               << " in domain: " << domain_name;
    auto subject_areas = subject_area_repo_.read_latest(name, domain_name);
    if (subject_areas.empty()) {
        return std::nullopt;
    }
    return subject_areas.front();
}

domain::subject_area
data_organization_service::create_subject_area(
    const domain::subject_area& subject_area) {
    BOOST_LOG_SEV(lg(), debug) << "Creating subject area: " << subject_area.name
                               << " in domain: " << subject_area.domain_name;

    if (subject_area.name.empty()) {
        throw std::invalid_argument("Subject area name cannot be empty");
    }
    if (subject_area.domain_name.empty()) {
        throw std::invalid_argument("Domain name cannot be empty");
    }

    auto existing = find_subject_area(subject_area.name,
                                      subject_area.domain_name);
    if (existing) {
        throw std::runtime_error("Subject area already exists: " +
                                 subject_area.name + " in domain " +
                                 subject_area.domain_name);
    }

    subject_area_repo_.write(subject_area);
    BOOST_LOG_SEV(lg(), info) << "Created subject area: " << subject_area.name;

    auto created = find_subject_area(subject_area.name, subject_area.domain_name);
    if (!created) {
        throw std::runtime_error("Failed to retrieve created subject area");
    }
    return *created;
}

void data_organization_service::update_subject_area(
    const domain::subject_area& subject_area) {
    BOOST_LOG_SEV(lg(), debug) << "Updating subject area: " << subject_area.name
                               << " in domain: " << subject_area.domain_name;

    if (subject_area.name.empty()) {
        throw std::invalid_argument("Subject area name cannot be empty");
    }
    if (subject_area.domain_name.empty()) {
        throw std::invalid_argument("Domain name cannot be empty");
    }

    subject_area_repo_.write(subject_area);
    BOOST_LOG_SEV(lg(), info) << "Updated subject area: " << subject_area.name;
}

void data_organization_service::remove_subject_area(
    const std::string& name, const std::string& domain_name) {
    BOOST_LOG_SEV(lg(), debug) << "Removing subject area: " << name
                               << " from domain: " << domain_name;
    subject_area_repo_.remove(name, domain_name);
    BOOST_LOG_SEV(lg(), info) << "Removed subject area: " << name;
}

std::vector<domain::subject_area>
data_organization_service::get_subject_area_history(
    const std::string& name, const std::string& domain_name) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for subject area: " << name
                               << " in domain: " << domain_name;
    return subject_area_repo_.read_all(name, domain_name);
}

}
