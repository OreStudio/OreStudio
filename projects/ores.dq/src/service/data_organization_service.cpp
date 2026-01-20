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
    : catalog_repo_(ctx), catalog_dependency_repo_(ctx),
      data_domain_repo_(ctx), subject_area_repo_(ctx) {}

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

void data_organization_service::save_catalog(const domain::catalog& catalog) {
    if (catalog.name.empty()) {
        throw std::invalid_argument("Catalog name cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving catalog: " << catalog.name;
    catalog_repo_.write(catalog);
    BOOST_LOG_SEV(lg(), info) << "Saved catalog: " << catalog.name;
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
// Catalog Dependency Management
// ============================================================================

std::vector<domain::catalog_dependency>
data_organization_service::list_catalog_dependencies() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all catalog dependencies";
    return catalog_dependency_repo_.read_latest();
}

std::vector<domain::catalog_dependency>
data_organization_service::list_catalog_dependencies_by_catalog(
    const std::string& catalog_name) {
    BOOST_LOG_SEV(lg(), debug) << "Listing dependencies for catalog: "
                               << catalog_name;
    return catalog_dependency_repo_.read_latest_by_catalog(catalog_name);
}

// ============================================================================
// Data Domain Management
// ============================================================================

std::vector<domain::data_domain> data_organization_service::list_data_domains() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all data domains";
    return data_domain_repo_.read_latest();
}

std::optional<domain::data_domain>
data_organization_service::find_data_domain(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Finding data domain: " << name;
    auto domains = data_domain_repo_.read_latest(name);
    if (domains.empty()) {
        return std::nullopt;
    }
    return domains.front();
}

void data_organization_service::save_data_domain(
    const domain::data_domain& data_domain) {
    if (data_domain.name.empty()) {
        throw std::invalid_argument("Data domain name cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving data domain: " << data_domain.name;
    data_domain_repo_.write(data_domain);
    BOOST_LOG_SEV(lg(), info) << "Saved data domain: " << data_domain.name;
}

void data_organization_service::remove_data_domain(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Removing data domain: " << name;
    data_domain_repo_.remove(name);
    BOOST_LOG_SEV(lg(), info) << "Removed data domain: " << name;
}

std::vector<domain::data_domain>
data_organization_service::get_data_domain_history(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for data domain: " << name;
    return data_domain_repo_.read_all(name);
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

void data_organization_service::save_subject_area(
    const domain::subject_area& subject_area) {
    if (subject_area.name.empty()) {
        throw std::invalid_argument("Subject area name cannot be empty.");
    }
    if (subject_area.domain_name.empty()) {
        throw std::invalid_argument("Subject area domain name cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving subject area: " << subject_area.name
                               << " in domain: " << subject_area.domain_name;
    subject_area_repo_.write(subject_area);
    BOOST_LOG_SEV(lg(), info) << "Saved subject area: " << subject_area.name;
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
