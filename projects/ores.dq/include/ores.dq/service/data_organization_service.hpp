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
#ifndef ORES_DQ_SERVICE_DATA_ORGANIZATION_SERVICE_HPP
#define ORES_DQ_SERVICE_DATA_ORGANIZATION_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.dq/domain/catalog.hpp"
#include "ores.dq/domain/data_domain.hpp"
#include "ores.dq/domain/subject_area.hpp"
#include "ores.dq/repository/catalog_repository.hpp"
#include "ores.dq/repository/data_domain_repository.hpp"
#include "ores.dq/repository/subject_area_repository.hpp"

namespace ores::dq::service {

/**
 * @brief Service for managing data organization entities.
 *
 * This service provides functionality for:
 * - Managing catalogs (CRUD operations)
 * - Managing subject areas and their associated domains
 */
class data_organization_service {
private:
    inline static std::string_view logger_name =
        "ores.dq.service.data_organization_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a data_organization_service with required repositories.
     *
     * @param ctx The database context.
     */
    explicit data_organization_service(context ctx);

    // ========================================================================
    // Catalog Management
    // ========================================================================

    /**
     * @brief Lists all catalogs.
     */
    std::vector<domain::catalog> list_catalogs();

    /**
     * @brief Lists catalogs with pagination.
     */
    std::vector<domain::catalog>
    list_catalogs(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active catalogs.
     */
    std::uint32_t get_catalog_count();

    /**
     * @brief Finds a catalog by its name.
     */
    std::optional<domain::catalog> find_catalog(const std::string& name);

    /**
     * @brief Creates a new catalog.
     *
     * @param catalog The catalog to create
     * @return The created catalog
     * @throws std::invalid_argument if name is empty
     * @throws std::runtime_error if catalog already exists
     */
    domain::catalog create_catalog(const domain::catalog& catalog);

    /**
     * @brief Updates an existing catalog.
     *
     * @param catalog The catalog with updated values
     */
    void update_catalog(const domain::catalog& catalog);

    /**
     * @brief Removes a catalog.
     *
     * @param name The name of the catalog to remove
     */
    void remove_catalog(const std::string& name);

    /**
     * @brief Gets the version history for a catalog.
     *
     * @param name The catalog name
     * @return Vector of all versions, newest first
     */
    std::vector<domain::catalog> get_catalog_history(const std::string& name);

    // ========================================================================
    // Data Domain Management
    // ========================================================================

    /**
     * @brief Lists all data domains.
     */
    std::vector<domain::data_domain> list_data_domains();

    /**
     * @brief Finds a data domain by its name.
     */
    std::optional<domain::data_domain> find_data_domain(const std::string& name);

    /**
     * @brief Creates a new data domain.
     *
     * @param data_domain The data domain to create
     * @return The created data domain
     * @throws std::invalid_argument if name is empty
     * @throws std::runtime_error if data domain already exists
     */
    domain::data_domain create_data_domain(const domain::data_domain& data_domain);

    /**
     * @brief Updates an existing data domain.
     *
     * @param data_domain The data domain with updated values
     */
    void update_data_domain(const domain::data_domain& data_domain);

    /**
     * @brief Removes a data domain.
     *
     * @param name The name of the data domain to remove
     */
    void remove_data_domain(const std::string& name);

    /**
     * @brief Gets the version history for a data domain.
     *
     * @param name The data domain name
     * @return Vector of all versions, newest first
     */
    std::vector<domain::data_domain> get_data_domain_history(const std::string& name);

    // ========================================================================
    // Subject Area Management
    // ========================================================================

    /**
     * @brief Lists all subject areas.
     */
    std::vector<domain::subject_area> list_subject_areas();

    /**
     * @brief Lists subject areas with pagination.
     */
    std::vector<domain::subject_area>
    list_subject_areas(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Lists subject areas for a specific domain.
     */
    std::vector<domain::subject_area>
    list_subject_areas_by_domain(const std::string& domain_name);

    /**
     * @brief Gets the total count of active subject areas.
     */
    std::uint32_t get_subject_area_count();

    /**
     * @brief Finds a subject area by its composite key.
     */
    std::optional<domain::subject_area>
    find_subject_area(const std::string& name, const std::string& domain_name);

    /**
     * @brief Creates a new subject area.
     *
     * @param subject_area The subject area to create
     * @return The created subject area
     * @throws std::invalid_argument if name or domain_name is empty
     * @throws std::runtime_error if subject area already exists
     */
    domain::subject_area create_subject_area(
        const domain::subject_area& subject_area);

    /**
     * @brief Updates an existing subject area.
     *
     * @param subject_area The subject area with updated values
     */
    void update_subject_area(const domain::subject_area& subject_area);

    /**
     * @brief Removes a subject area.
     *
     * @param name The name of the subject area to remove
     * @param domain_name The domain name of the subject area
     */
    void remove_subject_area(const std::string& name,
                             const std::string& domain_name);

    /**
     * @brief Gets the version history for a subject area.
     *
     * @param name The subject area name
     * @param domain_name The domain name
     * @return Vector of all versions, newest first
     */
    std::vector<domain::subject_area>
    get_subject_area_history(const std::string& name,
                             const std::string& domain_name);

private:
    repository::catalog_repository catalog_repo_;
    repository::data_domain_repository data_domain_repo_;
    repository::subject_area_repository subject_area_repo_;
};

}

#endif
