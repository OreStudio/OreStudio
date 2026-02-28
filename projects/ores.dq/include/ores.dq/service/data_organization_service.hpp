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
#include "ores.dq/domain/dataset_dependency.hpp"
#include "ores.dq/domain/data_domain.hpp"
#include "ores.dq/domain/subject_area.hpp"
#include "ores.dq/repository/catalog_repository.hpp"
#include "ores.dq/repository/dataset_dependency_repository.hpp"
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
     * @brief Saves a catalog (creates or updates).
     *
     * @param catalog The catalog to save
     */
    void save_catalog(const domain::catalog& catalog);

    /**
     * @brief Saves multiple catalogs (creates or updates).
     *
     * @param catalogs The catalogs to save
     */
    void save_catalogs(const std::vector<domain::catalog>& catalogs);

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
    // Dataset Dependency Management
    // ========================================================================

    /**
     * @brief Lists all dataset dependencies.
     */
    std::vector<domain::dataset_dependency> list_dataset_dependencies();

    /**
     * @brief Lists dataset dependencies for a specific dataset.
     * @param dataset_code The code of the dataset to query dependencies for
     */
    std::vector<domain::dataset_dependency>
    list_dataset_dependencies_by_dataset(const std::string& dataset_code);

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
     * @brief Saves a data domain (creates or updates).
     *
     * @param data_domain The data domain to save
     */
    void save_data_domain(const domain::data_domain& data_domain);

    /**
     * @brief Saves multiple data domains (creates or updates).
     *
     * @param data_domains The data domains to save
     */
    void save_data_domains(const std::vector<domain::data_domain>& data_domains);

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
     * @brief Saves a subject area (creates or updates).
     *
     * @param subject_area The subject area to save
     */
    void save_subject_area(const domain::subject_area& subject_area);

    /**
     * @brief Saves multiple subject areas (creates or updates).
     *
     * @param subject_areas The subject areas to save
     */
    void save_subject_areas(const std::vector<domain::subject_area>& subject_areas);

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
    repository::dataset_dependency_repository dataset_dependency_repo_;
    repository::data_domain_repository data_domain_repo_;
    repository::subject_area_repository subject_area_repo_;
};

}

#endif
