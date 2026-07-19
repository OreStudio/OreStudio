/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_DQ_CORE_SERVICE_CATALOG_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_CATALOG_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/catalog.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/catalog_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing catalogs.
 *
 * Provides a higher-level interface for catalog operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT catalog_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.catalog_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a catalog_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit catalog_service(context ctx);

    /**
     * @brief Lists catalogs with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of catalogs for the requested page.
     */
    std::vector<domain::catalog> list_catalogs(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active catalogs.
     *
     * @return Total number of active catalogs.
     */
    std::uint32_t count_catalogs();

    /**
     * @brief Retrieves a single catalog as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param name The name of the catalog.
     * @param version The version to fetch.
     * @return The catalog at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::catalog> get_catalog_at_version(const std::string& name,
                                                          std::uint32_t version);

    /**
     * @brief Retrieves a single catalog by its name.
     *
     * @param name The name of the catalog.
     * @return The catalog if found, std::nullopt otherwise.
     */
    std::optional<domain::catalog> get_catalog(const std::string& name);

    /**
     * @brief Saves a catalog (creates or updates).
     *
     * @param catalog The catalog to save.
     * @throws std::exception on failure.
     */
    void save_catalog(const domain::catalog& catalog);

    /**
     * @brief Saves a batch of catalogs.
     *
     * @param catalogs The catalogs to save.
     * @throws std::exception on failure.
     */
    void save_catalogs(const std::vector<domain::catalog>& catalogs);

    /**
     * @brief Deletes a catalog by its name.
     *
     * @param name The name of the catalog to delete.
     * @throws std::exception on failure.
     */
    void delete_catalog(const std::string& name);

    /**
     * @brief Deletes catalogs by their names.
     */
    void delete_catalogs(const std::vector<std::string>& names);

    /**
     * @brief Retrieves all historical versions of a catalog.
     */
    std::vector<domain::catalog> get_catalog_history(const std::string& name);

private:
    context ctx_;
    repository::catalog_repository repo_;
};

}

#endif
