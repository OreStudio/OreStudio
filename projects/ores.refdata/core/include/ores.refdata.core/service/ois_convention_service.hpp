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
#ifndef ORES_REFDATA_CORE_SERVICE_OIS_CONVENTION_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_OIS_CONVENTION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/ois_convention.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/ois_convention_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing OIS conventions.
 *
 * Provides a higher-level interface for OIS convention operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT ois_convention_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.ois_convention_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a ois_convention_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit ois_convention_service(context ctx);

    /**
     * @brief Lists OIS conventions with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of OIS conventions for the requested page.
     */
    std::vector<domain::ois_convention> list_ois_conventions(std::uint32_t offset,
                                                             std::uint32_t limit);

    /**
     * @brief Gets the total count of active OIS conventions.
     *
     * @return Total number of active OIS conventions.
     */
    std::uint32_t count_ois_conventions();


    /**
     * @brief Retrieves a single OIS convention as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The OIS convention at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::ois_convention> get_ois_convention_at_version(const std::string& id,
                                                                        std::uint32_t version);

    /**
     * @brief Retrieves a single OIS convention by its primary key.
     *
     * @return The OIS convention if found, std::nullopt otherwise.
     */
    std::optional<domain::ois_convention> get_ois_convention(const std::string& id);

    /**
     * @brief Saves a OIS convention (creates or updates).
     *
     * @param ois_convention The OIS convention to save.
     * @throws std::exception on failure.
     */
    void save_ois_convention(const domain::ois_convention& ois_convention);

    /**
     * @brief Saves a batch of OIS conventions.
     *
     * @param ois_conventions The OIS conventions to save.
     * @throws std::exception on failure.
     */
    void save_ois_conventions(const std::vector<domain::ois_convention>& ois_conventions);

    /**
     * @brief Deletes a OIS convention by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_ois_convention(const std::string& id);

    /**
     * @brief Deletes OIS conventions by their primary keys.
     */
    void delete_ois_conventions(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a OIS convention.
     */
    std::vector<domain::ois_convention> get_ois_convention_history(const std::string& id);

private:
    context ctx_;
    repository::ois_convention_repository repo_;
};

}

#endif
