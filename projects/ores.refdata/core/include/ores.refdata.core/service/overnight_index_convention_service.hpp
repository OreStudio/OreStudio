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
#ifndef ORES_REFDATA_CORE_SERVICE_OVERNIGHT_INDEX_CONVENTION_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_OVERNIGHT_INDEX_CONVENTION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/overnight_index_convention.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/overnight_index_convention_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing overnight index conventions.
 *
 * Provides a higher-level interface for overnight index convention operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT overnight_index_convention_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.overnight_index_convention_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a overnight_index_convention_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit overnight_index_convention_service(context ctx);

    /**
     * @brief Lists overnight index conventions with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of overnight index conventions for the requested page.
     */
    std::vector<domain::overnight_index_convention>
    list_overnight_index_conventions(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active overnight index conventions.
     *
     * @return Total number of active overnight index conventions.
     */
    std::uint32_t count_overnight_index_conventions();


    /**
     * @brief Retrieves a single overnight index convention as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The overnight index convention at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::overnight_index_convention>
    get_overnight_index_convention_at_version(const std::string& id, std::uint32_t version);

    /**
     * @brief Retrieves a single overnight index convention by its primary key.
     *
     * @return The overnight index convention if found, std::nullopt otherwise.
     */
    std::optional<domain::overnight_index_convention>
    get_overnight_index_convention(const std::string& id);

    /**
     * @brief Saves a overnight index convention (creates or updates).
     *
     * @param overnight_index_convention The overnight index convention to save.
     * @throws std::exception on failure.
     */
    void save_overnight_index_convention(
        const domain::overnight_index_convention& overnight_index_convention);

    /**
     * @brief Saves a batch of overnight index conventions.
     *
     * @param overnight_index_conventions The overnight index conventions to save.
     * @throws std::exception on failure.
     */
    void save_overnight_index_conventions(
        const std::vector<domain::overnight_index_convention>& overnight_index_conventions);

    /**
     * @brief Deletes a overnight index convention by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_overnight_index_convention(const std::string& id);

    /**
     * @brief Deletes overnight index conventions by their primary keys.
     */
    void delete_overnight_index_conventions(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a overnight index convention.
     */
    std::vector<domain::overnight_index_convention>
    get_overnight_index_convention_history(const std::string& id);

private:
    context ctx_;
    repository::overnight_index_convention_repository repo_;
};

}

#endif
