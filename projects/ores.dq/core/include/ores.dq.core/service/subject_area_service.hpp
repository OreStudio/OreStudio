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
#ifndef ORES_DQ_CORE_SERVICE_SUBJECT_AREA_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_SUBJECT_AREA_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/subject_area.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/subject_area_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing subject areas.
 *
 * Provides a higher-level interface for subject area operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT subject_area_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.subject_area_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a subject_area_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit subject_area_service(context ctx);

    /**
     * @brief Lists subject areas with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of subject areas for the requested page.
     */
    std::vector<domain::subject_area> list_areas(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active subject areas.
     *
     * @return Total number of active subject areas.
     */
    std::uint32_t count_areas();

    /**
     * @brief Retrieves a single subject area as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param name The name of the subject area.
     * @param version The version to fetch.
     * @return The subject area at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::subject_area> get_area_at_version(const std::string& name,
                                                            std::uint32_t version);

    /**
     * @brief Retrieves a single subject area by its name.
     *
     * @param name The name of the subject area.
     * @return The subject area if found, std::nullopt otherwise.
     */
    std::optional<domain::subject_area> get_area(const std::string& name);

    /**
     * @brief Saves a subject area (creates or updates).
     *
     * @param area The subject area to save.
     * @throws std::exception on failure.
     */
    void save_area(const domain::subject_area& area);

    /**
     * @brief Saves a batch of subject areas.
     *
     * @param areas The subject areas to save.
     * @throws std::exception on failure.
     */
    void save_areas(const std::vector<domain::subject_area>& areas);

    /**
     * @brief Deletes a subject area by its name.
     *
     * @param name The name of the subject area to delete.
     * @throws std::exception on failure.
     */
    void delete_area(const std::string& name);

    /**
     * @brief Deletes subject areas by their names.
     */
    void delete_areas(const std::vector<std::string>& names);

    /**
     * @brief Retrieves all historical versions of a subject area.
     */
    std::vector<domain::subject_area> get_area_history(const std::string& name);

private:
    context ctx_;
    repository::subject_area_repository repo_;
};

}

#endif
