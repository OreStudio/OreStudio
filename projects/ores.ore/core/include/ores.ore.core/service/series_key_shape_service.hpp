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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_service.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_ORE_CORE_SERVICE_SERIES_KEY_SHAPE_SERVICE_HPP
#define ORES_ORE_CORE_SERVICE_SERIES_KEY_SHAPE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.ore.api/domain/series_key_shape.hpp"
#include "ores.ore.core/export.hpp"
#include "ores.ore.core/repository/series_key_shape_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::ore::service {

/**
 * @brief Service for managing series key shapes.
 *
 * Provides a higher-level interface for series key shape operations,
 * wrapping the underlying repository.
 */
class ORES_ORE_CORE_EXPORT series_key_shape_service {
private:
    inline static std::string_view logger_name = "ores.ore.service.series_key_shape_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a series_key_shape_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit series_key_shape_service(context ctx);

    /**
     * @brief Lists series key shapes with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of series key shapes for the requested page.
     */
    std::vector<domain::series_key_shape> list_shapes(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active series key shapes.
     *
     * @return Total number of active series key shapes.
     */
    std::uint32_t count_shapes();


    /**
     * @brief Retrieves a single series key shape as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The series key shape at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::series_key_shape> get_shape_at_version(const std::string& series_type,
                                                                 std::uint32_t version);

    /**
     * @brief Retrieves a single series key shape by its primary key.
     *
     * @return The series key shape if found, std::nullopt otherwise.
     */
    std::optional<domain::series_key_shape> get_shape(const std::string& series_type);

    /**
     * @brief Saves a series key shape (creates or updates).
     *
     * @param shape The series key shape to save.
     * @throws std::exception on failure.
     */
    void save_shape(const domain::series_key_shape& shape);

    /**
     * @brief Saves a batch of series key shapes.
     *
     * @param shapes The series key shapes to save.
     * @throws std::exception on failure.
     */
    void save_shapes(const std::vector<domain::series_key_shape>& shapes);

    /**
     * @brief Deletes a series key shape by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_shape(const std::string& series_type);

    /**
     * @brief Deletes series key shapes by their primary keys.
     */
    void delete_shapes(const std::vector<std::string>& series_types);

    /**
     * @brief Retrieves all historical versions of a series key shape.
     */
    std::vector<domain::series_key_shape> get_shape_history(const std::string& series_type);

private:
    context ctx_;
    repository::series_key_shape_repository repo_;
};

}

#endif
