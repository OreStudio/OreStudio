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
#include "ores.ore.api/messaging/series_key_shape_protocol.hpp"
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
     * @brief The protocol operations, one method per subject.
     *
     * A method takes the canonical request and answers its response, so the
     * handler that serves the subject decodes, calls and replies without
     * deciding anything. The result a caller reads -- missing, conflicting,
     * denied -- is filled here, where the storage call that decided it is
     * made, rather than being inferred from an exception.
     */
    /**@{*/
    messaging::list_series_key_shapes_response
    list_series_key_shapes(const messaging::list_series_key_shapes_request& request);
    messaging::get_series_key_shape_response
    get_series_key_shape(const messaging::get_series_key_shape_request& request);
    messaging::get_many_series_key_shapes_response
    get_many_series_key_shapes(const messaging::get_many_series_key_shapes_request& request);
    messaging::put_series_key_shape_response
    put_series_key_shape(const messaging::put_series_key_shape_request& request);
    messaging::put_many_series_key_shapes_response
    put_many_series_key_shapes(const messaging::put_many_series_key_shapes_request& request);
    messaging::delete_series_key_shape_response
    delete_series_key_shape(const messaging::delete_series_key_shape_request& request);
    messaging::delete_many_series_key_shapes_response
    delete_many_series_key_shapes(const messaging::delete_many_series_key_shapes_request& request);
    messaging::list_series_key_shape_versions_response list_series_key_shape_versions(
        const messaging::list_series_key_shape_versions_request& request);
    messaging::get_series_key_shape_version_response
    get_series_key_shape_version(const messaging::get_series_key_shape_version_request& request);
    /**@}*/

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
     * @brief Retrieves a batch of series key shapes by primary key.
     */
    std::vector<domain::series_key_shape> get_shapes(const std::vector<std::string>& series_types);

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
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::series_key_shape> get_shape_history(const std::string& series_type);

private:
    context ctx_;
    repository::series_key_shape_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result prepare_change(const messaging::series_key_shape_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::series_key_shape& out);
};

}

#endif
