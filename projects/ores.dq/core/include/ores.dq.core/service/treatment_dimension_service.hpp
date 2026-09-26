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
#ifndef ORES_DQ_CORE_SERVICE_TREATMENT_DIMENSION_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_TREATMENT_DIMENSION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/treatment_dimension.hpp"
#include "ores.dq.api/messaging/treatment_dimension_protocol.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/treatment_dimension_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing treatment dimensions.
 *
 * Provides a higher-level interface for treatment dimension operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT treatment_dimension_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.treatment_dimension_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a treatment_dimension_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit treatment_dimension_service(context ctx);

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
    messaging::list_treatment_dimensions_response
    list_treatment_dimensions(const messaging::list_treatment_dimensions_request& request);
    messaging::get_treatment_dimension_response
    get_treatment_dimension(const messaging::get_treatment_dimension_request& request);
    messaging::get_many_treatment_dimensions_response
    get_many_treatment_dimensions(const messaging::get_many_treatment_dimensions_request& request);
    messaging::put_treatment_dimension_response
    put_treatment_dimension(const messaging::put_treatment_dimension_request& request);
    messaging::put_many_treatment_dimensions_response
    put_many_treatment_dimensions(const messaging::put_many_treatment_dimensions_request& request);
    messaging::delete_treatment_dimension_response
    delete_treatment_dimension(const messaging::delete_treatment_dimension_request& request);
    messaging::delete_many_treatment_dimensions_response delete_many_treatment_dimensions(
        const messaging::delete_many_treatment_dimensions_request& request);
    messaging::list_treatment_dimension_versions_response list_treatment_dimension_versions(
        const messaging::list_treatment_dimension_versions_request& request);
    messaging::get_treatment_dimension_version_response get_treatment_dimension_version(
        const messaging::get_treatment_dimension_version_request& request);
    /**@}*/

    /**
     * @brief Lists treatment dimensions with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of treatment dimensions for the requested page.
     */
    std::vector<domain::treatment_dimension> list_dimensions(std::uint32_t offset,
                                                             std::uint32_t limit);

    /**
     * @brief Gets the total count of active treatment dimensions.
     *
     * @return Total number of active treatment dimensions.
     */
    std::uint32_t count_dimensions();


    /**
     * @brief Retrieves a single treatment dimension as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The treatment dimension at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::treatment_dimension> get_dimension_at_version(const std::string& code,
                                                                        std::uint32_t version);

    /**
     * @brief Retrieves a single treatment dimension by its primary key.
     *
     * @return The treatment dimension if found, std::nullopt otherwise.
     */
    std::optional<domain::treatment_dimension> get_dimension(const std::string& code);

    /**
     * @brief Retrieves a batch of treatment dimensions by primary key.
     */
    std::vector<domain::treatment_dimension> get_dimensions(const std::vector<std::string>& codes);

    /**
     * @brief Saves a treatment dimension (creates or updates).
     *
     * @param dimension The treatment dimension to save.
     * @throws std::exception on failure.
     */
    void save_dimension(const domain::treatment_dimension& dimension);

    /**
     * @brief Saves a batch of treatment dimensions.
     *
     * @param dimensions The treatment dimensions to save.
     * @throws std::exception on failure.
     */
    void save_dimensions(const std::vector<domain::treatment_dimension>& dimensions);

    /**
     * @brief Deletes a treatment dimension by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_dimension(const std::string& code);

    /**
     * @brief Deletes treatment dimensions by their primary keys.
     */
    void delete_dimensions(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a treatment dimension.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::treatment_dimension> get_dimension_history(const std::string& code);

private:
    context ctx_;
    repository::treatment_dimension_repository repo_;

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
    ores::utility::domain::result
    prepare_change(const messaging::treatment_dimension_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::treatment_dimension& out);
};

}

#endif
