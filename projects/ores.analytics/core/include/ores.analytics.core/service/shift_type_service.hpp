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
#ifndef ORES_ANALYTICS_CORE_SERVICE_SHIFT_TYPE_SERVICE_HPP
#define ORES_ANALYTICS_CORE_SERVICE_SHIFT_TYPE_SERVICE_HPP

#include "ores.analytics.api/domain/shift_type.hpp"
#include "ores.analytics.api/messaging/shift_type_protocol.hpp"
#include "ores.analytics.core/export.hpp"
#include "ores.analytics.core/repository/shift_type_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::analytics::service {

/**
 * @brief Service for managing shift types.
 *
 * Provides a higher-level interface for shift type operations,
 * wrapping the underlying repository.
 */
class ORES_ANALYTICS_CORE_EXPORT shift_type_service {
private:
    inline static std::string_view logger_name = "ores.analytics.service.shift_type_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a shift_type_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit shift_type_service(context ctx);

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
    messaging::list_shift_types_response
    list_shift_types(const messaging::list_shift_types_request& request);
    messaging::get_shift_type_response
    get_shift_type(const messaging::get_shift_type_request& request);
    messaging::get_many_shift_types_response
    get_many_shift_types(const messaging::get_many_shift_types_request& request);
    messaging::put_shift_type_response
    put_shift_type(const messaging::put_shift_type_request& request);
    messaging::put_many_shift_types_response
    put_many_shift_types(const messaging::put_many_shift_types_request& request);
    messaging::delete_shift_type_response
    delete_shift_type(const messaging::delete_shift_type_request& request);
    messaging::delete_many_shift_types_response
    delete_many_shift_types(const messaging::delete_many_shift_types_request& request);
    messaging::list_shift_type_versions_response
    list_shift_type_versions(const messaging::list_shift_type_versions_request& request);
    messaging::get_shift_type_version_response
    get_shift_type_version(const messaging::get_shift_type_version_request& request);
    /**@}*/

    /**
     * @brief Lists shift types with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of shift types for the requested page.
     */
    std::vector<domain::shift_type> list_shift_types(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active shift types.
     *
     * @return Total number of active shift types.
     */
    std::uint32_t count_shift_types();


    /**
     * @brief Retrieves a single shift type as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The shift type at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::shift_type> get_shift_type_at_version(const std::string& code,
                                                                std::uint32_t version);

    /**
     * @brief Retrieves a single shift type by its primary key.
     *
     * @return The shift type if found, std::nullopt otherwise.
     */
    std::optional<domain::shift_type> get_shift_type(const std::string& code);

    /**
     * @brief Retrieves a batch of shift types by primary key.
     */
    std::vector<domain::shift_type> get_shift_types(const std::vector<std::string>& codes);

    /**
     * @brief Saves a shift type (creates or updates).
     *
     * @param shift_type The shift type to save.
     * @throws std::exception on failure.
     */
    void save_shift_type(const domain::shift_type& shift_type);

    /**
     * @brief Saves a batch of shift types.
     *
     * @param shift_types The shift types to save.
     * @throws std::exception on failure.
     */
    void save_shift_types(const std::vector<domain::shift_type>& shift_types);

    /**
     * @brief Deletes a shift type by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_shift_type(const std::string& code);

    /**
     * @brief Deletes shift types by their primary keys.
     */
    void delete_shift_types(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a shift type.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::shift_type> get_shift_type_history(const std::string& code);

private:
    context ctx_;
    repository::shift_type_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::shift_type_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::shift_type& out);
};

}

#endif
