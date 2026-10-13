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
#ifndef ORES_REPORTING_CORE_SERVICE_FILE_TYPE_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_FILE_TYPE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/file_type.hpp"
#include "ores.reporting.api/messaging/file_type_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/file_type_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing file types.
 *
 * Provides a higher-level interface for file type operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT file_type_service {
private:
    inline static std::string_view logger_name = "ores.reporting.service.file_type_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a file_type_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit file_type_service(context ctx);

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
    messaging::list_file_types_response
    list_file_types(const messaging::list_file_types_request& request);
    messaging::get_file_type_response
    get_file_type(const messaging::get_file_type_request& request);
    messaging::get_many_file_types_response
    get_many_file_types(const messaging::get_many_file_types_request& request);
    messaging::put_file_type_response
    put_file_type(const messaging::put_file_type_request& request);
    messaging::put_many_file_types_response
    put_many_file_types(const messaging::put_many_file_types_request& request);
    messaging::delete_file_type_response
    delete_file_type(const messaging::delete_file_type_request& request);
    messaging::delete_many_file_types_response
    delete_many_file_types(const messaging::delete_many_file_types_request& request);
    messaging::list_by_analytic_type_code_file_types_response list_by_analytic_type_code_file_types(
        const messaging::list_by_analytic_type_code_file_types_request& request);
    messaging::list_by_configuration_type_code_file_types_response
    list_by_configuration_type_code_file_types(
        const messaging::list_by_configuration_type_code_file_types_request& request);
    messaging::list_by_produced_by_file_type_code_file_types_response
    list_by_produced_by_file_type_code_file_types(
        const messaging::list_by_produced_by_file_type_code_file_types_request& request);
    messaging::list_file_type_versions_response
    list_file_type_versions(const messaging::list_file_type_versions_request& request);
    messaging::get_file_type_version_response
    get_file_type_version(const messaging::get_file_type_version_request& request);
    /**@}*/

    /**
     * @brief Lists file types with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of file types for the requested page.
     */
    std::vector<domain::file_type> list_types(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active file types.
     *
     * @return Total number of active file types.
     */
    std::uint32_t count_types();


    /**
     * @brief Lists file types filtered by analytic_type_code, with pagination.
     *
     * @param analytic_type_code The analytic_type_code to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching file types for the requested page.
     */
    std::vector<domain::file_type> list_types_by_analytic_type_code(
        const std::string& analytic_type_code, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active file types filtered by analytic_type_code.
     *
     * @param analytic_type_code The analytic_type_code to filter by.
     * @return Total number of matching file types.
     */
    std::uint32_t count_types_by_analytic_type_code(const std::string& analytic_type_code);


    /**
     * @brief Lists file types filtered by configuration_type_code, with pagination.
     *
     * @param configuration_type_code The configuration_type_code to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching file types for the requested page.
     */
    std::vector<domain::file_type> list_types_by_configuration_type_code(
        const std::string& configuration_type_code, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active file types filtered by configuration_type_code.
     *
     * @param configuration_type_code The configuration_type_code to filter by.
     * @return Total number of matching file types.
     */
    std::uint32_t
    count_types_by_configuration_type_code(const std::string& configuration_type_code);


    /**
     * @brief Lists file types filtered by produced_by_file_type_code, with pagination.
     *
     * @param produced_by_file_type_code The produced_by_file_type_code to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching file types for the requested page.
     */
    std::vector<domain::file_type> list_types_by_produced_by_file_type_code(
        const std::string& produced_by_file_type_code, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active file types filtered by produced_by_file_type_code.
     *
     * @param produced_by_file_type_code The produced_by_file_type_code to filter by.
     * @return Total number of matching file types.
     */
    std::uint32_t
    count_types_by_produced_by_file_type_code(const std::string& produced_by_file_type_code);


    /**
     * @brief Retrieves a single file type as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The file type at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::file_type> get_type_at_version(const std::string& code,
                                                         std::uint32_t version);

    /**
     * @brief Retrieves a single file type by its primary key.
     *
     * @return The file type if found, std::nullopt otherwise.
     */
    std::optional<domain::file_type> get_type(const std::string& code);

    /**
     * @brief Retrieves a batch of file types by primary key.
     */
    std::vector<domain::file_type> get_types(const std::vector<std::string>& codes);

    /**
     * @brief Saves a file type (creates or updates).
     *
     * @param type The file type to save.
     * @throws std::exception on failure.
     */
    void save_type(const domain::file_type& type);

    /**
     * @brief Saves a batch of file types.
     *
     * @param types The file types to save.
     * @throws std::exception on failure.
     */
    void save_types(const std::vector<domain::file_type>& types);

    /**
     * @brief Deletes a file type by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_type(const std::string& code);

    /**
     * @brief Deletes file types by their primary keys.
     */
    void delete_types(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a file type.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::file_type> get_type_history(const std::string& code);

private:
    context ctx_;
    repository::file_type_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::file_type_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::file_type& out);
};

}

#endif
