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
#ifndef ORES_REPORTING_CORE_SERVICE_ORE_FILE_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_ORE_FILE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/ore_file.hpp"
#include "ores.reporting.api/messaging/ore_file_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/ore_file_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing ORE files.
 *
 * Provides a higher-level interface for ORE file operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT ore_file_service {
private:
    inline static std::string_view logger_name = "ores.reporting.service.ore_file_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a ore_file_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit ore_file_service(context ctx);

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
    ::ores::reporting::messaging::list_ore_files_response
    list_ore_files(const ::ores::reporting::messaging::list_ore_files_request& request);
    ::ores::reporting::messaging::get_ore_file_response
    get_ore_file(const ::ores::reporting::messaging::get_ore_file_request& request);
    ::ores::reporting::messaging::get_many_ore_files_response
    get_many_ore_files(const ::ores::reporting::messaging::get_many_ore_files_request& request);
    ::ores::reporting::messaging::put_ore_file_response
    put_ore_file(const ::ores::reporting::messaging::put_ore_file_request& request);
    ::ores::reporting::messaging::put_many_ore_files_response
    put_many_ore_files(const ::ores::reporting::messaging::put_many_ore_files_request& request);
    ::ores::reporting::messaging::delete_ore_file_response
    delete_ore_file(const ::ores::reporting::messaging::delete_ore_file_request& request);
    ::ores::reporting::messaging::delete_many_ore_files_response delete_many_ore_files(
        const ::ores::reporting::messaging::delete_many_ore_files_request& request);
    ::ores::reporting::messaging::list_by_category_code_ore_files_response
    list_by_category_code_ore_files(
        const ::ores::reporting::messaging::list_by_category_code_ore_files_request& request);
    ::ores::reporting::messaging::list_by_configuration_type_code_ore_files_response
    list_by_configuration_type_code_ore_files(
        const ::ores::reporting::messaging::list_by_configuration_type_code_ore_files_request&
            request);
    ::ores::reporting::messaging::list_by_schema_ore_file_code_ore_files_response
    list_by_schema_ore_file_code_ore_files(
        const ::ores::reporting::messaging::list_by_schema_ore_file_code_ore_files_request&
            request);
    ::ores::reporting::messaging::list_ore_files_versions_response list_ore_files_versions(
        const ::ores::reporting::messaging::list_ore_files_versions_request& request);
    ::ores::reporting::messaging::get_ore_files_version_response get_ore_files_version(
        const ::ores::reporting::messaging::get_ore_files_version_request& request);
    /**@}*/

    /**
     * @brief Lists ORE files with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of ORE files for the requested page.
     */
    std::vector<domain::ore_file> list_files(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active ORE files.
     *
     * @return Total number of active ORE files.
     */
    std::uint32_t count_files();


    /**
     * @brief Lists ORE files filtered by category_code, with pagination.
     *
     * @param category_code The category_code to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching ORE files for the requested page.
     */
    std::vector<domain::ore_file> list_files_by_category_code(const std::string& category_code,
                                                              std::uint32_t offset,
                                                              std::uint32_t limit);

    /**
     * @brief Gets the total count of active ORE files filtered by category_code.
     *
     * @param category_code The category_code to filter by.
     * @return Total number of matching ORE files.
     */
    std::uint32_t count_files_by_category_code(const std::string& category_code);


    /**
     * @brief Lists ORE files filtered by configuration_type_code, with pagination.
     *
     * @param configuration_type_code The configuration_type_code to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching ORE files for the requested page.
     */
    std::vector<domain::ore_file> list_files_by_configuration_type_code(
        const std::string& configuration_type_code, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active ORE files filtered by configuration_type_code.
     *
     * @param configuration_type_code The configuration_type_code to filter by.
     * @return Total number of matching ORE files.
     */
    std::uint32_t
    count_files_by_configuration_type_code(const std::string& configuration_type_code);


    /**
     * @brief Lists ORE files filtered by schema_ore_file_code, with pagination.
     *
     * @param schema_ore_file_code The schema_ore_file_code to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching ORE files for the requested page.
     */
    std::vector<domain::ore_file> list_files_by_schema_ore_file_code(
        const std::string& schema_ore_file_code, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active ORE files filtered by schema_ore_file_code.
     *
     * @param schema_ore_file_code The schema_ore_file_code to filter by.
     * @return Total number of matching ORE files.
     */
    std::uint32_t count_files_by_schema_ore_file_code(const std::string& schema_ore_file_code);


    /**
     * @brief Retrieves a single ORE file as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The ORE file at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::ore_file> get_file_at_version(const std::string& code,
                                                        std::uint32_t version);

    /**
     * @brief Retrieves a single ORE file by its primary key.
     *
     * @return The ORE file if found, std::nullopt otherwise.
     */
    std::optional<domain::ore_file> get_file(const std::string& code);

    /**
     * @brief Retrieves a batch of ORE files by primary key.
     */
    std::vector<domain::ore_file> get_files(const std::vector<std::string>& codes);

    /**
     * @brief Saves a ORE file (creates or updates).
     *
     * @param file The ORE file to save.
     * @throws std::exception on failure.
     */
    void save_file(const domain::ore_file& file);

    /**
     * @brief Saves a batch of ORE files.
     *
     * @param files The ORE files to save.
     * @throws std::exception on failure.
     */
    void save_files(const std::vector<domain::ore_file>& files);

    /**
     * @brief Deletes a ORE file by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_file(const std::string& code);

    /**
     * @brief Deletes ORE files by their primary keys.
     */
    void delete_files(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a ORE file.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::ore_file> get_file_history(const std::string& code);

private:
    context ctx_;
    repository::ore_file_repository repo_;

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
    prepare_change(const ::ores::reporting::messaging::ore_file_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::ore_file& out);
};

}

#endif
