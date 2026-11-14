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
#ifndef ORES_REPORTING_CORE_SERVICE_ORE_FILE_VERSION_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_ORE_FILE_VERSION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/ore_file_version.hpp"
#include "ores.reporting.api/messaging/ore_file_version_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/ore_file_version_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing ORE file versions.
 *
 * Provides a higher-level interface for ORE file version operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT ore_file_version_service {
private:
    inline static std::string_view logger_name = "ores.reporting.service.ore_file_version_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a ore_file_version_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit ore_file_version_service(const context& ctx);

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
    ::ores::reporting::messaging::list_ore_file_versions_response list_ore_file_versions(
        const ::ores::reporting::messaging::list_ore_file_versions_request& request);
    ::ores::reporting::messaging::get_ore_file_version_response
    get_ore_file_version(const ::ores::reporting::messaging::get_ore_file_version_request& request);
    ::ores::reporting::messaging::get_many_ore_file_versions_response get_many_ore_file_versions(
        const ::ores::reporting::messaging::get_many_ore_file_versions_request& request);
    ::ores::reporting::messaging::put_ore_file_version_response
    put_ore_file_version(const ::ores::reporting::messaging::put_ore_file_version_request& request);
    ::ores::reporting::messaging::put_many_ore_file_versions_response put_many_ore_file_versions(
        const ::ores::reporting::messaging::put_many_ore_file_versions_request& request);
    ::ores::reporting::messaging::delete_ore_file_version_response delete_ore_file_version(
        const ::ores::reporting::messaging::delete_ore_file_version_request& request);
    ::ores::reporting::messaging::delete_many_ore_file_versions_response
    delete_many_ore_file_versions(
        const ::ores::reporting::messaging::delete_many_ore_file_versions_request& request);
    ::ores::reporting::messaging::list_by_ore_file_code_ore_file_versions_response
    list_by_ore_file_code_ore_file_versions(
        const ::ores::reporting::messaging::list_by_ore_file_code_ore_file_versions_request&
            request);
    ::ores::reporting::messaging::list_ore_file_version_versions_response
    list_ore_file_version_versions(
        const ::ores::reporting::messaging::list_ore_file_version_versions_request& request);
    ::ores::reporting::messaging::get_ore_file_version_version_response
    get_ore_file_version_version(
        const ::ores::reporting::messaging::get_ore_file_version_version_request& request);
    /**@}*/

    /**
     * @brief Lists ORE file versions with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of ORE file versions for the requested page.
     */
    std::vector<domain::ore_file_version> list_versions(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active ORE file versions.
     *
     * @return Total number of active ORE file versions.
     */
    std::uint32_t count_versions();


    /**
     * @brief Lists ORE file versions filtered by ore_file_code, with pagination.
     *
     * @param ore_file_code The ore_file_code to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching ORE file versions for the requested page.
     */
    std::vector<domain::ore_file_version> list_versions_by_ore_file_code(
        const std::string& ore_file_code, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active ORE file versions filtered by ore_file_code.
     *
     * @param ore_file_code The ore_file_code to filter by.
     * @return Total number of matching ORE file versions.
     */
    std::uint32_t count_versions_by_ore_file_code(const std::string& ore_file_code);


    /**
     * @brief Retrieves a single ORE file version as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The ORE file version at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::ore_file_version> get_version_at_version(const boost::uuids::uuid& id,
                                                                   std::uint32_t version);

    /**
     * @brief Retrieves a single ORE file version by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The ORE file version if found, std::nullopt otherwise.
     */
    std::optional<domain::ore_file_version> get_version(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of ORE file versions by primary key.
     */
    std::vector<domain::ore_file_version> get_versions(const std::vector<std::string>& ids);

    /**
     * @brief Saves a ORE file version (creates or updates).
     *
     * @param version The ORE file version to save.
     * @throws std::exception on failure.
     */
    void save_version(const domain::ore_file_version& version);

    /**
     * @brief Saves a batch of ORE file versions.
     *
     * @param versions The ORE file versions to save.
     * @throws std::exception on failure.
     */
    void save_versions(const std::vector<domain::ore_file_version>& versions);

    /**
     * @brief Deletes a ORE file version by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_version(const boost::uuids::uuid& id);

    /**
     * @brief Deletes ORE file versions by their primary keys.
     */
    void delete_versions(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a ORE file version.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::ore_file_version> get_version_history(const std::string& id);

private:
    context ctx_;
    repository::ore_file_version_repository repo_;

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
    prepare_change(const ::ores::reporting::messaging::ore_file_version_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::ore_file_version& out);
};

}

#endif
