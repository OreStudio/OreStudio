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
#ifndef ORES_REFDATA_CORE_SERVICE_CONVENTIONS_IMPORT_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CONVENTIONS_IMPORT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/conventions_import.hpp"
#include "ores.refdata.api/messaging/conventions_import_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/conventions_import_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing conventions imports.
 *
 * Provides a higher-level interface for conventions import operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT conventions_import_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.conventions_import_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a conventions_import_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit conventions_import_service(context ctx);

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
    messaging::list_conventions_imports_response
    list_conventions_imports(const messaging::list_conventions_imports_request& request);
    messaging::get_conventions_import_response
    get_conventions_import(const messaging::get_conventions_import_request& request);
    messaging::get_many_conventions_imports_response
    get_many_conventions_imports(const messaging::get_many_conventions_imports_request& request);
    messaging::put_conventions_import_response
    put_conventions_import(const messaging::put_conventions_import_request& request);
    messaging::put_many_conventions_imports_response
    put_many_conventions_imports(const messaging::put_many_conventions_imports_request& request);
    messaging::delete_conventions_import_response
    delete_conventions_import(const messaging::delete_conventions_import_request& request);
    messaging::delete_many_conventions_imports_response delete_many_conventions_imports(
        const messaging::delete_many_conventions_imports_request& request);
    messaging::list_conventions_import_versions_response list_conventions_import_versions(
        const messaging::list_conventions_import_versions_request& request);
    messaging::get_conventions_import_version_response get_conventions_import_version(
        const messaging::get_conventions_import_version_request& request);
    /**@}*/

    /**
     * @brief Lists conventions imports with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of conventions imports for the requested page.
     */
    std::vector<domain::conventions_import> list_conventions_imports(std::uint32_t offset,
                                                                     std::uint32_t limit);

    /**
     * @brief Gets the total count of active conventions imports.
     *
     * @return Total number of active conventions imports.
     */
    std::uint32_t count_conventions_imports();


    /**
     * @brief Retrieves a single conventions import as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The conventions import at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::conventions_import>
    get_conventions_import_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single conventions import by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The conventions import if found, std::nullopt otherwise.
     */
    std::optional<domain::conventions_import> get_conventions_import(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of conventions imports by primary key.
     */
    std::vector<domain::conventions_import>
    get_conventions_imports(const std::vector<std::string>& ids);

    /**
     * @brief Saves a conventions import (creates or updates).
     *
     * @param conventions_import The conventions import to save.
     * @throws std::exception on failure.
     */
    void save_conventions_import(const domain::conventions_import& conventions_import);

    /**
     * @brief Saves a batch of conventions imports.
     *
     * @param conventions_imports The conventions imports to save.
     * @throws std::exception on failure.
     */
    void
    save_conventions_imports(const std::vector<domain::conventions_import>& conventions_imports);

    /**
     * @brief Deletes a conventions import by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_conventions_import(const boost::uuids::uuid& id);

    /**
     * @brief Deletes conventions imports by their primary keys.
     */
    void delete_conventions_imports(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a conventions import.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::conventions_import> get_conventions_import_history(const std::string& id);

private:
    context ctx_;
    repository::conventions_import_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::conventions_import_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::conventions_import& out);
};

}

#endif
