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
#ifndef ORES_DQ_CORE_SERVICE_BADGE_DEFINITION_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_BADGE_DEFINITION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/badge_definition.hpp"
#include "ores.dq.api/messaging/badge_definition_protocol.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/badge_definition_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing badge definitions.
 *
 * Provides a higher-level interface for badge definition operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT badge_definition_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.badge_definition_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a badge_definition_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit badge_definition_service(context ctx);

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
    messaging::list_badge_definitions_response
    list_badge_definitions(const messaging::list_badge_definitions_request& request);
    messaging::get_badge_definition_response
    get_badge_definition(const messaging::get_badge_definition_request& request);
    messaging::get_many_badge_definitions_response
    get_many_badge_definitions(const messaging::get_many_badge_definitions_request& request);
    messaging::put_badge_definition_response
    put_badge_definition(const messaging::put_badge_definition_request& request);
    messaging::put_many_badge_definitions_response
    put_many_badge_definitions(const messaging::put_many_badge_definitions_request& request);
    messaging::delete_badge_definition_response
    delete_badge_definition(const messaging::delete_badge_definition_request& request);
    messaging::delete_many_badge_definitions_response
    delete_many_badge_definitions(const messaging::delete_many_badge_definitions_request& request);
    messaging::list_badge_definition_versions_response list_badge_definition_versions(
        const messaging::list_badge_definition_versions_request& request);
    messaging::get_badge_definition_version_response
    get_badge_definition_version(const messaging::get_badge_definition_version_request& request);
    /**@}*/

    /**
     * @brief Lists badge definitions with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of badge definitions for the requested page.
     */
    std::vector<domain::badge_definition> list_definitions(std::uint32_t offset,
                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active badge definitions.
     *
     * @return Total number of active badge definitions.
     */
    std::uint32_t count_definitions();


    /**
     * @brief Retrieves a single badge definition as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The badge definition at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::badge_definition> get_definition_at_version(const std::string& code,
                                                                      std::uint32_t version);

    /**
     * @brief Retrieves a single badge definition by its primary key.
     *
     * @return The badge definition if found, std::nullopt otherwise.
     */
    std::optional<domain::badge_definition> get_definition(const std::string& code);

    /**
     * @brief Retrieves a batch of badge definitions by primary key.
     */
    std::vector<domain::badge_definition> get_definitions(const std::vector<std::string>& codes);

    /**
     * @brief Saves a badge definition (creates or updates).
     *
     * @param definition The badge definition to save.
     * @throws std::exception on failure.
     */
    void save_definition(const domain::badge_definition& definition);

    /**
     * @brief Saves a batch of badge definitions.
     *
     * @param definitions The badge definitions to save.
     * @throws std::exception on failure.
     */
    void save_definitions(const std::vector<domain::badge_definition>& definitions);

    /**
     * @brief Deletes a badge definition by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_definition(const std::string& code);

    /**
     * @brief Deletes badge definitions by their primary keys.
     */
    void delete_definitions(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a badge definition.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::badge_definition> get_definition_history(const std::string& code);

private:
    context ctx_;
    repository::badge_definition_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::badge_definition_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::badge_definition& out);
};

}

#endif
