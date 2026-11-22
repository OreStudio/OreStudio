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
#ifndef ORES_SYNTHETIC_CORE_SERVICE_STOPPED_SIMULATED_SOURCE_SERVICE_HPP
#define ORES_SYNTHETIC_CORE_SERVICE_STOPPED_SIMULATED_SOURCE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.synthetic.api/domain/stopped_simulated_source.hpp"
#include "ores.synthetic.api/messaging/stopped_simulated_source_protocol.hpp"
#include "ores.synthetic.core/export.hpp"
#include "ores.synthetic.core/repository/stopped_simulated_source_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Service for managing stopped simulated sources.
 *
 * Provides a higher-level interface for stopped simulated source operations,
 * wrapping the underlying repository.
 */
class ORES_SYNTHETIC_CORE_EXPORT stopped_simulated_source_service {
private:
    inline static std::string_view logger_name =
        "ores.synthetic.service.stopped_simulated_source_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a stopped_simulated_source_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit stopped_simulated_source_service(const context& ctx);

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
    ::ores::synthetic::messaging::list_stopped_simulated_sources_response
    list_stopped_simulated_sources(
        const ::ores::synthetic::messaging::list_stopped_simulated_sources_request& request);
    ::ores::synthetic::messaging::get_stopped_simulated_source_response
    get_stopped_simulated_source(
        const ::ores::synthetic::messaging::get_stopped_simulated_source_request& request);
    ::ores::synthetic::messaging::get_many_stopped_simulated_sources_response
    get_many_stopped_simulated_sources(
        const ::ores::synthetic::messaging::get_many_stopped_simulated_sources_request& request);
    ::ores::synthetic::messaging::put_stopped_simulated_source_response
    put_stopped_simulated_source(
        const ::ores::synthetic::messaging::put_stopped_simulated_source_request& request);
    ::ores::synthetic::messaging::put_many_stopped_simulated_sources_response
    put_many_stopped_simulated_sources(
        const ::ores::synthetic::messaging::put_many_stopped_simulated_sources_request& request);
    ::ores::synthetic::messaging::delete_stopped_simulated_source_response
    delete_stopped_simulated_source(
        const ::ores::synthetic::messaging::delete_stopped_simulated_source_request& request);
    ::ores::synthetic::messaging::delete_many_stopped_simulated_sources_response
    delete_many_stopped_simulated_sources(
        const ::ores::synthetic::messaging::delete_many_stopped_simulated_sources_request& request);
    ::ores::synthetic::messaging::list_stopped_simulated_source_versions_response
    list_stopped_simulated_source_versions(
        const ::ores::synthetic::messaging::list_stopped_simulated_source_versions_request&
            request);
    ::ores::synthetic::messaging::get_stopped_simulated_source_version_response
    get_stopped_simulated_source_version(
        const ::ores::synthetic::messaging::get_stopped_simulated_source_version_request& request);
    /**@}*/

    /**
     * @brief Lists stopped simulated sources with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of stopped simulated sources for the requested page.
     */
    std::vector<domain::stopped_simulated_source> list_stopped_sources(std::uint32_t offset,
                                                                       std::uint32_t limit);

    /**
     * @brief Gets the total count of active stopped simulated sources.
     *
     * @return Total number of active stopped simulated sources.
     */
    std::uint32_t count_stopped_sources();


    /**
     * @brief Retrieves a single stopped simulated source as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The stopped simulated source at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::stopped_simulated_source>
    get_stopped_source_at_version(const std::string& source_name, std::uint32_t version);

    /**
     * @brief Retrieves a single stopped simulated source by its primary key.
     *
     * @return The stopped simulated source if found, std::nullopt otherwise.
     */
    std::optional<domain::stopped_simulated_source>
    get_stopped_source(const std::string& source_name);

    /**
     * @brief Retrieves a batch of stopped simulated sources by primary key.
     */
    std::vector<domain::stopped_simulated_source>
    get_stopped_sources(const std::vector<std::string>& source_names);

    /**
     * @brief Saves a stopped simulated source (creates or updates).
     *
     * @param stopped_source The stopped simulated source to save.
     * @throws std::exception on failure.
     */
    void save_stopped_source(const domain::stopped_simulated_source& stopped_source);

    /**
     * @brief Saves a batch of stopped simulated sources.
     *
     * @param stopped_sources The stopped simulated sources to save.
     * @throws std::exception on failure.
     */
    void save_stopped_sources(const std::vector<domain::stopped_simulated_source>& stopped_sources);

    /**
     * @brief Deletes a stopped simulated source by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_stopped_source(const std::string& source_name);

    /**
     * @brief Deletes stopped simulated sources by their primary keys.
     */
    void delete_stopped_sources(const std::vector<std::string>& source_names);

    /**
     * @brief Retrieves all historical versions of a stopped simulated source.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::stopped_simulated_source>
    get_stopped_source_history(const std::string& source_name);

private:
    context ctx_;
    repository::stopped_simulated_source_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * The caller reads the current row, so a batch reads every row it names
     * in one query rather than one query per change.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param current The row the change names as it is now, or empty.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result
    prepare_change(const ::ores::synthetic::messaging::stopped_simulated_source_change& change,
                   const ores::utility::domain::change_intent& intent,
                   const std::vector<domain::stopped_simulated_source>& current,
                   domain::stopped_simulated_source& out);
};

}

#endif
