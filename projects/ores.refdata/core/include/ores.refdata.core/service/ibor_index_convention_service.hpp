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
#ifndef ORES_REFDATA_CORE_SERVICE_IBOR_INDEX_CONVENTION_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_IBOR_INDEX_CONVENTION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/ibor_index_convention.hpp"
#include "ores.refdata.api/messaging/ibor_index_convention_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/ibor_index_convention_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing IBOR index conventions.
 *
 * Provides a higher-level interface for IBOR index convention operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT ibor_index_convention_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.ibor_index_convention_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a ibor_index_convention_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit ibor_index_convention_service(context ctx);

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
    messaging::list_ibor_index_conventions_response
    list_ibor_index_conventions(const messaging::list_ibor_index_conventions_request& request);
    messaging::get_ibor_index_convention_response
    get_ibor_index_convention(const messaging::get_ibor_index_convention_request& request);
    messaging::get_many_ibor_index_conventions_response get_many_ibor_index_conventions(
        const messaging::get_many_ibor_index_conventions_request& request);
    messaging::put_ibor_index_convention_response
    put_ibor_index_convention(const messaging::put_ibor_index_convention_request& request);
    messaging::put_many_ibor_index_conventions_response put_many_ibor_index_conventions(
        const messaging::put_many_ibor_index_conventions_request& request);
    messaging::delete_ibor_index_convention_response
    delete_ibor_index_convention(const messaging::delete_ibor_index_convention_request& request);
    messaging::delete_many_ibor_index_conventions_response delete_many_ibor_index_conventions(
        const messaging::delete_many_ibor_index_conventions_request& request);
    messaging::list_ibor_index_convention_versions_response list_ibor_index_convention_versions(
        const messaging::list_ibor_index_convention_versions_request& request);
    messaging::get_ibor_index_convention_version_response get_ibor_index_convention_version(
        const messaging::get_ibor_index_convention_version_request& request);
    /**@}*/

    /**
     * @brief Lists IBOR index conventions with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of IBOR index conventions for the requested page.
     */
    std::vector<domain::ibor_index_convention> list_ibor_index_conventions(std::uint32_t offset,
                                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active IBOR index conventions.
     *
     * @return Total number of active IBOR index conventions.
     */
    std::uint32_t count_ibor_index_conventions();


    /**
     * @brief Retrieves a single IBOR index convention as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The IBOR index convention at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::ibor_index_convention>
    get_ibor_index_convention_at_version(const std::string& id, std::uint32_t version);

    /**
     * @brief Retrieves a single IBOR index convention by its primary key.
     *
     * @return The IBOR index convention if found, std::nullopt otherwise.
     */
    std::optional<domain::ibor_index_convention> get_ibor_index_convention(const std::string& id);

    /**
     * @brief Retrieves a batch of IBOR index conventions by primary key.
     */
    std::vector<domain::ibor_index_convention>
    get_ibor_index_conventions(const std::vector<std::string>& ids);

    /**
     * @brief Saves a IBOR index convention (creates or updates).
     *
     * @param ibor_index_convention The IBOR index convention to save.
     * @throws std::exception on failure.
     */
    void save_ibor_index_convention(const domain::ibor_index_convention& ibor_index_convention);

    /**
     * @brief Saves a batch of IBOR index conventions.
     *
     * @param ibor_index_conventions The IBOR index conventions to save.
     * @throws std::exception on failure.
     */
    void save_ibor_index_conventions(
        const std::vector<domain::ibor_index_convention>& ibor_index_conventions);

    /**
     * @brief Deletes a IBOR index convention by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_ibor_index_convention(const std::string& id);

    /**
     * @brief Deletes IBOR index conventions by their primary keys.
     */
    void delete_ibor_index_conventions(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a IBOR index convention.
     */
    std::vector<domain::ibor_index_convention>
    get_ibor_index_convention_history(const std::string& id);

private:
    context ctx_;
    repository::ibor_index_convention_repository repo_;

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
    prepare_change(const messaging::ibor_index_convention_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::ibor_index_convention& out);
};

}

#endif
