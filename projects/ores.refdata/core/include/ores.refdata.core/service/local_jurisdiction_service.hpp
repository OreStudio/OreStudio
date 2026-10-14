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
#ifndef ORES_REFDATA_CORE_SERVICE_LOCAL_JURISDICTION_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_LOCAL_JURISDICTION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/local_jurisdiction.hpp"
#include "ores.refdata.api/messaging/local_jurisdiction_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/local_jurisdiction_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing local jurisdictions.
 *
 * Provides a higher-level interface for local jurisdiction operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT local_jurisdiction_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.local_jurisdiction_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a local_jurisdiction_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit local_jurisdiction_service(context ctx);

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
    messaging::list_local_jurisdictions_response
    list_local_jurisdictions(const messaging::list_local_jurisdictions_request& request);
    messaging::get_local_jurisdiction_response
    get_local_jurisdiction(const messaging::get_local_jurisdiction_request& request);
    messaging::get_many_local_jurisdictions_response
    get_many_local_jurisdictions(const messaging::get_many_local_jurisdictions_request& request);
    messaging::put_local_jurisdiction_response
    put_local_jurisdiction(const messaging::put_local_jurisdiction_request& request);
    messaging::put_many_local_jurisdictions_response
    put_many_local_jurisdictions(const messaging::put_many_local_jurisdictions_request& request);
    messaging::delete_local_jurisdiction_response
    delete_local_jurisdiction(const messaging::delete_local_jurisdiction_request& request);
    messaging::delete_many_local_jurisdictions_response delete_many_local_jurisdictions(
        const messaging::delete_many_local_jurisdictions_request& request);
    messaging::list_local_jurisdiction_versions_response list_local_jurisdiction_versions(
        const messaging::list_local_jurisdiction_versions_request& request);
    messaging::get_local_jurisdiction_version_response get_local_jurisdiction_version(
        const messaging::get_local_jurisdiction_version_request& request);
    /**@}*/

    /**
     * @brief Lists local jurisdictions with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of local jurisdictions for the requested page.
     */
    std::vector<domain::local_jurisdiction> list_jurisdictions(std::uint32_t offset,
                                                               std::uint32_t limit);

    /**
     * @brief Gets the total count of active local jurisdictions.
     *
     * @return Total number of active local jurisdictions.
     */
    std::uint32_t count_jurisdictions();


    /**
     * @brief Retrieves a single local jurisdiction as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The local jurisdiction at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::local_jurisdiction> get_jurisdiction_at_version(
        const std::string& code, const std::string& coding_scheme_code, std::uint32_t version);

    /**
     * @brief Retrieves a single local jurisdiction by its primary key.
     *
     * @return The local jurisdiction if found, std::nullopt otherwise.
     */
    std::optional<domain::local_jurisdiction>
    get_jurisdiction(const std::string& code, const std::string& coding_scheme_code);

    /**
     * @brief Retrieves a batch of local jurisdictions by primary key.
     */
    std::vector<domain::local_jurisdiction>
    get_jurisdictions(const std::vector<std::string>& codes,
                      const std::vector<std::string>& coding_scheme_codes);

    /**
     * @brief Saves a local jurisdiction (creates or updates).
     *
     * @param jurisdiction The local jurisdiction to save.
     * @throws std::exception on failure.
     */
    void save_jurisdiction(const domain::local_jurisdiction& jurisdiction);

    /**
     * @brief Saves a batch of local jurisdictions.
     *
     * @param jurisdictions The local jurisdictions to save.
     * @throws std::exception on failure.
     */
    void save_jurisdictions(const std::vector<domain::local_jurisdiction>& jurisdictions);

    /**
     * @brief Deletes a local jurisdiction by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_jurisdiction(const std::string& code, const std::string& coding_scheme_code);

    /**
     * @brief Deletes local jurisdictions by their primary keys.
     */
    void delete_jurisdictions(const std::vector<std::string>& codes,
                              const std::vector<std::string>& coding_scheme_codes);

    /**
     * @brief Retrieves all historical versions of a local jurisdiction.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::local_jurisdiction>
    get_jurisdiction_history(const std::string& code, const std::string& coding_scheme_code);

private:
    context ctx_;
    repository::local_jurisdiction_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::local_jurisdiction_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::local_jurisdiction& out);
};

}

#endif
