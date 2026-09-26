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
#ifndef ORES_DQ_CORE_SERVICE_CODING_SCHEME_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_CODING_SCHEME_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/coding_scheme.hpp"
#include "ores.dq.api/messaging/coding_scheme_protocol.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/coding_scheme_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing coding schemes.
 *
 * Provides a higher-level interface for coding scheme operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT coding_scheme_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.coding_scheme_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a coding_scheme_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit coding_scheme_service(context ctx);

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
    messaging::list_coding_schemes_response
    list_coding_schemes(const messaging::list_coding_schemes_request& request);
    messaging::get_coding_scheme_response
    get_coding_scheme(const messaging::get_coding_scheme_request& request);
    messaging::get_many_coding_schemes_response
    get_many_coding_schemes(const messaging::get_many_coding_schemes_request& request);
    messaging::put_coding_scheme_response
    put_coding_scheme(const messaging::put_coding_scheme_request& request);
    messaging::put_many_coding_schemes_response
    put_many_coding_schemes(const messaging::put_many_coding_schemes_request& request);
    messaging::delete_coding_scheme_response
    delete_coding_scheme(const messaging::delete_coding_scheme_request& request);
    messaging::delete_many_coding_schemes_response
    delete_many_coding_schemes(const messaging::delete_many_coding_schemes_request& request);
    messaging::list_coding_scheme_versions_response
    list_coding_scheme_versions(const messaging::list_coding_scheme_versions_request& request);
    messaging::get_coding_scheme_version_response
    get_coding_scheme_version(const messaging::get_coding_scheme_version_request& request);
    /**@}*/

    /**
     * @brief Lists coding schemes with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of coding schemes for the requested page.
     */
    std::vector<domain::coding_scheme> list_schemes(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active coding schemes.
     *
     * @return Total number of active coding schemes.
     */
    std::uint32_t count_schemes();


    /**
     * @brief Retrieves a single coding scheme as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The coding scheme at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::coding_scheme> get_scheme_at_version(const std::string& code,
                                                               std::uint32_t version);

    /**
     * @brief Retrieves a single coding scheme by its primary key.
     *
     * @return The coding scheme if found, std::nullopt otherwise.
     */
    std::optional<domain::coding_scheme> get_scheme(const std::string& code);

    /**
     * @brief Retrieves a batch of coding schemes by primary key.
     */
    std::vector<domain::coding_scheme> get_schemes(const std::vector<std::string>& codes);

    /**
     * @brief Saves a coding scheme (creates or updates).
     *
     * @param scheme The coding scheme to save.
     * @throws std::exception on failure.
     */
    void save_scheme(const domain::coding_scheme& scheme);

    /**
     * @brief Saves a batch of coding schemes.
     *
     * @param schemes The coding schemes to save.
     * @throws std::exception on failure.
     */
    void save_schemes(const std::vector<domain::coding_scheme>& schemes);

    /**
     * @brief Deletes a coding scheme by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_scheme(const std::string& code);

    /**
     * @brief Deletes coding schemes by their primary keys.
     */
    void delete_schemes(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a coding scheme.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::coding_scheme> get_scheme_history(const std::string& code);

private:
    context ctx_;
    repository::coding_scheme_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::coding_scheme_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::coding_scheme& out);
};

}

#endif
