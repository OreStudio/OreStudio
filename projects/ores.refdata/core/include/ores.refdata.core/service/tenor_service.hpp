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
#ifndef ORES_REFDATA_CORE_SERVICE_TENOR_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_TENOR_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/tenor.hpp"
#include "ores.refdata.api/messaging/tenor_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/tenor_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing tenors.
 *
 * Provides a higher-level interface for tenor operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT tenor_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.tenor_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a tenor_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit tenor_service(context ctx);

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
    messaging::list_tenors_response list_tenors(const messaging::list_tenors_request& request);
    messaging::get_tenor_response get_tenor(const messaging::get_tenor_request& request);
    messaging::get_many_tenors_response
    get_many_tenors(const messaging::get_many_tenors_request& request);
    messaging::put_tenor_response put_tenor(const messaging::put_tenor_request& request);
    messaging::put_many_tenors_response
    put_many_tenors(const messaging::put_many_tenors_request& request);
    messaging::delete_tenor_response delete_tenor(const messaging::delete_tenor_request& request);
    messaging::delete_many_tenors_response
    delete_many_tenors(const messaging::delete_many_tenors_request& request);
    messaging::list_tenor_versions_response
    list_tenor_versions(const messaging::list_tenor_versions_request& request);
    messaging::get_tenor_version_response
    get_tenor_version(const messaging::get_tenor_version_request& request);
    /**@}*/

    /**
     * @brief Lists tenors with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of tenors for the requested page.
     */
    std::vector<domain::tenor> list_tenors(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active tenors.
     *
     * @return Total number of active tenors.
     */
    std::uint32_t count_tenors();


    /**
     * @brief Retrieves a single tenor as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The tenor at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::tenor> get_tenor_at_version(const std::string& code,
                                                      std::uint32_t version);

    /**
     * @brief Retrieves a single tenor by its primary key.
     *
     * @return The tenor if found, std::nullopt otherwise.
     */
    std::optional<domain::tenor> get_tenor(const std::string& code);

    /**
     * @brief Retrieves a batch of tenors by primary key.
     */
    std::vector<domain::tenor> get_tenors(const std::vector<std::string>& codes);

    /**
     * @brief Saves a tenor (creates or updates).
     *
     * @param tenor The tenor to save.
     * @throws std::exception on failure.
     */
    void save_tenor(const domain::tenor& tenor);

    /**
     * @brief Saves a batch of tenors.
     *
     * @param tenors The tenors to save.
     * @throws std::exception on failure.
     */
    void save_tenors(const std::vector<domain::tenor>& tenors);

    /**
     * @brief Deletes a tenor by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_tenor(const std::string& code);

    /**
     * @brief Deletes tenors by their primary keys.
     */
    void delete_tenors(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a tenor.
     */
    std::vector<domain::tenor> get_tenor_history(const std::string& code);

private:
    context ctx_;
    repository::tenor_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::tenor_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::tenor& out);
};

}

#endif
