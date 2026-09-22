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
#ifndef ORES_REFDATA_CORE_SERVICE_PARTY_TYPE_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_PARTY_TYPE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/party_type.hpp"
#include "ores.refdata.api/messaging/party_type_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/party_type_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing party types.
 *
 * Provides a higher-level interface for party type operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT party_type_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.party_type_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a party_type_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit party_type_service(context ctx);

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
    messaging::list_party_types_response
    list_party_types(const messaging::list_party_types_request& request);
    messaging::get_party_type_response
    get_party_type(const messaging::get_party_type_request& request);
    messaging::get_many_party_types_response
    get_many_party_types(const messaging::get_many_party_types_request& request);
    messaging::put_party_type_response
    put_party_type(const messaging::put_party_type_request& request);
    messaging::put_many_party_types_response
    put_many_party_types(const messaging::put_many_party_types_request& request);
    messaging::delete_party_type_response
    delete_party_type(const messaging::delete_party_type_request& request);
    messaging::delete_many_party_types_response
    delete_many_party_types(const messaging::delete_many_party_types_request& request);
    messaging::list_party_type_versions_response
    list_party_type_versions(const messaging::list_party_type_versions_request& request);
    messaging::get_party_type_version_response
    get_party_type_version(const messaging::get_party_type_version_request& request);
    /**@}*/

    /**
     * @brief Lists party types with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of party types for the requested page.
     */
    std::vector<domain::party_type> list_types(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active party types.
     *
     * @return Total number of active party types.
     */
    std::uint32_t count_types();


    /**
     * @brief Retrieves a single party type as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The party type at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::party_type> get_type_at_version(const std::string& code,
                                                          std::uint32_t version);

    /**
     * @brief Retrieves a single party type by its primary key.
     *
     * @return The party type if found, std::nullopt otherwise.
     */
    std::optional<domain::party_type> find_type(const std::string& code);

    /**
     * @brief Retrieves a batch of party types by primary key.
     */
    std::vector<domain::party_type> get_types(const std::vector<std::string>& codes);

    /**
     * @brief Saves a party type (creates or updates).
     *
     * @param type The party type to save.
     * @throws std::exception on failure.
     */
    void save_type(const domain::party_type& type);

    /**
     * @brief Saves a batch of party types.
     *
     * @param types The party types to save.
     * @throws std::exception on failure.
     */
    void save_types(const std::vector<domain::party_type>& types);

    /**
     * @brief Deletes a party type by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_type(const std::string& code);

    /**
     * @brief Deletes party types by their primary keys.
     */
    void delete_types(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a party type.
     */
    std::vector<domain::party_type> get_type_history(const std::string& code);

private:
    context ctx_;
    repository::party_type_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::party_type_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::party_type& out);
};

}

#endif
