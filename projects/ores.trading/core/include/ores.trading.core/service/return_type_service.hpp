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
#ifndef ORES_TRADING_CORE_SERVICE_RETURN_TYPE_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_RETURN_TYPE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/return_type.hpp"
#include "ores.trading.api/messaging/return_type_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/return_type_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing return types.
 *
 * Provides a higher-level interface for return type operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT return_type_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.return_type_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a return_type_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit return_type_service(context ctx);

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
    messaging::list_return_types_response
    list_return_types(const messaging::list_return_types_request& request);
    messaging::get_return_type_response
    get_return_type(const messaging::get_return_type_request& request);
    messaging::get_many_return_types_response
    get_many_return_types(const messaging::get_many_return_types_request& request);
    messaging::put_return_type_response
    put_return_type(const messaging::put_return_type_request& request);
    messaging::put_many_return_types_response
    put_many_return_types(const messaging::put_many_return_types_request& request);
    messaging::delete_return_type_response
    delete_return_type(const messaging::delete_return_type_request& request);
    messaging::delete_many_return_types_response
    delete_many_return_types(const messaging::delete_many_return_types_request& request);
    messaging::list_return_type_versions_response
    list_return_type_versions(const messaging::list_return_type_versions_request& request);
    messaging::get_return_type_version_response
    get_return_type_version(const messaging::get_return_type_version_request& request);
    /**@}*/

    /**
     * @brief Lists return types with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of return types for the requested page.
     */
    std::vector<domain::return_type> list_return_types(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active return types.
     *
     * @return Total number of active return types.
     */
    std::uint32_t count_return_types();


    /**
     * @brief Retrieves a single return type as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The return type at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::return_type> get_return_type_at_version(const std::string& code,
                                                                  std::uint32_t version);

    /**
     * @brief Retrieves a single return type by its primary key.
     *
     * @return The return type if found, std::nullopt otherwise.
     */
    std::optional<domain::return_type> get_return_type(const std::string& code);

    /**
     * @brief Retrieves a batch of return types by primary key.
     */
    std::vector<domain::return_type> get_return_types(const std::vector<std::string>& codes);

    /**
     * @brief Saves a return type (creates or updates).
     *
     * @param return_type The return type to save.
     * @throws std::exception on failure.
     */
    void save_return_type(const domain::return_type& return_type);

    /**
     * @brief Saves a batch of return types.
     *
     * @param return_types The return types to save.
     * @throws std::exception on failure.
     */
    void save_return_types(const std::vector<domain::return_type>& return_types);

    /**
     * @brief Deletes a return type by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_return_type(const std::string& code);

    /**
     * @brief Deletes return types by their primary keys.
     */
    void delete_return_types(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a return type.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::return_type> get_return_type_history(const std::string& code);

private:
    context ctx_;
    repository::return_type_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::return_type_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::return_type& out);
};

}

#endif
