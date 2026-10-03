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
#ifndef ORES_TRADING_CORE_SERVICE_ENTRY_CHANNEL_TYPE_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_ENTRY_CHANNEL_TYPE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/entry_channel_type.hpp"
#include "ores.trading.api/messaging/entry_channel_type_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/entry_channel_type_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing entry channel types.
 *
 * Provides a higher-level interface for entry channel type operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT entry_channel_type_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.entry_channel_type_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a entry_channel_type_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit entry_channel_type_service(context ctx);

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
    messaging::list_entry_channel_types_response
    list_entry_channel_types(const messaging::list_entry_channel_types_request& request);
    messaging::get_entry_channel_type_response
    get_entry_channel_type(const messaging::get_entry_channel_type_request& request);
    messaging::get_many_entry_channel_types_response
    get_many_entry_channel_types(const messaging::get_many_entry_channel_types_request& request);
    /**@}*/

    /**
     * @brief Lists entry channel types with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of entry channel types for the requested page.
     */
    std::vector<domain::entry_channel_type> list_entry_channel_types(std::uint32_t offset,
                                                                     std::uint32_t limit);

    /**
     * @brief Gets the total count of active entry channel types.
     *
     * @return Total number of active entry channel types.
     */
    std::uint32_t count_entry_channel_types();


    /**
     * @brief Retrieves a single entry channel type by its primary key.
     *
     * @return The entry channel type if found, std::nullopt otherwise.
     */
    std::optional<domain::entry_channel_type> get_entry_channel_type(const std::string& code);

    /**
     * @brief Retrieves a batch of entry channel types by primary key.
     */
    std::vector<domain::entry_channel_type>
    get_entry_channel_types(const std::vector<std::string>& codes);

    /**
     * @brief Saves a entry channel type (creates or updates).
     *
     * @param entry_channel_type The entry channel type to save.
     * @throws std::exception on failure.
     */
    void save_entry_channel_type(const domain::entry_channel_type& entry_channel_type);

    /**
     * @brief Saves a batch of entry channel types.
     *
     * @param entry_channel_types The entry channel types to save.
     * @throws std::exception on failure.
     */
    void
    save_entry_channel_types(const std::vector<domain::entry_channel_type>& entry_channel_types);

    /**
     * @brief Deletes a entry channel type by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_entry_channel_type(const std::string& code);

    /**
     * @brief Deletes entry channel types by their primary keys.
     */
    void delete_entry_channel_types(const std::vector<std::string>& codes);


private:
    context ctx_;
    repository::entry_channel_type_repository repo_;
};

}

#endif
