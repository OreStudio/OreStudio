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
#ifndef ORES_TRADING_CORE_SERVICE_BOOKING_NATURE_TYPE_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_BOOKING_NATURE_TYPE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/booking_nature_type.hpp"
#include "ores.trading.api/messaging/booking_nature_type_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/booking_nature_type_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing booking nature types.
 *
 * Provides a higher-level interface for booking nature type operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT booking_nature_type_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.booking_nature_type_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a booking_nature_type_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit booking_nature_type_service(context ctx);

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
    messaging::list_booking_nature_types_response
    list_booking_nature_types(const messaging::list_booking_nature_types_request& request);
    messaging::get_booking_nature_type_response
    get_booking_nature_type(const messaging::get_booking_nature_type_request& request);
    messaging::get_many_booking_nature_types_response
    get_many_booking_nature_types(const messaging::get_many_booking_nature_types_request& request);
    /**@}*/

    /**
     * @brief Lists booking nature types with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of booking nature types for the requested page.
     */
    std::vector<domain::booking_nature_type> list_booking_nature_types(std::uint32_t offset,
                                                                       std::uint32_t limit);

    /**
     * @brief Gets the total count of active booking nature types.
     *
     * @return Total number of active booking nature types.
     */
    std::uint32_t count_booking_nature_types();


    /**
     * @brief Retrieves a single booking nature type by its primary key.
     *
     * @return The booking nature type if found, std::nullopt otherwise.
     */
    std::optional<domain::booking_nature_type> get_booking_nature_type(const std::string& code);

    /**
     * @brief Retrieves a batch of booking nature types by primary key.
     */
    std::vector<domain::booking_nature_type>
    get_booking_nature_types(const std::vector<std::string>& codes);

    /**
     * @brief Saves a booking nature type (creates or updates).
     *
     * @param booking_nature_type The booking nature type to save.
     * @throws std::exception on failure.
     */
    void save_booking_nature_type(const domain::booking_nature_type& booking_nature_type);

    /**
     * @brief Saves a batch of booking nature types.
     *
     * @param booking_nature_types The booking nature types to save.
     * @throws std::exception on failure.
     */
    void
    save_booking_nature_types(const std::vector<domain::booking_nature_type>& booking_nature_types);

    /**
     * @brief Deletes a booking nature type by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_booking_nature_type(const std::string& code);

    /**
     * @brief Deletes booking nature types by their primary keys.
     */
    void delete_booking_nature_types(const std::vector<std::string>& codes);


private:
    context ctx_;
    repository::booking_nature_type_repository repo_;
};

}

#endif
