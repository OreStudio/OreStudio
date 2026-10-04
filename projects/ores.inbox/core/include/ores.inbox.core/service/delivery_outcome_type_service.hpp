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
#ifndef ORES_INBOX_CORE_SERVICE_DELIVERY_OUTCOME_TYPE_SERVICE_HPP
#define ORES_INBOX_CORE_SERVICE_DELIVERY_OUTCOME_TYPE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.inbox.api/domain/delivery_outcome_type.hpp"
#include "ores.inbox.api/messaging/delivery_outcome_type_protocol.hpp"
#include "ores.inbox.core/export.hpp"
#include "ores.inbox.core/repository/delivery_outcome_type_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::inbox::service {

/**
 * @brief Service for managing delivery outcome types.
 *
 * Provides a higher-level interface for delivery outcome type operations,
 * wrapping the underlying repository.
 */
class ORES_INBOX_CORE_EXPORT delivery_outcome_type_service {
private:
    inline static std::string_view logger_name = "ores.inbox.service.delivery_outcome_type_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a delivery_outcome_type_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit delivery_outcome_type_service(context ctx);

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
    messaging::list_delivery_outcome_types_response
    list_delivery_outcome_types(const messaging::list_delivery_outcome_types_request& request);
    messaging::get_delivery_outcome_type_response
    get_delivery_outcome_type(const messaging::get_delivery_outcome_type_request& request);
    messaging::get_many_delivery_outcome_types_response get_many_delivery_outcome_types(
        const messaging::get_many_delivery_outcome_types_request& request);
    /**@}*/

    /**
     * @brief Lists delivery outcome types with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of delivery outcome types for the requested page.
     */
    std::vector<domain::delivery_outcome_type> list_delivery_outcome_types(std::uint32_t offset,
                                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active delivery outcome types.
     *
     * @return Total number of active delivery outcome types.
     */
    std::uint32_t count_delivery_outcome_types();


    /**
     * @brief Retrieves a single delivery outcome type by its primary key.
     *
     * @return The delivery outcome type if found, std::nullopt otherwise.
     */
    std::optional<domain::delivery_outcome_type> get_delivery_outcome_type(const std::string& code);

    /**
     * @brief Retrieves a batch of delivery outcome types by primary key.
     */
    std::vector<domain::delivery_outcome_type>
    get_delivery_outcome_types(const std::vector<std::string>& codes);

    /**
     * @brief Saves a delivery outcome type (creates or updates).
     *
     * @param delivery_outcome_type The delivery outcome type to save.
     * @throws std::exception on failure.
     */
    void save_delivery_outcome_type(const domain::delivery_outcome_type& delivery_outcome_type);

    /**
     * @brief Saves a batch of delivery outcome types.
     *
     * @param delivery_outcome_types The delivery outcome types to save.
     * @throws std::exception on failure.
     */
    void save_delivery_outcome_types(
        const std::vector<domain::delivery_outcome_type>& delivery_outcome_types);

    /**
     * @brief Deletes a delivery outcome type by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_delivery_outcome_type(const std::string& code);

    /**
     * @brief Deletes delivery outcome types by their primary keys.
     */
    void delete_delivery_outcome_types(const std::vector<std::string>& codes);


private:
    context ctx_;
    repository::delivery_outcome_type_repository repo_;
};

}

#endif
