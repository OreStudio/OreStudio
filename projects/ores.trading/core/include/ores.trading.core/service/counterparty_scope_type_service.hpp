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
#ifndef ORES_TRADING_CORE_SERVICE_COUNTERPARTY_SCOPE_TYPE_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_COUNTERPARTY_SCOPE_TYPE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/counterparty_scope_type.hpp"
#include "ores.trading.api/messaging/counterparty_scope_type_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/counterparty_scope_type_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing counterparty scope types.
 *
 * Provides a higher-level interface for counterparty scope type operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT counterparty_scope_type_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.counterparty_scope_type_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a counterparty_scope_type_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit counterparty_scope_type_service(context ctx);

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
    messaging::list_counterparty_scope_types_response
    list_counterparty_scope_types(const messaging::list_counterparty_scope_types_request& request);
    messaging::get_counterparty_scope_type_response
    get_counterparty_scope_type(const messaging::get_counterparty_scope_type_request& request);
    messaging::get_many_counterparty_scope_types_response get_many_counterparty_scope_types(
        const messaging::get_many_counterparty_scope_types_request& request);
    /**@}*/

    /**
     * @brief Lists counterparty scope types with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of counterparty scope types for the requested page.
     */
    std::vector<domain::counterparty_scope_type> list_counterparty_scope_types(std::uint32_t offset,
                                                                               std::uint32_t limit);

    /**
     * @brief Gets the total count of active counterparty scope types.
     *
     * @return Total number of active counterparty scope types.
     */
    std::uint32_t count_counterparty_scope_types();


    /**
     * @brief Retrieves a single counterparty scope type by its primary key.
     *
     * @return The counterparty scope type if found, std::nullopt otherwise.
     */
    std::optional<domain::counterparty_scope_type>
    get_counterparty_scope_type(const std::string& code);

    /**
     * @brief Retrieves a batch of counterparty scope types by primary key.
     */
    std::vector<domain::counterparty_scope_type>
    get_counterparty_scope_types(const std::vector<std::string>& codes);

    /**
     * @brief Saves a counterparty scope type (creates or updates).
     *
     * @param counterparty_scope_type The counterparty scope type to save.
     * @throws std::exception on failure.
     */
    void
    save_counterparty_scope_type(const domain::counterparty_scope_type& counterparty_scope_type);

    /**
     * @brief Saves a batch of counterparty scope types.
     *
     * @param counterparty_scope_types The counterparty scope types to save.
     * @throws std::exception on failure.
     */
    void save_counterparty_scope_types(
        const std::vector<domain::counterparty_scope_type>& counterparty_scope_types);

    /**
     * @brief Deletes a counterparty scope type by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_counterparty_scope_type(const std::string& code);

    /**
     * @brief Deletes counterparty scope types by their primary keys.
     */
    void delete_counterparty_scope_types(const std::vector<std::string>& codes);


private:
    context ctx_;
    repository::counterparty_scope_type_repository repo_;
};

}

#endif
