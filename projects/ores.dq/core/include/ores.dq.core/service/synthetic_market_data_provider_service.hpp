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
#ifndef ORES_DQ_CORE_SERVICE_SYNTHETIC_MARKET_DATA_PROVIDER_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_SYNTHETIC_MARKET_DATA_PROVIDER_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/synthetic_market_data_provider.hpp"
#include "ores.dq.api/messaging/synthetic_market_data_provider_protocol.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/synthetic_market_data_provider_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing synthetic market data providers.
 *
 * Provides a higher-level interface for synthetic market data provider operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT synthetic_market_data_provider_service {
private:
    inline static std::string_view logger_name =
        "ores.dq.service.synthetic_market_data_provider_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a synthetic_market_data_provider_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit synthetic_market_data_provider_service(context ctx);

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
    messaging::list_synthetic_market_data_providers_response list_synthetic_market_data_providers(
        const messaging::list_synthetic_market_data_providers_request& request);
    messaging::get_synthetic_market_data_provider_response get_synthetic_market_data_provider(
        const messaging::get_synthetic_market_data_provider_request& request);
    messaging::get_many_synthetic_market_data_providers_response
    get_many_synthetic_market_data_providers(
        const messaging::get_many_synthetic_market_data_providers_request& request);
    /**@}*/

    /**
     * @brief Lists synthetic market data providers with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of synthetic market data providers for the requested page.
     */
    std::vector<domain::synthetic_market_data_provider> list_providers(std::uint32_t offset,
                                                                       std::uint32_t limit);

    /**
     * @brief Gets the total count of active synthetic market data providers.
     *
     * @return Total number of active synthetic market data providers.
     */
    std::uint32_t count_providers();


    /**
     * @brief Retrieves a single synthetic market data provider by its primary key.
     *
     * @return The synthetic market data provider if found, std::nullopt otherwise.
     */
    std::optional<domain::synthetic_market_data_provider> get_provider(const std::string& code);

    /**
     * @brief Retrieves a batch of synthetic market data providers by primary key.
     */
    std::vector<domain::synthetic_market_data_provider>
    get_providers(const std::vector<std::string>& codes);

    /**
     * @brief Saves a synthetic market data provider (creates or updates).
     *
     * @param provider The synthetic market data provider to save.
     * @throws std::exception on failure.
     */
    void save_provider(const domain::synthetic_market_data_provider& provider);

    /**
     * @brief Saves a batch of synthetic market data providers.
     *
     * @param providers The synthetic market data providers to save.
     * @throws std::exception on failure.
     */
    void save_providers(const std::vector<domain::synthetic_market_data_provider>& providers);

    /**
     * @brief Deletes a synthetic market data provider by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_provider(const std::string& code);

    /**
     * @brief Deletes synthetic market data providers by their primary keys.
     */
    void delete_providers(const std::vector<std::string>& codes);


private:
    context ctx_;
    repository::synthetic_market_data_provider_repository repo_;
};

}

#endif
