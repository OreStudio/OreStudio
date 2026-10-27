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
#ifndef ORES_SYNTHETIC_CORE_SERVICE_MARKET_DATA_PROVIDER_SERVICE_HPP
#define ORES_SYNTHETIC_CORE_SERVICE_MARKET_DATA_PROVIDER_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.synthetic.api/domain/market_data_provider.hpp"
#include "ores.synthetic.api/messaging/market_data_provider_protocol.hpp"
#include "ores.synthetic.core/export.hpp"
#include "ores.synthetic.core/repository/market_data_provider_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Service for managing market data providers.
 *
 * Provides a higher-level interface for market data provider operations,
 * wrapping the underlying repository.
 */
class ORES_SYNTHETIC_CORE_EXPORT market_data_provider_service {
private:
    inline static std::string_view logger_name =
        "ores.synthetic.service.market_data_provider_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a market_data_provider_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit market_data_provider_service(context ctx);

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
    ::ores::synthetic::messaging::list_market_data_providers_response list_market_data_providers(
        const ::ores::synthetic::messaging::list_market_data_providers_request& request);
    ::ores::synthetic::messaging::get_market_data_provider_response get_market_data_provider(
        const ::ores::synthetic::messaging::get_market_data_provider_request& request);
    ::ores::synthetic::messaging::get_many_market_data_providers_response
    get_many_market_data_providers(
        const ::ores::synthetic::messaging::get_many_market_data_providers_request& request);
    ::ores::synthetic::messaging::put_market_data_provider_response put_market_data_provider(
        const ::ores::synthetic::messaging::put_market_data_provider_request& request);
    ::ores::synthetic::messaging::put_many_market_data_providers_response
    put_many_market_data_providers(
        const ::ores::synthetic::messaging::put_many_market_data_providers_request& request);
    ::ores::synthetic::messaging::delete_market_data_provider_response delete_market_data_provider(
        const ::ores::synthetic::messaging::delete_market_data_provider_request& request);
    ::ores::synthetic::messaging::delete_many_market_data_providers_response
    delete_many_market_data_providers(
        const ::ores::synthetic::messaging::delete_many_market_data_providers_request& request);
    ::ores::synthetic::messaging::list_market_data_provider_versions_response
    list_market_data_provider_versions(
        const ::ores::synthetic::messaging::list_market_data_provider_versions_request& request);
    ::ores::synthetic::messaging::get_market_data_provider_version_response
    get_market_data_provider_version(
        const ::ores::synthetic::messaging::get_market_data_provider_version_request& request);
    /**@}*/

    /**
     * @brief Lists market data providers with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of market data providers for the requested page.
     */
    std::vector<domain::market_data_provider> list_providers(std::uint32_t offset,
                                                             std::uint32_t limit);

    /**
     * @brief Gets the total count of active market data providers.
     *
     * @return Total number of active market data providers.
     */
    std::uint32_t count_providers();


    /**
     * @brief Retrieves a single market data provider as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The market data provider at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::market_data_provider> get_provider_at_version(const std::string& code,
                                                                        std::uint32_t version);

    /**
     * @brief Retrieves a single market data provider by its primary key.
     *
     * @return The market data provider if found, std::nullopt otherwise.
     */
    std::optional<domain::market_data_provider> get_provider(const std::string& code);

    /**
     * @brief Retrieves a batch of market data providers by primary key.
     */
    std::vector<domain::market_data_provider> get_providers(const std::vector<std::string>& codes);

    /**
     * @brief Saves a market data provider (creates or updates).
     *
     * @param provider The market data provider to save.
     * @throws std::exception on failure.
     */
    void save_provider(const domain::market_data_provider& provider);

    /**
     * @brief Saves a batch of market data providers.
     *
     * @param providers The market data providers to save.
     * @throws std::exception on failure.
     */
    void save_providers(const std::vector<domain::market_data_provider>& providers);

    /**
     * @brief Deletes a market data provider by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_provider(const std::string& code);

    /**
     * @brief Deletes market data providers by their primary keys.
     */
    void delete_providers(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a market data provider.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::market_data_provider> get_provider_history(const std::string& code);

private:
    context ctx_;
    repository::market_data_provider_repository repo_;

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
    ores::utility::domain::result
    prepare_change(const ::ores::synthetic::messaging::market_data_provider_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::market_data_provider& out);
};

}

#endif
