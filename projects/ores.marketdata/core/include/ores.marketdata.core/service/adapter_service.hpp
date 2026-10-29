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
#ifndef ORES_MARKETDATA_CORE_SERVICE_ADAPTER_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_ADAPTER_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/adapter.hpp"
#include "ores.marketdata.api/messaging/adapter_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/adapter_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Service for managing adapters.
 *
 * Provides a higher-level interface for adapter operations,
 * wrapping the underlying repository.
 */
class ORES_MARKETDATA_CORE_EXPORT adapter_service {
private:
    inline static std::string_view logger_name = "ores.marketdata.service.adapter_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a adapter_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit adapter_service(context ctx);

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
    ::ores::marketdata::messaging::list_adapters_response
    list_adapters(const ::ores::marketdata::messaging::list_adapters_request& request);
    ::ores::marketdata::messaging::get_adapter_response
    get_adapter(const ::ores::marketdata::messaging::get_adapter_request& request);
    ::ores::marketdata::messaging::get_many_adapters_response
    get_many_adapters(const ::ores::marketdata::messaging::get_many_adapters_request& request);
    ::ores::marketdata::messaging::list_adapter_versions_response list_adapter_versions(
        const ::ores::marketdata::messaging::list_adapter_versions_request& request);
    ::ores::marketdata::messaging::get_adapter_version_response
    get_adapter_version(const ::ores::marketdata::messaging::get_adapter_version_request& request);
    /**@}*/

    /**
     * @brief Lists adapters with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of adapters for the requested page.
     */
    std::vector<domain::adapter> list_adapters(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active adapters.
     *
     * @return Total number of active adapters.
     */
    std::uint32_t count_adapters();


    /**
     * @brief Retrieves a single adapter as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The adapter at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::adapter> get_adapter_at_version(const std::string& code,
                                                          std::uint32_t version);

    /**
     * @brief Retrieves a single adapter by its primary key.
     *
     * @return The adapter if found, std::nullopt otherwise.
     */
    std::optional<domain::adapter> get_adapter(const std::string& code);

    /**
     * @brief Retrieves a batch of adapters by primary key.
     */
    std::vector<domain::adapter> get_adapters(const std::vector<std::string>& codes);

    /**
     * @brief Saves a adapter (creates or updates).
     *
     * @param adapter The adapter to save.
     * @throws std::exception on failure.
     */
    void save_adapter(const domain::adapter& adapter);

    /**
     * @brief Saves a batch of adapters.
     *
     * @param adapters The adapters to save.
     * @throws std::exception on failure.
     */
    void save_adapters(const std::vector<domain::adapter>& adapters);

    /**
     * @brief Deletes a adapter by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_adapter(const std::string& code);

    /**
     * @brief Deletes adapters by their primary keys.
     */
    void delete_adapters(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a adapter.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::adapter> get_adapter_history(const std::string& code);

private:
    context ctx_;
    repository::adapter_repository repo_;
};

}

#endif
