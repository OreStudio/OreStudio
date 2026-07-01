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
#ifndef ORES_MARKETDATA_CORE_SERVICE_FEED_BINDING_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_FEED_BINDING_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/feed_binding.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/feed_binding_repository.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Service for managing feed bindings.
 *
 * Provides a higher-level interface for feed binding operations,
 * wrapping the underlying repository.
 */
class ORES_MARKETDATA_CORE_EXPORT feed_binding_service {
private:
    inline static std::string_view logger_name = "ores.marketdata.service.feed_binding_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a feed_binding_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit feed_binding_service(context ctx);

    /**
     * @brief Lists feed bindings with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of feed bindings for the requested page.
     */
    std::vector<domain::feed_binding> list_feed_bindings(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active feed bindings.
     *
     * @return Total number of active feed bindings.
     */
    std::uint32_t count_feed_bindings();

    /**
     * @brief Retrieves a single feed binding by its id.
     *
     * @param id The id of the feed binding.
     * @return The feed binding if found, std::nullopt otherwise.
     */
    std::optional<domain::feed_binding> get_feed_binding(const std::string& id);

    /**
     * @brief Saves a feed binding (creates or updates).
     *
     * @param feed_binding The feed binding to save.
     * @throws std::exception on failure.
     */
    void save_feed_binding(const domain::feed_binding& feed_binding);

    /**
     * @brief Saves a batch of feed bindings.
     *
     * @param feed_bindings The feed bindings to save.
     * @throws std::exception on failure.
     */
    void save_feed_bindings(const std::vector<domain::feed_binding>& feed_bindings);

    /**
     * @brief Deletes a feed binding by its id.
     *
     * @param id The id of the feed binding to delete.
     * @throws std::exception on failure.
     */
    void delete_feed_binding(const std::string& id);

    /**
     * @brief Deletes feed bindings by their ids.
     */
    void delete_feed_bindings(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a feed binding.
     */
    std::vector<domain::feed_binding> get_feed_binding_history(const std::string& id);

private:
    context ctx_;
    repository::feed_binding_repository repo_;
};

}

#endif
