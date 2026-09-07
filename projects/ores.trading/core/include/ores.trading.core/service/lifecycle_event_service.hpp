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
#ifndef ORES_TRADING_CORE_SERVICE_LIFECYCLE_EVENT_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_LIFECYCLE_EVENT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/lifecycle_event.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/lifecycle_event_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing lifecycle events.
 *
 * Provides a higher-level interface for lifecycle event operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT lifecycle_event_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.lifecycle_event_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a lifecycle_event_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit lifecycle_event_service(context ctx);

    /**
     * @brief Lists lifecycle events with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of lifecycle events for the requested page.
     */
    std::vector<domain::lifecycle_event> list_events(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active lifecycle events.
     *
     * @return Total number of active lifecycle events.
     */
    std::uint32_t count_events();


    /**
     * @brief Retrieves a single lifecycle event as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The lifecycle event at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::lifecycle_event> get_event_at_version(const std::string& code,
                                                                std::uint32_t version);

    /**
     * @brief Retrieves a single lifecycle event by its primary key.
     *
     * @return The lifecycle event if found, std::nullopt otherwise.
     */
    std::optional<domain::lifecycle_event> get_event(const std::string& code);

    /**
     * @brief Saves a lifecycle event (creates or updates).
     *
     * @param event The lifecycle event to save.
     * @throws std::exception on failure.
     */
    void save_event(const domain::lifecycle_event& event);

    /**
     * @brief Saves a batch of lifecycle events.
     *
     * @param events The lifecycle events to save.
     * @throws std::exception on failure.
     */
    void save_events(const std::vector<domain::lifecycle_event>& events);

    /**
     * @brief Deletes a lifecycle event by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_event(const std::string& code);

    /**
     * @brief Deletes lifecycle events by their primary keys.
     */
    void delete_events(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a lifecycle event.
     */
    std::vector<domain::lifecycle_event> get_event_history(const std::string& code);

private:
    context ctx_;
    repository::lifecycle_event_repository repo_;
};

}

#endif
