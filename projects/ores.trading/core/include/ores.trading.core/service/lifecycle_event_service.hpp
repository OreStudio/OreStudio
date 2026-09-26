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
#ifndef ORES_TRADING_CORE_SERVICE_LIFECYCLE_EVENT_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_LIFECYCLE_EVENT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/lifecycle_event.hpp"
#include "ores.trading.api/messaging/lifecycle_event_protocol.hpp"
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
     * @brief The protocol operations, one method per subject.
     *
     * A method takes the canonical request and answers its response, so the
     * handler that serves the subject decodes, calls and replies without
     * deciding anything. The result a caller reads -- missing, conflicting,
     * denied -- is filled here, where the storage call that decided it is
     * made, rather than being inferred from an exception.
     */
    /**@{*/
    messaging::list_lifecycle_events_response
    list_lifecycle_events(const messaging::list_lifecycle_events_request& request);
    messaging::get_lifecycle_event_response
    get_lifecycle_event(const messaging::get_lifecycle_event_request& request);
    messaging::get_many_lifecycle_events_response
    get_many_lifecycle_events(const messaging::get_many_lifecycle_events_request& request);
    messaging::put_lifecycle_event_response
    put_lifecycle_event(const messaging::put_lifecycle_event_request& request);
    messaging::put_many_lifecycle_events_response
    put_many_lifecycle_events(const messaging::put_many_lifecycle_events_request& request);
    messaging::delete_lifecycle_event_response
    delete_lifecycle_event(const messaging::delete_lifecycle_event_request& request);
    messaging::delete_many_lifecycle_events_response
    delete_many_lifecycle_events(const messaging::delete_many_lifecycle_events_request& request);
    messaging::list_lifecycle_event_versions_response
    list_lifecycle_event_versions(const messaging::list_lifecycle_event_versions_request& request);
    messaging::get_lifecycle_event_version_response
    get_lifecycle_event_version(const messaging::get_lifecycle_event_version_request& request);
    /**@}*/

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
     * @brief Retrieves a batch of lifecycle events by primary key.
     */
    std::vector<domain::lifecycle_event> get_events(const std::vector<std::string>& codes);

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
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::lifecycle_event> get_event_history(const std::string& code);

private:
    context ctx_;
    repository::lifecycle_event_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::lifecycle_event_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::lifecycle_event& out);
};

}

#endif
