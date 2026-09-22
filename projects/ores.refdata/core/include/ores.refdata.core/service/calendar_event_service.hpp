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
#ifndef ORES_REFDATA_CORE_SERVICE_CALENDAR_EVENT_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CALENDAR_EVENT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/calendar_event.hpp"
#include "ores.refdata.api/messaging/calendar_event_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/calendar_event_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing calendar events.
 *
 * Provides a higher-level interface for calendar event operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT calendar_event_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.calendar_event_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a calendar_event_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit calendar_event_service(context ctx);

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
    messaging::list_calendar_events_response
    list_calendar_events(const messaging::list_calendar_events_request& request);
    messaging::get_calendar_event_response
    get_calendar_event(const messaging::get_calendar_event_request& request);
    messaging::get_many_calendar_events_response
    get_many_calendar_events(const messaging::get_many_calendar_events_request& request);
    messaging::put_calendar_event_response
    put_calendar_event(const messaging::put_calendar_event_request& request);
    messaging::put_many_calendar_events_response
    put_many_calendar_events(const messaging::put_many_calendar_events_request& request);
    messaging::delete_calendar_event_response
    delete_calendar_event(const messaging::delete_calendar_event_request& request);
    messaging::delete_many_calendar_events_response
    delete_many_calendar_events(const messaging::delete_many_calendar_events_request& request);
    messaging::list_by_calendar_code_calendar_events_response list_by_calendar_code_calendar_events(
        const messaging::list_by_calendar_code_calendar_events_request& request);
    messaging::list_by_diary_entry_type_calendar_events_response
    list_by_diary_entry_type_calendar_events(
        const messaging::list_by_diary_entry_type_calendar_events_request& request);
    messaging::list_calendar_event_versions_response
    list_calendar_event_versions(const messaging::list_calendar_event_versions_request& request);
    messaging::get_calendar_event_version_response
    get_calendar_event_version(const messaging::get_calendar_event_version_request& request);
    /**@}*/

    /**
     * @brief Lists calendar events with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of calendar events for the requested page.
     */
    std::vector<domain::calendar_event> list_calendar_events(std::uint32_t offset,
                                                             std::uint32_t limit);

    /**
     * @brief Gets the total count of active calendar events.
     *
     * @return Total number of active calendar events.
     */
    std::uint32_t count_calendar_events();


    /**
     * @brief Lists calendar events filtered by calendar_code, with pagination.
     *
     * @param calendar_code The calendar_code to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching calendar events for the requested page.
     */
    std::vector<domain::calendar_event> list_calendar_events_by_calendar_code(
        const std::string& calendar_code, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active calendar events filtered by calendar_code.
     *
     * @param calendar_code The calendar_code to filter by.
     * @return Total number of matching calendar events.
     */
    std::uint32_t count_calendar_events_by_calendar_code(const std::string& calendar_code);


    /**
     * @brief Lists calendar events filtered by calendar_code that were live at
     * any point during a parent version's own [valid_from, valid_to) window.
     * See the "Temporal composite entity versioning" architecture doc.
     *
     * @param calendar_code The calendar_code to filter by.
     * @param valid_from_bound The parent version's own valid_from.
     * @param valid_to_bound The parent version's own valid_to.
     * @return Vector of matching calendar events.
     */
    std::vector<domain::calendar_event> list_calendar_events_by_calendar_code_as_of(
        const std::string& calendar_code,
        std::chrono::system_clock::time_point valid_from_bound,
        std::chrono::system_clock::time_point valid_to_bound);

    /**
     * @brief Lists calendar events filtered by diary_entry_type, with pagination.
     *
     * @param diary_entry_type The diary_entry_type to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching calendar events for the requested page.
     */
    std::vector<domain::calendar_event> list_calendar_events_by_diary_entry_type(
        const std::string& diary_entry_type, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active calendar events filtered by diary_entry_type.
     *
     * @param diary_entry_type The diary_entry_type to filter by.
     * @return Total number of matching calendar events.
     */
    std::uint32_t count_calendar_events_by_diary_entry_type(const std::string& diary_entry_type);


    /**
     * @brief Lists calendar events filtered by diary_entry_type that were live at
     * any point during a parent version's own [valid_from, valid_to) window.
     * See the "Temporal composite entity versioning" architecture doc.
     *
     * @param diary_entry_type The diary_entry_type to filter by.
     * @param valid_from_bound The parent version's own valid_from.
     * @param valid_to_bound The parent version's own valid_to.
     * @return Vector of matching calendar events.
     */
    std::vector<domain::calendar_event> list_calendar_events_by_diary_entry_type_as_of(
        const std::string& diary_entry_type,
        std::chrono::system_clock::time_point valid_from_bound,
        std::chrono::system_clock::time_point valid_to_bound);

    /**
     * @brief Retrieves a single calendar event as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The calendar event at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::calendar_event> get_calendar_event_at_version(const std::string& id,
                                                                        std::uint32_t version);

    /**
     * @brief Retrieves a single calendar event by its primary key.
     *
     * @return The calendar event if found, std::nullopt otherwise.
     */
    std::optional<domain::calendar_event> get_calendar_event(const std::string& id);

    /**
     * @brief Retrieves a batch of calendar events by primary key.
     */
    std::vector<domain::calendar_event> get_calendar_events(const std::vector<std::string>& ids);

    /**
     * @brief Saves a calendar event (creates or updates).
     *
     * @param calendar_event The calendar event to save.
     * @throws std::exception on failure.
     */
    void save_calendar_event(const domain::calendar_event& calendar_event);

    /**
     * @brief Saves a batch of calendar events.
     *
     * @param calendar_events The calendar events to save.
     * @throws std::exception on failure.
     */
    void save_calendar_events(const std::vector<domain::calendar_event>& calendar_events);

    /**
     * @brief Deletes a calendar event by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_calendar_event(const std::string& id);

    /**
     * @brief Deletes calendar events by their primary keys.
     */
    void delete_calendar_events(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a calendar event.
     */
    std::vector<domain::calendar_event> get_calendar_event_history(const std::string& id);

private:
    context ctx_;
    repository::calendar_event_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::calendar_event_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::calendar_event& out);
};

}

#endif
