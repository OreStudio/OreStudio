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
#ifndef ORES_REFDATA_CORE_REPOSITORY_CALENDAR_EVENT_REPOSITORY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_CALENDAR_EVENT_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/calendar_event.hpp"
#include "ores.refdata.core/export.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::refdata::repository {

/**
 * @brief Reads and writes calendar events to data storage.
 */
class ORES_REFDATA_CORE_EXPORT calendar_event_repository {
private:
    inline static std::string_view logger_name =
        "ores.refdata.repository.calendar_event_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Writes calendar events to database.
     */
    /**@{*/
    void write(context ctx, const domain::calendar_event& v);
    void write(context ctx, const std::vector<domain::calendar_event>& v);
    /**@}*/

    /**
     * @brief Reads latest calendar events, possibly filtered by primary key.
     */
    /**@{*/
    std::vector<domain::calendar_event> read_latest(context ctx);
    std::vector<domain::calendar_event> read_latest(context ctx, const std::string& id);
    /**@}*/

    /**
     * @brief Reads all calendar events, possibly filtered by primary key.
     */
    std::vector<domain::calendar_event> read_all(context ctx, const std::string& id);

    /**
     * @brief Reads a single calendar event as it stood at a specific
     * version — the version's own [valid_from, valid_to) window is returned
     * verbatim, so the caller can compose child entities "as of" the same
     * window. See the "Temporal composite entity versioning" architecture
     * doc.
     * @param ctx Repository context with database connection
     * @param version The version to fetch
     */
    std::optional<domain::calendar_event>
    read_at_version(context ctx, const std::string& id, std::uint32_t version);

    /**
     * @brief Reads latest calendar events filtered by calendar_code, with pagination.
     * @param ctx Repository context with database connection
     * @param calendar_code The calendar_code to filter by
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     */
    std::vector<domain::calendar_event> read_latest_by_calendar_code(
        context ctx, const std::string& calendar_code, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active calendar events filtered by calendar_code.
     */
    std::uint32_t get_total_calendar_event_count_by_calendar_code(context ctx,
                                                                  const std::string& calendar_code);

    /**
     * @brief Reads calendar events filtered by calendar_code that were live at
     * any point during [valid_from_bound, valid_to_bound) — i.e. the set of
     * calendar events that compose a parent entity's state as of one of
     * its own historical versions. See the "Temporal composite entity
     * versioning" architecture doc.
     * @param ctx Repository context with database connection
     * @param calendar_code The calendar_code to filter by
     * @param valid_from_bound The parent version's own valid_from
     * @param valid_to_bound The parent version's own valid_to
     */
    std::vector<domain::calendar_event>
    read_by_calendar_code_as_of(context ctx,
                                const std::string& calendar_code,
                                std::chrono::system_clock::time_point valid_from_bound,
                                std::chrono::system_clock::time_point valid_to_bound);
    /**
     * @brief Reads latest calendar events filtered by diary_entry_type, with pagination.
     * @param ctx Repository context with database connection
     * @param diary_entry_type The diary_entry_type to filter by
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     */
    std::vector<domain::calendar_event>
    read_latest_by_diary_entry_type(context ctx,
                                    const std::string& diary_entry_type,
                                    std::uint32_t offset,
                                    std::uint32_t limit);

    /**
     * @brief Gets the total count of active calendar events filtered by diary_entry_type.
     */
    std::uint32_t
    get_total_calendar_event_count_by_diary_entry_type(context ctx,
                                                       const std::string& diary_entry_type);

    /**
     * @brief Reads calendar events filtered by diary_entry_type that were live at
     * any point during [valid_from_bound, valid_to_bound) — i.e. the set of
     * calendar events that compose a parent entity's state as of one of
     * its own historical versions. See the "Temporal composite entity
     * versioning" architecture doc.
     * @param ctx Repository context with database connection
     * @param diary_entry_type The diary_entry_type to filter by
     * @param valid_from_bound The parent version's own valid_from
     * @param valid_to_bound The parent version's own valid_to
     */
    std::vector<domain::calendar_event>
    read_by_diary_entry_type_as_of(context ctx,
                                   const std::string& diary_entry_type,
                                   std::chrono::system_clock::time_point valid_from_bound,
                                   std::chrono::system_clock::time_point valid_to_bound);
    /**
     * @brief Reads latest calendar events with pagination support.
     * @param ctx Repository context with database connection
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     */
    std::vector<domain::calendar_event>
    read_latest(context ctx, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active calendar events.
     * @param ctx Repository context with database connection
     * @return Total number of active calendar events
     */
    std::uint32_t get_total_calendar_event_count(context ctx);

    /**
     * @brief Deletes a calendar event by closing its temporal validity.
     */
    void remove(context ctx, const std::string& id);

    /**
     * @brief Deletes calendar events by closing their temporal validity.
     */
    void remove(context ctx, const std::vector<std::string>& ids);
};

}

#endif
