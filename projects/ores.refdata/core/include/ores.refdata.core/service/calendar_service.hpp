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
#ifndef ORES_REFDATA_CORE_SERVICE_CALENDAR_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CALENDAR_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/calendar.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/calendar_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing calendars.
 *
 * Provides a higher-level interface for calendar operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT calendar_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.calendar_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a calendar_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit calendar_service(context ctx);

    /**
     * @brief Lists calendars with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of calendars for the requested page.
     */
    std::vector<domain::calendar> list_calendars(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active calendars.
     *
     * @return Total number of active calendars.
     */
    std::uint32_t count_calendars();

    /**
     * @brief Retrieves a single calendar as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param code The code of the calendar.
     * @param version The version to fetch.
     * @return The calendar at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::calendar> get_calendar_at_version(const std::string& code,
                                                            std::uint32_t version);

    /**
     * @brief Retrieves a single calendar by its code.
     *
     * @param code The code of the calendar.
     * @return The calendar if found, std::nullopt otherwise.
     */
    std::optional<domain::calendar> get_calendar(const std::string& code);

    /**
     * @brief Saves a calendar (creates or updates).
     *
     * @param calendar The calendar to save.
     * @throws std::exception on failure.
     */
    void save_calendar(const domain::calendar& calendar);

    /**
     * @brief Saves a batch of calendars.
     *
     * @param calendars The calendars to save.
     * @throws std::exception on failure.
     */
    void save_calendars(const std::vector<domain::calendar>& calendars);

    /**
     * @brief Deletes a calendar by its code.
     *
     * @param code The code of the calendar to delete.
     * @throws std::exception on failure.
     */
    void delete_calendar(const std::string& code);

    /**
     * @brief Deletes calendars by their codes.
     */
    void delete_calendars(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a calendar.
     */
    std::vector<domain::calendar> get_calendar_history(const std::string& code);

private:
    context ctx_;
    repository::calendar_repository repo_;
};

}

#endif
