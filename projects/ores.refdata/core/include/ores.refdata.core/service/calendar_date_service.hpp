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
#ifndef ORES_REFDATA_SERVICE_CALENDAR_DATE_SERVICE_HPP
#define ORES_REFDATA_SERVICE_CALENDAR_DATE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/calendar_date.hpp"
#include "ores.refdata.core/repository/calendar_date_repository.hpp"
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing calendar dates.
 *
 * This service provides functionality for:
 * - Managing calendar dates (CRUD operations)
 */
class calendar_date_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.calendar_date_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a calendar_date_service with required repositories.
     *
     * @param ctx The database context.
     */
    explicit calendar_date_service(context ctx);

    /**
     * @brief Lists all calendar dates.
     */
    std::vector<domain::calendar_date> list_calendar_dates();

    /**
     * @brief Lists calendar dates for a specific calendar.
     *
     * @param calendar_code The calendar to filter by
     */
    std::vector<domain::calendar_date>
    list_calendar_dates_by_calendar(const std::string& calendar_code);

    /**
     * @brief Lists calendar dates for a specific calendar, with pagination.
     */
    std::vector<domain::calendar_date> list_calendar_dates_by_calendar(
        const std::string& calendar_code, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active calendar dates filtered by calendar_code.
     */
    std::uint32_t get_total_calendar_date_count_by_calendar(const std::string& calendar_code);

    /**
     * @brief Saves a calendar date (creates or updates).
     *
     * @param calendar_date The calendar date to save
     */
    void save_calendar_date(const domain::calendar_date& calendar_date);

    /**
     * @brief Removes a calendar date.
     *
     * @param calendar_code The calendar
     * @param date The date
     */
    void remove_calendar_date(const std::string& calendar_code, const std::string& date);

private:
    repository::calendar_date_repository repo_;
};

}

#endif
