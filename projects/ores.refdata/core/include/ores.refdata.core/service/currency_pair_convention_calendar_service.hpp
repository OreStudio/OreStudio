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
#ifndef ORES_REFDATA_SERVICE_CURRENCY_PAIR_CONVENTION_CALENDAR_SERVICE_HPP
#define ORES_REFDATA_SERVICE_CURRENCY_PAIR_CONVENTION_CALENDAR_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/currency_pair_convention_calendar.hpp"
#include "ores.refdata.core/repository/currency_pair_convention_calendar_repository.hpp"
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing currency pair convention calendars.
 *
 * This service provides functionality for:
 * - Managing currency pair convention calendars (CRUD operations)
 */
class currency_pair_convention_calendar_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.currency_pair_convention_calendar_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a currency_pair_convention_calendar_service with required repositories.
     *
     * @param ctx The database context.
     */
    explicit currency_pair_convention_calendar_service(context ctx);

    /**
     * @brief Lists all currency pair convention calendars.
     */
    std::vector<domain::currency_pair_convention_calendar> list_pair_convention_calendars();

    /**
     * @brief Lists currency pair convention calendars with pagination.
     */
    std::vector<domain::currency_pair_convention_calendar>
    list_pair_convention_calendars(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active currency pair convention calendars.
     */
    std::uint32_t get_total_pair_convention_calendar_count();

    /**
     * @brief Lists currency pair convention calendars for a specific pair.
     *
     * @param pair_code The pair to filter by
     */
    std::vector<domain::currency_pair_convention_calendar>
    list_pair_convention_calendars_by_pair(const std::string& pair_code);

    /**
     * @brief Lists currency pair convention calendars for a specific pair, with pagination.
     */
    std::vector<domain::currency_pair_convention_calendar> list_pair_convention_calendars_by_pair(
        const std::string& pair_code, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active currency pair convention calendars filtered by
     * pair_code.
     */
    std::uint32_t get_total_pair_convention_calendar_count_by_pair(const std::string& pair_code);

    /**
     * @brief Gets the total count of active currency pair convention calendars filtered by
     * calendar_code.
     */
    std::uint32_t
    get_total_pair_convention_calendar_count_by_calendar(const std::string& calendar_code);
    /**
     * @brief Saves a currency pair convention calendar (creates or updates).
     *
     * @param pair_convention_calendar The currency pair convention calendar to save
     */
    void save_pair_convention_calendar(
        const domain::currency_pair_convention_calendar& pair_convention_calendar);

    /**
     * @brief Removes a currency pair convention calendar.
     *
     * @param pair_code The pair
     * @param calendar_code The calendar
     */
    void remove_pair_convention_calendar(const std::string& pair_code,
                                         const std::string& calendar_code);


private:
    repository::currency_pair_convention_calendar_repository repo_;
};

}

#endif
