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
#ifndef ORES_REFDATA_CORE_SERVICE_TENOR_SCHEDULE_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_TENOR_SCHEDULE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/tenor_schedule.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/tenor_schedule_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing tenor schedules.
 *
 * Provides a higher-level interface for tenor schedule operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT tenor_schedule_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.tenor_schedule_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a tenor_schedule_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit tenor_schedule_service(context ctx);

    /**
     * @brief Lists tenor schedules with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of tenor schedules for the requested page.
     */
    std::vector<domain::tenor_schedule> list_schedules(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active tenor schedules.
     *
     * @return Total number of active tenor schedules.
     */
    std::uint32_t count_schedules();


    /**
     * @brief Lists tenor schedules filtered by calendar_code, with pagination.
     *
     * @param calendar_code The calendar_code to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching tenor schedules for the requested page.
     */
    std::vector<domain::tenor_schedule> list_schedules_by_calendar_code(
        const std::string& calendar_code, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active tenor schedules filtered by calendar_code.
     *
     * @param calendar_code The calendar_code to filter by.
     * @return Total number of matching tenor schedules.
     */
    std::uint32_t count_schedules_by_calendar_code(const std::string& calendar_code);

    /**
     * @brief Lists tenor schedules filtered by diary_entry_type, with pagination.
     *
     * @param diary_entry_type The diary_entry_type to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching tenor schedules for the requested page.
     */
    std::vector<domain::tenor_schedule> list_schedules_by_diary_entry_type(
        const std::string& diary_entry_type, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active tenor schedules filtered by diary_entry_type.
     *
     * @param diary_entry_type The diary_entry_type to filter by.
     * @return Total number of matching tenor schedules.
     */
    std::uint32_t count_schedules_by_diary_entry_type(const std::string& diary_entry_type);

    /**
     * @brief Retrieves a single tenor schedule as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The tenor schedule at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::tenor_schedule> get_schedule_at_version(const std::string& code,
                                                                  std::uint32_t version);

    /**
     * @brief Retrieves a single tenor schedule by its primary key.
     *
     * @return The tenor schedule if found, std::nullopt otherwise.
     */
    std::optional<domain::tenor_schedule> get_schedule(const std::string& code);

    /**
     * @brief Saves a tenor schedule (creates or updates).
     *
     * @param schedule The tenor schedule to save.
     * @throws std::exception on failure.
     */
    void save_schedule(const domain::tenor_schedule& schedule);

    /**
     * @brief Saves a batch of tenor schedules.
     *
     * @param schedules The tenor schedules to save.
     * @throws std::exception on failure.
     */
    void save_schedules(const std::vector<domain::tenor_schedule>& schedules);

    /**
     * @brief Deletes a tenor schedule by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_schedule(const std::string& code);

    /**
     * @brief Deletes tenor schedules by their primary keys.
     */
    void delete_schedules(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a tenor schedule.
     */
    std::vector<domain::tenor_schedule> get_schedule_history(const std::string& code);

private:
    context ctx_;
    repository::tenor_schedule_repository repo_;
};

}

#endif
