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
#ifndef ORES_REFDATA_CORE_REPOSITORY_TENOR_SCHEDULE_REPOSITORY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_TENOR_SCHEDULE_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/tenor_schedule.hpp"
#include "ores.refdata.core/export.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::refdata::repository {

/**
 * @brief Reads and writes tenor schedules to data storage.
 */
class ORES_REFDATA_CORE_EXPORT tenor_schedule_repository {
private:
    inline static std::string_view logger_name =
        "ores.refdata.repository.tenor_schedule_repository";

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
     * @brief Writes tenor schedules to database.
     */
    /**@{*/
    void write(context ctx, const domain::tenor_schedule& v);
    void write(context ctx, const std::vector<domain::tenor_schedule>& v);
    /**@}*/

    /**
     * @brief Reads latest tenor schedules, possibly filtered by primary key.
     */
    /**@{*/
    std::vector<domain::tenor_schedule> read_latest(context ctx);
    std::vector<domain::tenor_schedule> read_latest(context ctx, const std::string& code);
    /**@}*/

    /**
     * @brief Reads all tenor schedules, possibly filtered by primary key.
     */
    std::vector<domain::tenor_schedule> read_all(context ctx, const std::string& code);

    /**
     * @brief Reads a single tenor schedule as it stood at a specific
     * version — the version's own [valid_from, valid_to) window is returned
     * verbatim, so the caller can compose child entities "as of" the same
     * window. See the "Temporal composite entity versioning" architecture
     * doc.
     * @param ctx Repository context with database connection
     * @param version The version to fetch
     */
    std::optional<domain::tenor_schedule>
    read_at_version(context ctx, const std::string& code, std::uint32_t version);

    /**
     * @brief Reads latest tenor schedules filtered by calendar_code, with pagination.
     * @param ctx Repository context with database connection
     * @param calendar_code The calendar_code to filter by
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     */
    std::vector<domain::tenor_schedule> read_latest_by_calendar_code(
        context ctx, const std::string& calendar_code, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active tenor schedules filtered by calendar_code.
     */
    std::uint32_t get_total_schedule_count_by_calendar_code(context ctx,
                                                            const std::string& calendar_code);

    /**
     * @brief Reads latest tenor schedules filtered by diary_entry_type, with pagination.
     * @param ctx Repository context with database connection
     * @param diary_entry_type The diary_entry_type to filter by
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     */
    std::vector<domain::tenor_schedule>
    read_latest_by_diary_entry_type(context ctx,
                                    const std::string& diary_entry_type,
                                    std::uint32_t offset,
                                    std::uint32_t limit);

    /**
     * @brief Gets the total count of active tenor schedules filtered by diary_entry_type.
     */
    std::uint32_t get_total_schedule_count_by_diary_entry_type(context ctx,
                                                               const std::string& diary_entry_type);

    /**
     * @brief Reads latest tenor schedules with pagination support.
     * @param ctx Repository context with database connection
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     */
    std::vector<domain::tenor_schedule>
    read_latest(context ctx, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active tenor schedules.
     * @param ctx Repository context with database connection
     * @return Total number of active tenor schedules
     */
    std::uint32_t get_total_schedule_count(context ctx);

    /**
     * @brief Deletes a tenor schedule by closing its temporal validity.
     */
    void remove(context ctx, const std::string& code);

    /**
     * @brief Deletes tenor schedules by closing their temporal validity.
     */
    void remove(context ctx, const std::vector<std::string>& codes);
};

}

#endif
