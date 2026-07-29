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
#ifndef ORES_REFDATA_CORE_SERVICE_CALENDAR_EXCEPTION_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CALENDAR_EXCEPTION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/calendar_exception.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/calendar_exception_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing calendar exceptions.
 *
 * Provides a higher-level interface for calendar exception operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT calendar_exception_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.calendar_exception_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a calendar_exception_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit calendar_exception_service(context ctx);

    /**
     * @brief Lists calendar exceptions with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of calendar exceptions for the requested page.
     */
    std::vector<domain::calendar_exception> list_calendar_exceptions(std::uint32_t offset,
                                                                     std::uint32_t limit);

    /**
     * @brief Gets the total count of active calendar exceptions.
     *
     * @return Total number of active calendar exceptions.
     */
    std::uint32_t count_calendar_exceptions();


    /**
     * @brief Lists calendar exceptions filtered by calendar_code, with pagination.
     *
     * @param calendar_code The calendar_code to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching calendar exceptions for the requested page.
     */
    std::vector<domain::calendar_exception> list_calendar_exceptions_by_calendar_code(
        const std::string& calendar_code, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active calendar exceptions filtered by calendar_code.
     *
     * @param calendar_code The calendar_code to filter by.
     * @return Total number of matching calendar exceptions.
     */
    std::uint32_t count_calendar_exceptions_by_calendar_code(const std::string& calendar_code);

    /**
     * @brief Lists calendar exceptions filtered by calendar_code that were live at
     * any point during a parent version's own [valid_from, valid_to) window.
     * See the "Temporal composite entity versioning" architecture doc.
     *
     * @param calendar_code The calendar_code to filter by.
     * @param valid_from_bound The parent version's own valid_from.
     * @param valid_to_bound The parent version's own valid_to.
     * @return Vector of matching calendar exceptions.
     */
    std::vector<domain::calendar_exception> list_calendar_exceptions_by_calendar_code_as_of(
        const std::string& calendar_code,
        std::chrono::system_clock::time_point valid_from_bound,
        std::chrono::system_clock::time_point valid_to_bound);
    /**
     * @brief Retrieves a single calendar exception as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The calendar exception at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::calendar_exception>
    get_calendar_exception_at_version(const std::string& id, std::uint32_t version);

    /**
     * @brief Retrieves a single calendar exception by its primary key.
     *
     * @return The calendar exception if found, std::nullopt otherwise.
     */
    std::optional<domain::calendar_exception> get_calendar_exception(const std::string& id);

    /**
     * @brief Saves a calendar exception (creates or updates).
     *
     * @param calendar_exception The calendar exception to save.
     * @throws std::exception on failure.
     */
    void save_calendar_exception(const domain::calendar_exception& calendar_exception);

    /**
     * @brief Saves a batch of calendar exceptions.
     *
     * @param calendar_exceptions The calendar exceptions to save.
     * @throws std::exception on failure.
     */
    void
    save_calendar_exceptions(const std::vector<domain::calendar_exception>& calendar_exceptions);

    /**
     * @brief Deletes a calendar exception by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_calendar_exception(const std::string& id);

    /**
     * @brief Deletes calendar exceptions by their primary keys.
     */
    void delete_calendar_exceptions(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a calendar exception.
     */
    std::vector<domain::calendar_exception> get_calendar_exception_history(const std::string& id);

private:
    context ctx_;
    repository::calendar_exception_repository repo_;
};

}

#endif
