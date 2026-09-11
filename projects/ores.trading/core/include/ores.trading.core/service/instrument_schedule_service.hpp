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
#ifndef ORES_TRADING_CORE_SERVICE_INSTRUMENT_SCHEDULE_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_INSTRUMENT_SCHEDULE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/instrument_schedule.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/instrument_schedule_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing instrument schedules.
 *
 * Provides a higher-level interface for instrument schedule operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT instrument_schedule_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.instrument_schedule_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a instrument_schedule_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit instrument_schedule_service(context ctx);

    /**
     * @brief Lists instrument schedules with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of instrument schedules for the requested page.
     */
    std::vector<domain::instrument_schedule> list_instrument_schedules(std::uint32_t offset,
                                                                       std::uint32_t limit);

    /**
     * @brief Gets the total count of active instrument schedules.
     *
     * @return Total number of active instrument schedules.
     */
    std::uint32_t count_instrument_schedules();


    /**
     * @brief Retrieves a single instrument schedule as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The instrument schedule at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::instrument_schedule>
    get_instrument_schedule_at_version(const std::string& instrument_id,
                                       const std::string& owner_role,
                                       const std::string& owner_number,
                                       const std::string& schedule_role,
                                       const std::string& sequence_number,
                                       std::uint32_t version);

    /**
     * @brief Retrieves a single instrument schedule by its primary key.
     *
     * @return The instrument schedule if found, std::nullopt otherwise.
     */
    std::optional<domain::instrument_schedule>
    get_instrument_schedule(const std::string& instrument_id,
                            const std::string& owner_role,
                            const std::string& owner_number,
                            const std::string& schedule_role,
                            const std::string& sequence_number);

    /**
     * @brief Saves a instrument schedule (creates or updates).
     *
     * @param instrument_schedule The instrument schedule to save.
     * @throws std::exception on failure.
     */
    void save_instrument_schedule(const domain::instrument_schedule& instrument_schedule);

    /**
     * @brief Saves a batch of instrument schedules.
     *
     * @param instrument_schedules The instrument schedules to save.
     * @throws std::exception on failure.
     */
    void
    save_instrument_schedules(const std::vector<domain::instrument_schedule>& instrument_schedules);

    /**
     * @brief Deletes a instrument schedule by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_instrument_schedule(const std::string& instrument_id,
                                    const std::string& owner_role,
                                    const std::string& owner_number,
                                    const std::string& schedule_role,
                                    const std::string& sequence_number);

    /**
     * @brief Deletes instrument schedules by their primary keys.
     */
    void delete_instrument_schedules(const std::vector<std::string>& instrument_ids,
                                     const std::vector<std::string>& owner_roles,
                                     const std::vector<std::string>& owner_numbers,
                                     const std::vector<std::string>& schedule_roles,
                                     const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Retrieves all historical versions of a instrument schedule.
     */
    std::vector<domain::instrument_schedule>
    get_instrument_schedule_history(const std::string& instrument_id,
                                    const std::string& owner_role,
                                    const std::string& owner_number,
                                    const std::string& schedule_role,
                                    const std::string& sequence_number);

private:
    context ctx_;
    repository::instrument_schedule_repository repo_;
};

}

#endif
