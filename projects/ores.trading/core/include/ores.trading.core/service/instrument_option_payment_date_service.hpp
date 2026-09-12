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
#ifndef ORES_TRADING_CORE_SERVICE_INSTRUMENT_OPTION_PAYMENT_DATE_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_INSTRUMENT_OPTION_PAYMENT_DATE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/instrument_option_payment_date.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/instrument_option_payment_date_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing instrument option payment dates.
 *
 * Provides a higher-level interface for instrument option payment date operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT instrument_option_payment_date_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.instrument_option_payment_date_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a instrument_option_payment_date_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit instrument_option_payment_date_service(context ctx);

    /**
     * @brief Lists instrument option payment dates with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of instrument option payment dates for the requested page.
     */
    std::vector<domain::instrument_option_payment_date>
    list_option_payment_dates(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active instrument option payment dates.
     *
     * @return Total number of active instrument option payment dates.
     */
    std::uint32_t count_option_payment_dates();


    /**
     * @brief Retrieves a single instrument option payment date as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The instrument option payment date at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::instrument_option_payment_date>
    get_option_payment_date_at_version(const std::string& instrument_id,
                                       const std::string& sequence_number,
                                       std::uint32_t version);

    /**
     * @brief Retrieves a single instrument option payment date by its primary key.
     *
     * @return The instrument option payment date if found, std::nullopt otherwise.
     */
    std::optional<domain::instrument_option_payment_date>
    get_option_payment_date(const std::string& instrument_id, const std::string& sequence_number);

    /**
     * @brief Saves a instrument option payment date (creates or updates).
     *
     * @param option_payment_date The instrument option payment date to save.
     * @throws std::exception on failure.
     */
    void
    save_option_payment_date(const domain::instrument_option_payment_date& option_payment_date);

    /**
     * @brief Saves a batch of instrument option payment dates.
     *
     * @param option_payment_dates The instrument option payment dates to save.
     * @throws std::exception on failure.
     */
    void save_option_payment_dates(
        const std::vector<domain::instrument_option_payment_date>& option_payment_dates);

    /**
     * @brief Deletes a instrument option payment date by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_option_payment_date(const std::string& instrument_id,
                                    const std::string& sequence_number);

    /**
     * @brief Deletes instrument option payment dates by their primary keys.
     */
    void delete_option_payment_dates(const std::vector<std::string>& instrument_ids,
                                     const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Retrieves all historical versions of a instrument option payment date.
     */
    std::vector<domain::instrument_option_payment_date>
    get_option_payment_date_history(const std::string& instrument_id,
                                    const std::string& sequence_number);

private:
    context ctx_;
    repository::instrument_option_payment_date_repository repo_;
};

}

#endif
