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
#ifndef ORES_TRADING_CORE_SERVICE_INSTRUMENT_OPTION_PREMIUM_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_INSTRUMENT_OPTION_PREMIUM_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/instrument_option_premium.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/instrument_option_premium_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing instrument option premiums.
 *
 * Provides a higher-level interface for instrument option premium operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT instrument_option_premium_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.instrument_option_premium_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a instrument_option_premium_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit instrument_option_premium_service(context ctx);

    /**
     * @brief Lists instrument option premiums with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of instrument option premiums for the requested page.
     */
    std::vector<domain::instrument_option_premium> list_option_premiums(std::uint32_t offset,
                                                                        std::uint32_t limit);

    /**
     * @brief Gets the total count of active instrument option premiums.
     *
     * @return Total number of active instrument option premiums.
     */
    std::uint32_t count_option_premiums();


    /**
     * @brief Retrieves a single instrument option premium as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The instrument option premium at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::instrument_option_premium>
    get_option_premium_at_version(const std::string& instrument_id,
                                  const std::string& sequence_number,
                                  std::uint32_t version);

    /**
     * @brief Retrieves a single instrument option premium by its primary key.
     *
     * @return The instrument option premium if found, std::nullopt otherwise.
     */
    std::optional<domain::instrument_option_premium>
    get_option_premium(const std::string& instrument_id, const std::string& sequence_number);

    /**
     * @brief Saves a instrument option premium (creates or updates).
     *
     * @param option_premium The instrument option premium to save.
     * @throws std::exception on failure.
     */
    void save_option_premium(const domain::instrument_option_premium& option_premium);

    /**
     * @brief Saves a batch of instrument option premiums.
     *
     * @param option_premiums The instrument option premiums to save.
     * @throws std::exception on failure.
     */
    void
    save_option_premiums(const std::vector<domain::instrument_option_premium>& option_premiums);

    /**
     * @brief Deletes a instrument option premium by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_option_premium(const std::string& instrument_id,
                               const std::string& sequence_number);

    /**
     * @brief Deletes instrument option premiums by their primary keys.
     */
    void delete_option_premiums(const std::vector<std::string>& instrument_ids,
                                const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Retrieves all historical versions of a instrument option premium.
     */
    std::vector<domain::instrument_option_premium>
    get_option_premium_history(const std::string& instrument_id,
                               const std::string& sequence_number);

private:
    context ctx_;
    repository::instrument_option_premium_repository repo_;
};

}

#endif
