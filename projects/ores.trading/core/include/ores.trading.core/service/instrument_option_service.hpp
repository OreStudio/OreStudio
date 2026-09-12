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
#ifndef ORES_TRADING_CORE_SERVICE_INSTRUMENT_OPTION_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_INSTRUMENT_OPTION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/instrument_option.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/instrument_option_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing instrument options.
 *
 * Provides a higher-level interface for instrument option operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT instrument_option_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.instrument_option_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a instrument_option_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit instrument_option_service(context ctx);

    /**
     * @brief Lists instrument options with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of instrument options for the requested page.
     */
    std::vector<domain::instrument_option> list_instrument_options(std::uint32_t offset,
                                                                   std::uint32_t limit);

    /**
     * @brief Gets the total count of active instrument options.
     *
     * @return Total number of active instrument options.
     */
    std::uint32_t count_instrument_options();


    /**
     * @brief Retrieves a single instrument option as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The instrument option at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::instrument_option>
    get_instrument_option_at_version(const std::string& instrument_id, std::uint32_t version);

    /**
     * @brief Retrieves a single instrument option by its primary key.
     *
     * @return The instrument option if found, std::nullopt otherwise.
     */
    std::optional<domain::instrument_option>
    get_instrument_option(const std::string& instrument_id);

    /**
     * @brief Saves a instrument option (creates or updates).
     *
     * @param instrument_option The instrument option to save.
     * @throws std::exception on failure.
     */
    void save_instrument_option(const domain::instrument_option& instrument_option);

    /**
     * @brief Saves a batch of instrument options.
     *
     * @param instrument_options The instrument options to save.
     * @throws std::exception on failure.
     */
    void save_instrument_options(const std::vector<domain::instrument_option>& instrument_options);

    /**
     * @brief Deletes a instrument option by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_instrument_option(const std::string& instrument_id);

    /**
     * @brief Deletes instrument options by their primary keys.
     */
    void delete_instrument_options(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Retrieves all historical versions of a instrument option.
     */
    std::vector<domain::instrument_option>
    get_instrument_option_history(const std::string& instrument_id);

private:
    context ctx_;
    repository::instrument_option_repository repo_;
};

}

#endif
