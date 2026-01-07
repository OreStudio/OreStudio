/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_RISK_SERVICE_COUNTRY_SERVICE_HPP
#define ORES_RISK_SERVICE_COUNTRY_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include "ores.database/domain/context.hpp"
#include "ores.risk/domain/country.hpp"
#include "ores.risk/repository/country_repository.hpp"
#include "ores.telemetry/log/make_logger.hpp"

namespace ores::risk::service {

/**
 * @brief Service for managing countries.
 *
 * Provides a higher-level interface for country operations, wrapping
 * the underlying repository.
 */
class country_service {
private:
    inline static std::string_view logger_name =
        "ores.risk.service.country_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a country_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit country_service(context ctx);

    /**
     * @brief Lists countries with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of countries for the requested page.
     */
    std::vector<domain::country> list_countries(std::uint32_t offset,
                                                 std::uint32_t limit);

    /**
     * @brief Gets the total count of active countries.
     *
     * @return Total number of active countries.
     */
    std::uint32_t count_countries();

    /**
     * @brief Saves a country (creates or updates).
     *
     * @param country The country to save.
     * @throws std::exception on failure.
     */
    void save_country(const domain::country& country);

    /**
     * @brief Deletes a country by its alpha-2 code.
     *
     * @param alpha2_code The alpha-2 code of the country to delete.
     * @throws std::exception on failure.
     */
    void delete_country(const std::string& alpha2_code);

    /**
     * @brief Retrieves a single country by its alpha-2 code.
     *
     * @param alpha2_code The alpha-2 code of the country.
     * @return The country if found, std::nullopt otherwise.
     */
    std::optional<domain::country> get_country(const std::string& alpha2_code);

    /**
     * @brief Retrieves all historical versions of a country.
     *
     * @param alpha2_code The alpha-2 code of the country.
     * @return Vector of all historical versions of the country.
     */
    std::vector<domain::country> get_country_history(const std::string& alpha2_code);

private:
    context ctx_;
    repository::country_repository repo_;
};

}

#endif
