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
#ifndef ORES_REFDATA_SERVICE_CURRENCY_COUNTRY_SERVICE_HPP
#define ORES_REFDATA_SERVICE_CURRENCY_COUNTRY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/currency_country.hpp"
#include "ores.refdata.core/repository/currency_country_repository.hpp"
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing currency countries.
 *
 * This service provides functionality for:
 * - Managing currency countries (CRUD operations)
 */
class currency_country_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.currency_country_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a currency_country_service with required repositories.
     *
     * @param ctx The database context.
     */
    explicit currency_country_service(context ctx);

    /**
     * @brief Lists all currency countries.
     */
    std::vector<domain::currency_country> list_currency_countries();

    /**
     * @brief Lists currency countries with pagination.
     */
    std::vector<domain::currency_country> list_currency_countries(std::uint32_t offset,
                                                                  std::uint32_t limit);

    /**
     * @brief Gets the total count of active currency countries.
     */
    std::uint32_t get_total_currency_country_count();

    /**
     * @brief Lists currency countries for a specific currency.
     *
     * @param currency_iso_code The currency to filter by
     */
    std::vector<domain::currency_country>
    list_currency_countries_by_currency(const std::string& currency_iso_code);

    /**
     * @brief Lists currency countries for a specific currency, with pagination.
     */
    std::vector<domain::currency_country> list_currency_countries_by_currency(
        const std::string& currency_iso_code, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active currency countries filtered by currency_iso_code.
     */
    std::uint32_t
    get_total_currency_country_count_by_currency(const std::string& currency_iso_code);

    /**
     * @brief Gets the total count of active currency countries filtered by country_alpha2_code.
     */
    std::uint32_t
    get_total_currency_country_count_by_country(const std::string& country_alpha2_code);
    /**
     * @brief Saves a currency country (creates or updates).
     *
     * @param currency_country The currency country to save
     */
    void save_currency_country(const domain::currency_country& currency_country);

    /**
     * @brief Removes a currency country.
     *
     * @param currency_iso_code The currency
     * @param country_alpha2_code The country
     */
    void remove_currency_country(const std::string& currency_iso_code,
                                 const std::string& country_alpha2_code);


private:
    context ctx_;
    repository::currency_country_repository repo_;
};

}

#endif
