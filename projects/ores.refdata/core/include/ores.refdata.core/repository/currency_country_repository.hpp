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
 * Template: cpp_domain_type_repository.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REFDATA_CORE_REPOSITORY_CURRENCY_COUNTRY_REPOSITORY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_CURRENCY_COUNTRY_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/currency_country.hpp"
#include "ores.refdata.core/export.hpp"
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::refdata::repository {

/**
 * @brief Reads and writes currency countries to data storage.
 */
class ORES_REFDATA_CORE_EXPORT currency_country_repository {
private:
    inline static std::string_view logger_name =
        "ores.refdata.repository.currency_country_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit currency_country_repository(context ctx);

    std::string sql();

    void write(const domain::currency_country& currency_country);
    void write(const std::vector<domain::currency_country>& currency_countries);

    std::vector<domain::currency_country> read_latest();
    std::vector<domain::currency_country> read_latest(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active currency countries.
     */
    std::uint32_t get_total_currency_country_count();
    std::vector<domain::currency_country>
    read_latest_by_currency(const std::string& currency_iso_code);
    /**
     * @brief Reads latest currency countries filtered by currency_iso_code, with pagination.
     */
    std::vector<domain::currency_country> read_latest_by_currency(
        const std::string& currency_iso_code, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active currency countries filtered by currency_iso_code.
     */
    std::uint32_t
    get_total_currency_country_count_by_currency(const std::string& currency_iso_code);

    std::vector<domain::currency_country>
    read_latest_by_country(const std::string& country_alpha2_code);

    /**
     * @brief Gets the total count of active currency countries filtered by country_alpha2_code.
     */
    std::uint32_t
    get_total_currency_country_count_by_country(const std::string& country_alpha2_code);

    void remove(const std::string& currency_iso_code, const std::string& country_alpha2_code);
    void remove_by_currency(const std::string& currency_iso_code);

private:
    context ctx_;
};

}

#endif
