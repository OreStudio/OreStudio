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
#ifndef ORES_REFDATA_SERVICE_PARTY_COUNTRY_SERVICE_HPP
#define ORES_REFDATA_SERVICE_PARTY_COUNTRY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/party_country.hpp"
#include "ores.refdata.core/repository/party_country_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing party countries.
 *
 * This service provides functionality for:
 * - Managing party countries (CRUD operations)
 */
class party_country_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.party_country_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a party_country_service with required repositories.
     *
     * @param ctx The database context.
     */
    explicit party_country_service(context ctx);

    /**
     * @brief Lists all party countries.
     */
    std::vector<domain::party_country> list_party_countries();

    /**
     * @brief Lists party countries with pagination.
     */
    std::vector<domain::party_country> list_party_countries(std::uint32_t offset,
                                                            std::uint32_t limit);

    /**
     * @brief Gets the total count of active party countries.
     */
    std::uint32_t get_total_party_country_count();

    /**
     * @brief Lists party countries for a specific party.
     *
     * @param party_id The party to filter by
     */
    std::vector<domain::party_country>
    list_party_countries_by_party(const boost::uuids::uuid& party_id);

    /**
     * @brief Lists party countries for a specific party, with pagination.
     */
    std::vector<domain::party_country> list_party_countries_by_party(
        const boost::uuids::uuid& party_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active party countries filtered by party_id.
     */
    std::uint32_t get_total_party_country_count_by_party(const boost::uuids::uuid& party_id);

    /**
     * @brief Gets the total count of active party countries filtered by country_alpha2_code.
     */
    std::uint32_t get_total_party_country_count_by_country(const std::string& country_alpha2_code);
    /**
     * @brief Saves a party country (creates or updates).
     *
     * @param party_country The party country to save
     */
    void save_party_country(const domain::party_country& party_country);

    /**
     * @brief Removes a party country.
     *
     * @param party_id The party
     * @param country_alpha2_code The country
     */
    void remove_party_country(const boost::uuids::uuid& party_id,
                              const std::string& country_alpha2_code);


private:
    context ctx_;
    repository::party_country_repository repo_;
};

}

#endif
