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
#ifndef ORES_REFDATA_CORE_SERVICE_COUNTRY_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_COUNTRY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/country.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/country_repository.hpp"
#include "ores.refdata.core/repository/party_country_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing countries.
 */
class ORES_REFDATA_CORE_EXPORT country_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.country_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit country_service(context ctx);

    std::vector<domain::country> list_countries(std::uint32_t offset, std::uint32_t limit);

    std::uint32_t count_countries();

    std::optional<domain::country> get_country(const std::string& alpha2_code);

    void save_country(const domain::country& country);

    void save_countries(const std::vector<domain::country>& countries);

    void delete_country(const std::string& alpha2_code);

    void delete_countries(const std::vector<std::string>& alpha2_codes);

    std::vector<domain::country> get_country_history(const std::string& alpha2_code);

    /**
     * @brief Lists countries visible to a specific party, with pagination.
     *
     * @param party_id The UUID of the party.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of visible countries for the requested page.
     */
    std::vector<domain::country> list_countries_for_party(const boost::uuids::uuid& party_id,
                                                          std::uint32_t offset,
                                                          std::uint32_t limit);

    /**
     * @brief Gets the total count of countries visible to a specific party.
     *
     * @param party_id The UUID of the party.
     * @return Number of countries the party is permitted to see.
     */
    std::uint32_t count_countries_for_party(const boost::uuids::uuid& party_id);

private:
    context ctx_;
    repository::country_repository repo_;
    repository::party_country_repository junction_repo_;
};

}

#endif
