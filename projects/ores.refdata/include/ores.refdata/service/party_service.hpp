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
#ifndef ORES_REFDATA_SERVICE_PARTY_SERVICE_HPP
#define ORES_REFDATA_SERVICE_PARTY_SERVICE_HPP

#include <string>
#include <vector>
#include <cstdint>
#include <optional>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.refdata/domain/party.hpp"
#include "ores.refdata/repository/party_repository.hpp"

namespace ores::refdata::service {

/**
 * @brief Service for managing parties.
 *
 * This service provides functionality for:
 * - Managing parties (CRUD operations)
 */
class party_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.party_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a party_service with required repositories.
     *
     * @param ctx The database context.
     */
    explicit party_service(context ctx);

    /**
     * @brief Lists all parties.
     */
    std::vector<domain::party> list_parties();

    /**
     * @brief Lists parties with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of parties for the requested page.
     */
    std::vector<domain::party> list_parties(std::uint32_t offset,
                                             std::uint32_t limit);

    /**
     * @brief Gets the total count of active parties.
     *
     * @return Total number of parties.
     */
    std::uint32_t count_parties();

    /**
     * @brief Finds a party by its ID.
     */
    std::optional<domain::party>
    find_party(const boost::uuids::uuid& id);

    /**
     * @brief Finds a party by its code.
     */
    std::optional<domain::party>
    find_party_by_code(const std::string& code);

    /**
     * @brief Saves a party (creates or updates).
     *
     * @param party The party to save
     */
    void save_party(const domain::party& party);

    /**
     * @brief Saves multiple parties (creates or updates).
     *
     * @param parties The parties to save
     */
    void save_parties(const std::vector<domain::party>& parties);

    /**
     * @brief Removes a party.
     *
     * @param id The ID of the party to remove
     */
    void remove_party(const boost::uuids::uuid& id);

    /**
     * @brief Gets the version history for a party.
     *
     * @param id The party ID
     * @return Vector of all versions, newest first
     */
    std::vector<domain::party>
    get_party_history(const boost::uuids::uuid& id);

private:
    repository::party_repository repo_;
};

}

#endif
