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
#ifndef ORES_REFDATA_SERVICE_PARTY_IDENTIFIER_SERVICE_HPP
#define ORES_REFDATA_SERVICE_PARTY_IDENTIFIER_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.refdata/domain/party_identifier.hpp"
#include "ores.refdata/repository/party_identifier_repository.hpp"

namespace ores::refdata::service {

/**
 * @brief Service for managing party identifiers.
 *
 * This service provides functionality for:
 * - Managing party identifiers (CRUD operations)
 */
class party_identifier_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.party_identifier_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a party_identifier_service with required repositories.
     *
     * @param ctx The database context.
     */
    explicit party_identifier_service(context ctx);

    /**
     * @brief Lists all party identifiers.
     */
    std::vector<domain::party_identifier> list_party_identifiers();

    /**
     * @brief Lists party identifiers for a specific party.
     */
    std::vector<domain::party_identifier>
    list_party_identifiers_by_party(const boost::uuids::uuid& party_id);

    /**
     * @brief Finds a party identifier by its ID.
     */
    std::optional<domain::party_identifier>
    find_party_identifier(const boost::uuids::uuid& id);

    /**
     * @brief Finds a party identifier by its code.
     */
    std::optional<domain::party_identifier>
    find_party_identifier_by_code(const std::string& code);

    /**
     * @brief Saves a party identifier (creates or updates).
     *
     * @param party_identifier The party identifier to save
     */
    void save_party_identifier(const domain::party_identifier& party_identifier);

    /**
     * @brief Removes a party identifier.
     *
     * @param id The ID of the party identifier to remove
     */
    void remove_party_identifier(const boost::uuids::uuid& id);

    /**
     * @brief Gets the version history for a party identifier.
     *
     * @param id The party identifier ID
     * @return Vector of all versions, newest first
     */
    std::vector<domain::party_identifier>
    get_party_identifier_history(const boost::uuids::uuid& id);

private:
    repository::party_identifier_repository repo_;
};

}

#endif
