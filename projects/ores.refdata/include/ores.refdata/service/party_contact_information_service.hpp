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
#ifndef ORES_REFDATA_SERVICE_PARTY_CONTACT_INFORMATION_SERVICE_HPP
#define ORES_REFDATA_SERVICE_PARTY_CONTACT_INFORMATION_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.refdata/domain/party_contact_information.hpp"
#include "ores.refdata/repository/party_contact_information_repository.hpp"

namespace ores::refdata::service {

/**
 * @brief Service for managing party contact informations.
 *
 * This service provides functionality for:
 * - Managing party contact informations (CRUD operations)
 */
class party_contact_information_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.party_contact_information_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a party_contact_information_service with required repositories.
     *
     * @param ctx The database context.
     */
    explicit party_contact_information_service(context ctx);

    /**
     * @brief Lists all party contact informations.
     */
    std::vector<domain::party_contact_information> list_party_contact_informations();

    /**
     * @brief Finds a party contact information by its ID.
     */
    std::optional<domain::party_contact_information>
    find_party_contact_information(const boost::uuids::uuid& id);

    /**
     * @brief Finds a party contact information by its code.
     */
    std::optional<domain::party_contact_information>
    find_party_contact_information_by_code(const std::string& code);

    /**
     * @brief Saves a party contact information (creates or updates).
     *
     * @param party_contact_information The party contact information to save
     */
    void save_party_contact_information(const domain::party_contact_information& party_contact_information);

    /**
     * @brief Removes a party contact information.
     *
     * @param id The ID of the party contact information to remove
     */
    void remove_party_contact_information(const boost::uuids::uuid& id);

    /**
     * @brief Gets the version history for a party contact information.
     *
     * @param id The party contact information ID
     * @return Vector of all versions, newest first
     */
    std::vector<domain::party_contact_information>
    get_party_contact_information_history(const boost::uuids::uuid& id);

private:
    repository::party_contact_information_repository repo_;
};

}

#endif
