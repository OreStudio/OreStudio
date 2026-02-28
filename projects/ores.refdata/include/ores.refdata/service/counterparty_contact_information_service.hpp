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
#ifndef ORES_REFDATA_SERVICE_COUNTERPARTY_CONTACT_INFORMATION_SERVICE_HPP
#define ORES_REFDATA_SERVICE_COUNTERPARTY_CONTACT_INFORMATION_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.refdata/domain/counterparty_contact_information.hpp"
#include "ores.refdata/repository/counterparty_contact_information_repository.hpp"

namespace ores::refdata::service {

/**
 * @brief Service for managing counterparty contact informations.
 *
 * This service provides functionality for:
 * - Managing counterparty contact informations (CRUD operations)
 */
class counterparty_contact_information_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.counterparty_contact_information_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a counterparty_contact_information_service with required repositories.
     *
     * @param ctx The database context.
     */
    explicit counterparty_contact_information_service(context ctx);

    /**
     * @brief Lists all counterparty contact informations.
     */
    std::vector<domain::counterparty_contact_information> list_counterparty_contact_informations();

    /**
     * @brief Lists counterparty contact informations for a specific counterparty.
     */
    std::vector<domain::counterparty_contact_information>
    list_counterparty_contact_informations_by_counterparty(const boost::uuids::uuid& counterparty_id);

    /**
     * @brief Finds a counterparty contact information by its ID.
     */
    std::optional<domain::counterparty_contact_information>
    find_counterparty_contact_information(const boost::uuids::uuid& id);

    /**
     * @brief Finds a counterparty contact information by its code.
     */
    std::optional<domain::counterparty_contact_information>
    find_counterparty_contact_information_by_code(const std::string& code);

    /**
     * @brief Saves a counterparty contact information (creates or updates).
     *
     * @param counterparty_contact_information The counterparty contact information to save
     */
    void save_counterparty_contact_information(const domain::counterparty_contact_information& counterparty_contact_information);

    /**
     * @brief Saves multiple counterparty contact informations (creates or updates).
     *
     * @param counterparty_contact_informations The counterparty contact informations to save
     */
    void save_counterparty_contact_informations(const std::vector<domain::counterparty_contact_information>& counterparty_contact_informations);

    /**
     * @brief Removes a counterparty contact information.
     *
     * @param id The ID of the counterparty contact information to remove
     */
    void remove_counterparty_contact_information(const boost::uuids::uuid& id);

    /**
     * @brief Gets the version history for a counterparty contact information.
     *
     * @param id The counterparty contact information ID
     * @return Vector of all versions, newest first
     */
    std::vector<domain::counterparty_contact_information>
    get_counterparty_contact_information_history(const boost::uuids::uuid& id);

private:
    repository::counterparty_contact_information_repository repo_;
};

}

#endif
