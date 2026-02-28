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
#ifndef ORES_REFDATA_SERVICE_COUNTERPARTY_IDENTIFIER_SERVICE_HPP
#define ORES_REFDATA_SERVICE_COUNTERPARTY_IDENTIFIER_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.refdata/domain/counterparty_identifier.hpp"
#include "ores.refdata/repository/counterparty_identifier_repository.hpp"

namespace ores::refdata::service {

/**
 * @brief Service for managing counterparty identifiers.
 *
 * This service provides functionality for:
 * - Managing counterparty identifiers (CRUD operations)
 */
class counterparty_identifier_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.counterparty_identifier_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a counterparty_identifier_service with required repositories.
     *
     * @param ctx The database context.
     */
    explicit counterparty_identifier_service(context ctx);

    /**
     * @brief Lists all counterparty identifiers.
     */
    std::vector<domain::counterparty_identifier> list_counterparty_identifiers();

    /**
     * @brief Lists counterparty identifiers for a specific counterparty.
     */
    std::vector<domain::counterparty_identifier>
    list_counterparty_identifiers_by_counterparty(const boost::uuids::uuid& counterparty_id);

    /**
     * @brief Finds a counterparty identifier by its ID.
     */
    std::optional<domain::counterparty_identifier>
    find_counterparty_identifier(const boost::uuids::uuid& id);

    /**
     * @brief Finds a counterparty identifier by its code.
     */
    std::optional<domain::counterparty_identifier>
    find_counterparty_identifier_by_code(const std::string& code);

    /**
     * @brief Saves a counterparty identifier (creates or updates).
     *
     * @param counterparty_identifier The counterparty identifier to save
     */
    void save_counterparty_identifier(const domain::counterparty_identifier& counterparty_identifier);

    /**
     * @brief Saves multiple counterparty identifiers (creates or updates).
     *
     * @param counterparty_identifiers The counterparty identifiers to save
     */
    void save_counterparty_identifiers(const std::vector<domain::counterparty_identifier>& counterparty_identifiers);

    /**
     * @brief Removes a counterparty identifier.
     *
     * @param id The ID of the counterparty identifier to remove
     */
    void remove_counterparty_identifier(const boost::uuids::uuid& id);

    /**
     * @brief Gets the version history for a counterparty identifier.
     *
     * @param id The counterparty identifier ID
     * @return Vector of all versions, newest first
     */
    std::vector<domain::counterparty_identifier>
    get_counterparty_identifier_history(const boost::uuids::uuid& id);

private:
    repository::counterparty_identifier_repository repo_;
};

}

#endif
