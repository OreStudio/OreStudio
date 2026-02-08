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
#ifndef ORES_IAM_SERVICE_ACCOUNT_PARTY_SERVICE_HPP
#define ORES_IAM_SERVICE_ACCOUNT_PARTY_SERVICE_HPP

#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.iam/domain/account_party.hpp"
#include "ores.iam/repository/account_party_repository.hpp"

namespace ores::iam::service {

/**
 * @brief Service for managing account parties.
 *
 * This service provides functionality for:
 * - Managing account parties (CRUD operations)
 */
class account_party_service {
private:
    inline static std::string_view logger_name =
        "ores.iam.service.account_party_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a account_party_service with required repositories.
     *
     * @param ctx The database context.
     */
    explicit account_party_service(context ctx);

    /**
     * @brief Lists all account parties.
     */
    std::vector<domain::account_party> list_account_parties();

    /**
     * @brief Lists account parties for a specific account.
     *
     * @param account_id The account to filter by
     */
    std::vector<domain::account_party>
    list_account_parties_by_account(const boost::uuids::uuid& account_id);

    /**
     * @brief Saves a account party (creates or updates).
     *
     * @param account_party The account party to save
     */
    void save_account_party(const domain::account_party& account_party);

    /**
     * @brief Removes a account party.
     *
     * @param account_id The account
     * @param party_id The party
     */
    void remove_account_party(const boost::uuids::uuid& account_id,
        const boost::uuids::uuid& party_id);

private:
    repository::account_party_repository repo_;
};

}

#endif
