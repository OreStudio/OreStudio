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

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/account_party.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.iam.core/repository/account_party_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::service {

/**
 * @brief Service for managing account parties.
 *
 * This service provides functionality for:
 * - Managing account parties (CRUD operations)
 */
class ORES_IAM_CORE_EXPORT account_party_service {
private:
    inline static std::string_view logger_name = "ores.iam.service.account_party_service";

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
     * @brief Lists account parties with pagination.
     */
    std::vector<domain::account_party> list_account_parties(std::uint32_t offset,
                                                            std::uint32_t limit);

    /**
     * @brief Gets the total count of active account parties.
     */
    std::uint32_t get_total_account_party_count();

    /**
     * @brief Lists account parties for a specific account.
     *
     * @param account_id The account to filter by
     */
    std::vector<domain::account_party>
    list_account_parties_by_account(const boost::uuids::uuid& account_id);

    /**
     * @brief Lists account parties for a specific account, with pagination.
     */
    std::vector<domain::account_party> list_account_parties_by_account(
        const boost::uuids::uuid& account_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active account parties for an account.
     */
    std::uint32_t get_total_account_party_count_by_account(const boost::uuids::uuid& account_id);

    /**
     * @brief Finds one association by both of the columns that name it.
     *
     * A link is named by the pair, and the repository reads a page by one
     * column at a time, so the pair is resolved here. Returns nothing when the
     * account is not associated with that party.
     */
    std::optional<domain::account_party> find_account_party(
        const boost::uuids::uuid& account_id, const boost::uuids::uuid& party_id);

    /**
     * @brief Gets the total count of active account parties for a party.
     */
    std::uint32_t get_total_account_party_count_by_party(const boost::uuids::uuid& party_id);

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

    /**
     * @brief Replaces the active account parties for an account.
     *
     * Soft-closes the currently active rows for the given account and
     * inserts the rows in @p account_parties, so the active set exactly
     * matches the caller's list.
     */
    void
    replace_account_parties_by_account(const boost::uuids::uuid& account_id,
                                       const std::vector<domain::account_party>& account_parties,
                                       const std::string& modified_by,
                                       const std::string& performed_by,
                                       const std::string& change_reason_code,
                                       const std::string& change_commentary);

private:
    repository::account_party_repository repo_;
};

}

#endif
