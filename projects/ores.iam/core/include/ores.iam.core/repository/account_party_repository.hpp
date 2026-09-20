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
#ifndef ORES_IAM_CORE_REPOSITORY_ACCOUNT_PARTY_REPOSITORY_HPP
#define ORES_IAM_CORE_REPOSITORY_ACCOUNT_PARTY_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/account_party.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.logging/make_logger.hpp"
#include <boost/uuid/uuid.hpp>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::iam::repository {

/**
 * @brief Reads and writes account parties to data storage.
 */
class ORES_IAM_CORE_EXPORT account_party_repository {
private:
    inline static std::string_view logger_name = "ores.iam.repository.account_party_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit account_party_repository(context ctx);

    std::string sql();

    void write(const domain::account_party& account_party);
    void write(const std::vector<domain::account_party>& account_parties);

    std::vector<domain::account_party> read_latest();
    std::vector<domain::account_party> read_latest(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active account parties.
     */
    std::uint32_t get_total_account_party_count();
    std::vector<domain::account_party> read_latest_by_account(const boost::uuids::uuid& account_id);
    /**
     * @brief Reads latest account parties filtered by account_id, with pagination.
     */
    std::vector<domain::account_party> read_latest_by_account(const boost::uuids::uuid& account_id,
                                                              std::uint32_t offset,
                                                              std::uint32_t limit);

    /**
     * @brief Gets the total count of active account parties filtered by account_id.
     */
    std::uint32_t get_total_account_party_count_by_account(const boost::uuids::uuid& account_id);

    std::vector<domain::account_party> read_latest_by_party(const boost::uuids::uuid& party_id);

    /**
     * @brief Gets the total count of active account parties filtered by party_id.
     */
    std::uint32_t get_total_account_party_count_by_party(const boost::uuids::uuid& party_id);

    void remove(const boost::uuids::uuid& account_id, const boost::uuids::uuid& party_id);
    void remove_by_account(const boost::uuids::uuid& account_id);
    /**
     * @brief Replaces the active account parties for a account.
     *
     * Soft-closes the currently active rows for the given
     * account and inserts the rows in @p account_parties,
     * so the active set exactly matches the caller's list.
     */
    void replace_by_account(const boost::uuids::uuid& account_id,
                            const std::vector<domain::account_party>& account_parties,
                            const std::string& modified_by,
                            const std::string& performed_by,
                            const std::string& change_reason_code,
                            const std::string& change_commentary);

private:
    context ctx_;
};

}

#endif
