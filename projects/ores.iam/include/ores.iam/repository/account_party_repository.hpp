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
#ifndef ORES_IAM_REPOSITORY_ACCOUNT_PARTY_REPOSITORY_HPP
#define ORES_IAM_REPOSITORY_ACCOUNT_PARTY_REPOSITORY_HPP

#include <string>
#include <vector>
#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.iam/domain/account_party.hpp"

namespace ores::iam::repository {

/**
 * @brief Reads and writes account parties to data storage.
 */
class account_party_repository {
private:
    inline static std::string_view logger_name =
        "ores.iam.repository.account_party_repository";

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
    std::vector<domain::account_party>
    read_latest_by_account(const boost::uuids::uuid& account_id);
    std::vector<domain::account_party>
    read_latest_by_party(const boost::uuids::uuid& party_id);

    void remove(const boost::uuids::uuid& account_id,
                const boost::uuids::uuid& party_id);
    void remove_by_account(const boost::uuids::uuid& account_id);

private:
    context ctx_;
};

}

#endif
