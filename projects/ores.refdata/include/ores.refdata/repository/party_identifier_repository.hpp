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
#ifndef ORES_REFDATA_REPOSITORY_PARTY_IDENTIFIER_REPOSITORY_HPP
#define ORES_REFDATA_REPOSITORY_PARTY_IDENTIFIER_REPOSITORY_HPP

#include <string>
#include <vector>
#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.refdata/domain/party_identifier.hpp"

namespace ores::refdata::repository {

/**
 * @brief Reads and writes party identifiers to data storage.
 */
class party_identifier_repository {
private:
    inline static std::string_view logger_name =
        "ores.refdata.repository.party_identifier_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit party_identifier_repository(context ctx);

    std::string sql();

    void write(const domain::party_identifier& party_identifier);
    void write(const std::vector<domain::party_identifier>& party_identifiers);

    std::vector<domain::party_identifier> read_latest();
    std::vector<domain::party_identifier> read_latest(const boost::uuids::uuid& id);
    std::vector<domain::party_identifier> read_latest_by_code(const std::string& code);

    std::vector<domain::party_identifier> read_all(const boost::uuids::uuid& id);
    void remove(const boost::uuids::uuid& id);

private:
    context ctx_;
};

}

#endif
