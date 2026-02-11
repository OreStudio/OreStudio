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
#ifndef ORES_REFDATA_REPOSITORY_PARTY_REPOSITORY_HPP
#define ORES_REFDATA_REPOSITORY_PARTY_REPOSITORY_HPP

#include <string>
#include <vector>
#include <cstdint>
#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.refdata/domain/party.hpp"

namespace ores::refdata::repository {

/**
 * @brief Reads and writes parties to data storage.
 */
class party_repository {
private:
    inline static std::string_view logger_name =
        "ores.refdata.repository.party_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit party_repository(context ctx);

    std::string sql();

    void write(const domain::party& party);
    void write(const std::vector<domain::party>& parties);

    std::vector<domain::party> read_latest();
    std::vector<domain::party> read_latest(const boost::uuids::uuid& id);
    std::vector<domain::party> read_latest_by_code(const std::string& code);

    /**
     * @brief Reads the system party for a given tenant.
     *
     * Every tenant has exactly one system party (party_category='system')
     * which serves as the root of the party hierarchy.
     *
     * @param tenant_id The tenant identifier
     * @return Vector containing the system party, or empty if not found
     */
    std::vector<domain::party>
    read_system_party(const std::string& tenant_id);

    /**
     * @brief Reads latest parties with pagination support.
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     * @return Vector of parties within the specified range
     */
    std::vector<domain::party> read_latest(std::uint32_t offset,
                                            std::uint32_t limit);

    /**
     * @brief Gets the total count of active parties.
     * @return Total number of parties with valid_to == max_timestamp
     */
    std::uint32_t get_total_party_count();

    std::vector<domain::party> read_all(const boost::uuids::uuid& id);
    void remove(const boost::uuids::uuid& id);

private:
    context ctx_;
};

}

#endif
