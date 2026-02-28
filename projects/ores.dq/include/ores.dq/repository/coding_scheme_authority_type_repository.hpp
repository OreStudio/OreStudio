/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_DQ_REPOSITORY_CODING_SCHEME_AUTHORITY_TYPE_REPOSITORY_HPP
#define ORES_DQ_REPOSITORY_CODING_SCHEME_AUTHORITY_TYPE_REPOSITORY_HPP

#include <string>
#include <vector>
#include <sqlgen/postgres.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.dq/domain/coding_scheme_authority_type.hpp"

namespace ores::dq::repository {

/**
 * @brief Reads and writes coding_scheme_authority_types to data storage.
 */
class coding_scheme_authority_type_repository {
private:
    inline static std::string_view logger_name =
        "ores.dq.repository.coding_scheme_authority_type_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit coding_scheme_authority_type_repository(context ctx);

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Writes coding_scheme_authority_types to database.
     */
    /**@{*/
    void write(const domain::coding_scheme_authority_type& authority_type);
    void write(const std::vector<domain::coding_scheme_authority_type>& authority_types);
    /**@}*/

    /**
     * @brief Reads latest coding_scheme_authority_types, possibly filtered by code.
     */
    /**@{*/
    std::vector<domain::coding_scheme_authority_type> read_latest();
    std::vector<domain::coding_scheme_authority_type> read_latest(const std::string& code);
    /**@}*/

    /**
     * @brief Reads latest coding_scheme_authority_types with pagination support.
     */
    std::vector<domain::coding_scheme_authority_type>
    read_latest(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active coding_scheme_authority_types.
     */
    std::uint32_t get_total_count();

    /**
     * @brief Reads all historical versions of a coding_scheme_authority_type by code.
     */
    std::vector<domain::coding_scheme_authority_type> read_all(const std::string& code);

    /**
     * @brief Deletes a coding_scheme_authority_type by closing its temporal validity.
     */
    void remove(const std::string& code);

    /**
     * @brief Deletes coding_scheme_authority_types by closing their temporal validity.
     */
    void remove(const std::vector<std::string>& codes);

private:
    context ctx_;
};

}

#endif
