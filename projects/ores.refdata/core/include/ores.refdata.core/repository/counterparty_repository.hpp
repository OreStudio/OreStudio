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
#ifndef ORES_REFDATA_CORE_REPOSITORY_COUNTERPARTY_REPOSITORY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_COUNTERPARTY_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/counterparty.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.utility/domain/hierarchy.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::refdata::repository {

/**
 * @brief Reads and writes counterparties to data storage.
 */
class ORES_REFDATA_CORE_EXPORT counterparty_repository {
private:
    inline static std::string_view logger_name = "ores.refdata.repository.counterparty_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Writes counterparties to database.
     */
    /**@{*/
    void write(context ctx, const domain::counterparty& v);
    void write(context ctx, const std::vector<domain::counterparty>& v);
    /**@}*/

    /**
     * @brief Reads latest counterparties, possibly filtered by id.
     */
    /**@{*/
    std::vector<domain::counterparty> read_latest(context ctx);
    std::vector<domain::counterparty> read_latest(context ctx, const std::string& id);
    /**@}*/

    /**
     * @brief Reads all counterparties, possibly filtered by id.
     */
    std::vector<domain::counterparty> read_all(context ctx, const std::string& id);


    /**
     * @brief Reads latest counterparties with pagination support.
     * @param ctx Repository context with database connection
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     */
    std::vector<domain::counterparty>
    read_latest(context ctx, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active counterparties.
     * @param ctx Repository context with database connection
     * @return Total number of active counterparties
     */
    std::uint32_t get_total_counterparty_count(context ctx);

    /**
     * @brief Deletes a counterparty by closing its temporal validity.
     */
    void remove(context ctx, const std::string& id);

    /**
     * @brief Deletes counterparties by closing their temporal validity.
     */
    void remove(context ctx, const std::vector<std::string>& ids);

    /**
     * @brief Reads the counterparty hierarchy as a flat set of {id,
     * parent_id, name} rows, via ores_refdata_counterparties_hierarchy_fn.
     *
     * @param ctx Repository context with database connection (tenant is
     * derived from ctx.tenant_id()).
     * @param root_id The counterparty to start from.
     * @param from_root If true, first walks up to the ultimate ancestor and
     * returns the whole tree the given node belongs to, instead of just its
     * subtree.
     * @return Flat hierarchy rows, ready for ores::utility::domain::build_tree.
     */
    std::vector<ores::utility::domain::hierarchy_flat_row>
    get_hierarchy(context ctx, const boost::uuids::uuid& root_id, bool from_root);
};

}

#endif
