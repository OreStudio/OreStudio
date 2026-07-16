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
#ifndef ORES_SYNTHETIC_CORE_REPOSITORY_FOLDER_REPOSITORY_HPP
#define ORES_SYNTHETIC_CORE_REPOSITORY_FOLDER_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.synthetic.api/domain/folder.hpp"
#include "ores.synthetic.core/export.hpp"
#include "ores.utility/domain/hierarchy.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::synthetic::repository {

/**
 * @brief Reads and writes folders to data storage.
 */
class ORES_SYNTHETIC_CORE_EXPORT folder_repository {
private:
    inline static std::string_view logger_name = "ores.synthetic.repository.folder_repository";

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
     * @brief Writes folders to database.
     */
    /**@{*/
    void write(context ctx, const domain::folder& v);
    void write(context ctx, const std::vector<domain::folder>& v);
    /**@}*/

    /**
     * @brief Reads latest folders, possibly filtered by id.
     */
    /**@{*/
    std::vector<domain::folder> read_latest(context ctx);
    std::vector<domain::folder> read_latest(context ctx, const std::string& id);
    /**@}*/

    /**
     * @brief Reads all folders, possibly filtered by id.
     */
    std::vector<domain::folder> read_all(context ctx, const std::string& id);

    /**
     * @brief Reads a single folder as it stood at a specific
     * version — the version's own [valid_from, valid_to) window is returned
     * verbatim, so the caller can compose child entities "as of" the same
     * window. See the "Temporal composite entity versioning" architecture
     * doc.
     * @param ctx Repository context with database connection
     * @param id The id to look up
     * @param version The version to fetch
     */
    std::optional<domain::folder>
    read_at_version(context ctx, const std::string& id, std::uint32_t version);

    /**
     * @brief Reads latest folders with pagination support.
     * @param ctx Repository context with database connection
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     */
    std::vector<domain::folder> read_latest(context ctx, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active folders.
     * @param ctx Repository context with database connection
     * @return Total number of active folders
     */
    std::uint32_t get_total_folder_count(context ctx);

    /**
     * @brief Deletes a folder by closing its temporal validity.
     */
    void remove(context ctx, const std::string& id);

    /**
     * @brief Deletes folders by closing their temporal validity.
     */
    void remove(context ctx, const std::vector<std::string>& ids);

    /**
     * @brief Reads the folder hierarchy as a flat set of {id,
     * parent_id, name} rows, via ores_synthetic_folders_hierarchy_fn.
     *
     * @param ctx Repository context with database connection (tenant is
     * derived from ctx.tenant_id()).
     * @param root_id The folder to start from.
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
