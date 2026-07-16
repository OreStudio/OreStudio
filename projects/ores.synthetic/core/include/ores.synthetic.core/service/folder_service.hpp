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
#ifndef ORES_SYNTHETIC_CORE_SERVICE_FOLDER_SERVICE_HPP
#define ORES_SYNTHETIC_CORE_SERVICE_FOLDER_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.synthetic.api/domain/folder.hpp"
#include "ores.synthetic.core/export.hpp"
#include "ores.synthetic.core/repository/folder_repository.hpp"
#include "ores.utility/domain/hierarchy.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Service for managing folders.
 *
 * Provides a higher-level interface for folder operations,
 * wrapping the underlying repository.
 */
class ORES_SYNTHETIC_CORE_EXPORT folder_service {
private:
    inline static std::string_view logger_name = "ores.synthetic.service.folder_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a folder_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit folder_service(context ctx);

    /**
     * @brief Lists folders with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of folders for the requested page.
     */
    std::vector<domain::folder> list_folders(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active folders.
     *
     * @return Total number of active folders.
     */
    std::uint32_t count_folders();

    /**
     * @brief Retrieves a single folder as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param id The id of the folder.
     * @param version The version to fetch.
     * @return The folder at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::folder> get_folder_at_version(const std::string& id,
                                                        std::uint32_t version);

    /**
     * @brief Retrieves a single folder by its id.
     *
     * @param id The id of the folder.
     * @return The folder if found, std::nullopt otherwise.
     */
    std::optional<domain::folder> get_folder(const std::string& id);

    /**
     * @brief Saves a folder (creates or updates).
     *
     * @param folder The folder to save.
     * @throws std::exception on failure.
     */
    void save_folder(const domain::folder& folder);

    /**
     * @brief Saves a batch of folders.
     *
     * @param folders The folders to save.
     * @throws std::exception on failure.
     */
    void save_folders(const std::vector<domain::folder>& folders);

    /**
     * @brief Deletes a folder by its id.
     *
     * @param id The id of the folder to delete.
     * @throws std::exception on failure.
     */
    void delete_folder(const std::string& id);

    /**
     * @brief Deletes folders by their ids.
     */
    void delete_folders(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a folder.
     */
    std::vector<domain::folder> get_folder_history(const std::string& id);

    /**
     * @brief Gets the folder hierarchy (as a forest of trees) rooted
     * at, or containing, the given folder.
     *
     * @param root_id The folder to start from.
     * @param from_root If true, returns the whole tree the given node
     * belongs to instead of just its subtree.
     * @return A forest of hierarchy_node trees (normally a single root).
     */
    std::vector<ores::utility::domain::hierarchy_node>
    get_hierarchy(const boost::uuids::uuid& root_id, bool from_root);

private:
    context ctx_;
    repository::folder_repository repo_;
};

}

#endif
