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
#ifndef ORES_DQ_SERVICE_DATASET_BUNDLE_SERVICE_HPP
#define ORES_DQ_SERVICE_DATASET_BUNDLE_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.dq/domain/dataset_bundle.hpp"
#include "ores.dq/domain/dataset_bundle_member.hpp"
#include "ores.dq/repository/dataset_bundle_repository.hpp"
#include "ores.dq/repository/dataset_bundle_member_repository.hpp"

namespace ores::dq::service {

/**
 * @brief Service for managing dataset bundles and their members.
 *
 * This service provides functionality for:
 * - Managing dataset bundles (CRUD operations)
 * - Managing bundle membership (CRUD operations)
 */
class dataset_bundle_service {
private:
    inline static std::string_view logger_name =
        "ores.dq.service.dataset_bundle_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a dataset_bundle_service with required repositories.
     *
     * @param ctx The database context.
     */
    explicit dataset_bundle_service(context ctx);

    // ========================================================================
    // Dataset Bundle Management
    // ========================================================================

    /**
     * @brief Lists all dataset bundles.
     */
    std::vector<domain::dataset_bundle> list_bundles();

    /**
     * @brief Finds a bundle by its ID.
     */
    std::optional<domain::dataset_bundle>
    find_bundle(const boost::uuids::uuid& id);

    /**
     * @brief Finds a bundle by its code.
     */
    std::optional<domain::dataset_bundle>
    find_bundle_by_code(const std::string& code);

    /**
     * @brief Saves a bundle (creates or updates).
     *
     * @param bundle The bundle to save
     */
    void save_bundle(const domain::dataset_bundle& bundle);

    /**
     * @brief Removes a bundle.
     *
     * @param id The ID of the bundle to remove
     */
    void remove_bundle(const boost::uuids::uuid& id);

    /**
     * @brief Gets the version history for a bundle.
     *
     * @param id The bundle ID
     * @return Vector of all versions, newest first
     */
    std::vector<domain::dataset_bundle>
    get_bundle_history(const boost::uuids::uuid& id);

    // ========================================================================
    // Dataset Bundle Member Management
    // ========================================================================

    /**
     * @brief Lists all bundle members.
     */
    std::vector<domain::dataset_bundle_member> list_members();

    /**
     * @brief Lists members for a specific bundle.
     *
     * @param bundle_code The bundle code to filter by
     */
    std::vector<domain::dataset_bundle_member>
    list_members_by_bundle(const std::string& bundle_code);

    /**
     * @brief Saves a bundle member (creates or updates).
     *
     * @param member The member to save
     */
    void save_member(const domain::dataset_bundle_member& member);

    /**
     * @brief Removes a bundle member.
     *
     * @param bundle_code The bundle code
     * @param dataset_code The dataset code
     */
    void remove_member(const std::string& bundle_code,
        const std::string& dataset_code);

private:
    repository::dataset_bundle_repository bundle_repo_;
    repository::dataset_bundle_member_repository member_repo_;
};

}

#endif
