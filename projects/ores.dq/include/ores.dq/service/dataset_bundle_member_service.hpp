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
#ifndef ORES_DQ_SERVICE_DATASET_BUNDLE_MEMBER_SERVICE_HPP
#define ORES_DQ_SERVICE_DATASET_BUNDLE_MEMBER_SERVICE_HPP

#include <string>
#include <vector>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.dq/domain/dataset_bundle_member.hpp"
#include "ores.dq/repository/dataset_bundle_member_repository.hpp"

namespace ores::dq::service {

/**
 * @brief Service for managing dataset bundle members.
 *
 * This service provides functionality for:
 * - Managing dataset bundle members (CRUD operations)
 */
class dataset_bundle_member_service {
private:
    inline static std::string_view logger_name =
        "ores.dq.service.dataset_bundle_member_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a dataset_bundle_member_service with required repositories.
     *
     * @param ctx The database context.
     */
    explicit dataset_bundle_member_service(context ctx);

    /**
     * @brief Lists all dataset bundle members.
     */
    std::vector<domain::dataset_bundle_member> list_members();

    /**
     * @brief Lists dataset bundle members for a specific bundle.
     *
     * @param bundle_code The bundle to filter by
     */
    std::vector<domain::dataset_bundle_member>
    list_members_by_bundle(const std::string& bundle_code);

    /**
     * @brief Saves a dataset bundle member (creates or updates).
     *
     * @param member The dataset bundle member to save
     */
    void save_member(const domain::dataset_bundle_member& member);

    /**
     * @brief Saves multiple dataset bundle members (creates or updates).
     *
     * @param members The dataset bundle members to save
     */
    void save_members(const std::vector<domain::dataset_bundle_member>& members);

    /**
     * @brief Removes a dataset bundle member.
     *
     * @param bundle_code The bundle
     * @param dataset_code The dataset
     */
    void remove_member(const std::string& bundle_code,
        const std::string& dataset_code);

private:
    repository::dataset_bundle_member_repository repo_;
};

}

#endif
