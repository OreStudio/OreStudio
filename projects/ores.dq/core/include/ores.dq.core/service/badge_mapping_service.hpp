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
#ifndef ORES_DQ_SERVICE_BADGE_MAPPING_SERVICE_HPP
#define ORES_DQ_SERVICE_BADGE_MAPPING_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/badge_mapping.hpp"
#include "ores.dq.core/repository/badge_mapping_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing badge mappings.
 *
 * This service provides functionality for:
 * - Managing badge mappings (CRUD operations)
 */
class badge_mapping_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.badge_mapping_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a badge_mapping_service with required repositories.
     *
     * @param ctx The database context.
     */
    explicit badge_mapping_service(context ctx);

    /**
     * @brief Lists all badge mappings.
     */
    std::vector<domain::badge_mapping> list_mappings();

    /**
     * @brief Lists badge mappings for a specific code domain.
     *
     * @param code_domain_code The code domain to filter by
     */
    std::vector<domain::badge_mapping>
    list_mappings_by_code_domain(const std::string& code_domain_code);

    /**
     * @brief Saves a badge mapping (creates or updates).
     *
     * @param mapping The badge mapping to save
     */
    void save_mapping(const domain::badge_mapping& mapping);

    /**
     * @brief Removes a badge mapping.
     *
     * @param code_domain_code The code domain
     * @param entity_code The entity code
     */
    void remove_mapping(const std::string& code_domain_code, const std::string& entity_code);

private:
    repository::badge_mapping_repository repo_;
};

}

#endif
