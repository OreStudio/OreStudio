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
#ifndef ORES_SYNTHETIC_CORE_SERVICE_GMM_COMPONENT_SERVICE_HPP
#define ORES_SYNTHETIC_CORE_SERVICE_GMM_COMPONENT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.synthetic.api/domain/gmm_component.hpp"
#include "ores.synthetic.core/export.hpp"
#include "ores.synthetic.core/repository/gmm_component_repository.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Service for managing GMM components.
 *
 * Provides a higher-level interface for component operations, wrapping the
 * underlying repository.
 */
class ORES_SYNTHETIC_CORE_EXPORT gmm_component_service {
private:
    inline static std::string_view logger_name = "ores.synthetic.service.gmm_component_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit gmm_component_service(context ctx);

    /**
     * @brief Lists components with pagination support.
     */
    std::vector<domain::gmm_component> list_components(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active components.
     */
    std::uint32_t count_components();

    /**
     * @brief Saves a component (creates or updates).
     */
    void save_component(const domain::gmm_component& component);

    /**
     * @brief Saves a batch of components atomically (all or nothing).
     */
    void save_components(const std::vector<domain::gmm_component>& components);

    /**
     * @brief Deletes a component by its id.
     */
    void delete_component(const std::string& id);

    /**
     * @brief Deletes components by their ids.
     */
    void delete_components(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves a single component by its id.
     */
    std::optional<domain::gmm_component> get_component(const std::string& id);

    /**
     * @brief Retrieves all historical versions of a component.
     */
    std::vector<domain::gmm_component> get_component_history(const std::string& id);

private:
    context ctx_;
    repository::gmm_component_repository repo_;
};

}

#endif
