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
#ifndef ORES_SYNTHETIC_CORE_REPOSITORY_GMM_COMPONENT_MAPPER_HPP
#define ORES_SYNTHETIC_CORE_REPOSITORY_GMM_COMPONENT_MAPPER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.synthetic.api/domain/gmm_component.hpp"
#include "ores.synthetic.core/export.hpp"
#include "ores.synthetic.core/repository/gmm_component_entity.hpp"

namespace ores::synthetic::repository {

/**
 * @brief Maps domain model entities to data storage layer and vice-versa.
 */
class ORES_SYNTHETIC_CORE_EXPORT gmm_component_mapper {
private:
    inline static std::string_view logger_name = "ores.synthetic.repository.gmm_component_mapper";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static domain::gmm_component map(const gmm_component_entity& v);
    static gmm_component_entity map(const domain::gmm_component& v);

    static std::vector<domain::gmm_component> map(const std::vector<gmm_component_entity>& v);
    static std::vector<gmm_component_entity> map(const std::vector<domain::gmm_component>& v);
};

}

#endif
