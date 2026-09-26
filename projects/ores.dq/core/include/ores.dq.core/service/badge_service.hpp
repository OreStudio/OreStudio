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
#ifndef ORES_DQ_CORE_SERVICE_BADGE_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_BADGE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/messaging/badge_protocol.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/badge_mapping_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Read-only service over the badge mapping junction.
 *
 * Badge definitions, badge severities and code domains each have a generated
 * service of their own; only the mapping projection is read here.
 */
class ORES_DQ_CORE_EXPORT badge_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.badge_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit badge_service(context ctx);

    std::vector<messaging::badge_mapping> list_mappings();

private:
    context ctx_;
    repository::badge_mapping_repository map_repo_;
};

}

#endif
