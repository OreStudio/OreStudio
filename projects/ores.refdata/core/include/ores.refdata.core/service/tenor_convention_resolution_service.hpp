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
#ifndef ORES_REFDATA_CORE_SERVICE_TENOR_CONVENTION_RESOLUTION_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_TENOR_CONVENTION_RESOLUTION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/tenor_convention_resolution.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/tenor_convention_resolution_repository.hpp"
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for reading tenor convention resolutions. Read-only: this junction's rows are
 * reference data managed via Foundation-layer SQL provisioning -- see
 * ores.refdata.tenor_convention_resolution.org. Hand-authored to fill the gap while junction
 * codegen doesn't generate a service/protocol/handler layer -- see the story on retiring the
 * legacy codegen profile system and adding junction C++ support.
 */
class ORES_REFDATA_CORE_EXPORT tenor_convention_resolution_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.tenor_convention_resolution_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit tenor_convention_resolution_service(context ctx);

    /**
     * @brief Lists every active resolution row for the tenant.
     */
    std::vector<domain::tenor_convention_resolution> list_resolutions();

private:
    repository::tenor_convention_resolution_repository repo_;
};

}

#endif
