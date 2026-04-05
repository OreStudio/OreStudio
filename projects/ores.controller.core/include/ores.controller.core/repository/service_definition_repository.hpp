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
#ifndef ORES_CONTROLLER_CORE_REPOSITORY_SERVICE_DEFINITION_REPOSITORY_HPP
#define ORES_CONTROLLER_CORE_REPOSITORY_SERVICE_DEFINITION_REPOSITORY_HPP

#include <vector>
#include <sqlgen/postgres.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.controller.api/domain/service_definition.hpp"

namespace ores::controller::repository {

/**
 * @brief Reads and writes service definitions to the database.
 *
 * The underlying table is bitemporal (valid_from/valid_to) but has no
 * tenant_id — it is a system-level table managed by the controller daemon.
 */
class service_definition_repository {
private:
    inline static std::string_view logger_name =
        "ores.controller.repository.service_definition_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Returns all currently active service definitions
     * (valid_to = MAX_TIMESTAMP).
     */
    std::vector<api::domain::service_definition> read_latest(context ctx);

    /**
     * @brief Inserts a new version of a service definition.
     *
     * The DB insert trigger closes the previous active version automatically
     * via the EXCLUDE constraint on the bitemporal table.
     */
    void save(context ctx, const api::domain::service_definition& v);
};

}

#endif
