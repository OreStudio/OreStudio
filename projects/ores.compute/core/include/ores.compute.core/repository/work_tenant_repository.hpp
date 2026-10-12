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
#ifndef ORES_COMPUTE_REPOSITORY_WORK_TENANT_REPOSITORY_HPP
#define ORES_COMPUTE_REPOSITORY_WORK_TENANT_REPOSITORY_HPP

#include "ores.compute.core/export.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <boost/uuid/uuid.hpp>
#include <string_view>
#include <vector>

namespace ores::compute::repository {

/**
 * @brief The tenants that have dispatched work to the grid.
 *
 * The list belongs to the installation. Only a session of the system tenant reads
 * or writes it, and the policy decides that, so every method here runs with the
 * service's own context. The poller of the grid samples reads it to know which
 * tenants to count, because it cannot read the tenants' own work to find them.
 */
class ORES_COMPUTE_CORE_EXPORT work_tenant_repository {
private:
    inline static std::string_view logger_name = "ores.compute.repository.work_tenant_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Record that a tenant dispatches work. A tenant already listed is left as it is.
     */
    void note(context ctx, const boost::uuids::uuid& work_tenant_id);

    /**
     * @brief Every tenant that has dispatched work.
     */
    std::vector<boost::uuids::uuid> list(context ctx);
};

}

#endif
