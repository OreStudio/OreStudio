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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_LIVE_TENANT_REPOSITORY_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_LIVE_TENANT_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <set>
#include <string_view>

namespace ores::marketdata::repository {

/**
 * @brief The tenants a write may name: active or bootstrapping, as the database's own tenant
 * check accepts them.
 *
 * A read across tenants returns the rows of every tenant, a terminated one included, and a
 * write for a terminated tenant fails. A service filters such a read by this set.
 */
class ORES_MARKETDATA_CORE_EXPORT live_tenant_repository {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.repository.live_tenant_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    [[nodiscard]] std::set<boost::uuids::uuid> read(context ctx);
};

}

#endif
