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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_REQUIREMENT_COVERAGE_REPOSITORY_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_REQUIREMENT_COVERAGE_REPOSITORY_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.logging/logger.hpp"
#include "ores.marketdata.core/export.hpp"
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::marketdata::repository {

/**
 * @brief One oresmd address a run needs, and whether the market answers it.
 *
 * The answer mirrors what a run's export writes: the party's own series with the
 * address, or, for a fixing, the system tenant's series of that index, which the
 * export reads when the party holds none.
 */
struct requirement_coverage final {
    std::string oresmd_uri;
    bool answered = false;
};

/**
 * @brief Which of a run's requirements the market answers, at a date.
 *
 * The question is the one =ores_marketdata_requirement_coverage_fn= answers, asked
 * once for the whole list so the planner pays for one chunk rather than one per
 * address, and extended by the system tenant's fixing series, which the export
 * writes for a party that holds none of its own.
 */
class ORES_MARKETDATA_CORE_EXPORT requirement_coverage_repository {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.repository.requirement_coverage_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief One row per requirement, whether or not the market answers it.
     *
     * @param as_of The business date the points are read at, or nothing for now. The
     *              session's tenant and party come from @p ctx, since a run prices for the
     *              party it acts for and no other.
     */
    [[nodiscard]] std::vector<requirement_coverage>
    read(const context& ctx,
         const std::vector<std::string>& requirements,
         const std::optional<std::string>& as_of);
};

}

#endif
