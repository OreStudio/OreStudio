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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_OBSERVATION_READ_REPOSITORY_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_OBSERVATION_READ_REPOSITORY_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstddef>
#include <string>
#include <vector>

namespace ores::marketdata::repository {

/// One stored observation as a window read returns it.
struct stored_observation final {
    std::chrono::system_clock::time_point observed_at;
    std::string coordinate;
    std::string value;
    std::string source;
};

/**
 * @brief Reads the current observations of a series over a span of days.
 *
 * Only the current version of each observation is read, so a correction stands in for
 * the value it replaced.
 */
class ORES_MARKETDATA_CORE_EXPORT observation_read_repository {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.repository.observation_read_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief The observations of a series from the start of @p from to the end of @p to,
     * in UTC, oldest first and by coordinate within a time, at most @p limit of them.
     */
    std::vector<stored_observation> between(const context& ctx,
                                            const boost::uuids::uuid& series_id,
                                            std::chrono::year_month_day from,
                                            std::chrono::year_month_day to,
                                            std::size_t limit);
};

}

#endif
