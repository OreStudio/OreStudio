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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_FEED_BINDING_ACTIVITY_REPOSITORY_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_FEED_BINDING_ACTIVITY_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::repository {

/**
 * @brief What a feed binding last delivered.
 */
struct feed_binding_activity_record final {
    boost::uuids::uuid series_id;
    std::chrono::system_clock::time_point last_tick_at;
};

/**
 * @brief The newest tick of one binding, to record.
 */
struct feed_binding_activity_entry final {
    boost::uuids::uuid party_id;
    std::string source_name;
    boost::uuids::uuid series_id;
    std::chrono::system_clock::time_point last_tick_at;
};

/**
 * @brief The row that says what each feed binding last delivered.
 *
 * One row for each tenant, party and source. A tick of the tick table carries no source, so
 * the ingest records the newest tick of each binding here, and the activity reader reads it.
 */
class ORES_MARKETDATA_CORE_EXPORT feed_binding_activity_repository {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.repository.feed_binding_activity_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Records the newest tick of each binding of @p entries with one statement, unless a
     * row already holds a newer one.
     *
     * The tenant is the tenant of @p ctx. Each binding appears once.
     */
    void record(context ctx, const std::vector<feed_binding_activity_entry>& entries);

    /**
     * @brief The activity of one binding, or nothing when its source has delivered nothing.
     */
    std::optional<feed_binding_activity_record>
    read(context ctx, const boost::uuids::uuid& party_id, const std::string& source_name);
};

}

#endif
