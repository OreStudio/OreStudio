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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_TICK_COORDINATE_REPOSITORY_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_TICK_COORDINATE_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <string>

namespace ores::marketdata::repository {

/**
 * @brief The dictionary that turns a datum URI into the small number that a tick row stores.
 *
 * A datum gets the next number of its series the first time the ingest sees it, and keeps that
 * number, because every tick row of the series refers to it. A series without coordinates has
 * one datum and so one number.
 */
class ORES_MARKETDATA_CORE_EXPORT tick_coordinate_repository {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.repository.tick_coordinate_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief The number of a datum within its series, assigned when the datum has none.
     *
     * Two writers may race for the same number, so a lost race reads again. The tenant is the
     * tenant of @p ctx.
     *
     * @throws std::runtime_error if no number can be assigned or read.
     */
    std::int16_t resolve(context ctx,
                         const boost::uuids::uuid& party_id,
                         const boost::uuids::uuid& series_id,
                         const std::string& oresmd_uri);
};

}

#endif
