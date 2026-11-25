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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_PUBLISHED_SET_READ_REPOSITORY_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_PUBLISHED_SET_READ_REPOSITORY_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.marketdata.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <vector>

namespace ores::marketdata::repository {

/**
 * @brief The newest published set of one series: when it was built and the state it is in now.
 */
struct published_set_coverage final {
    boost::uuids::uuid series_id;
    std::string as_of;
    std::string state_code;
};

/**
 * @brief Reads the newest published set of a set of series in one statement.
 *
 * The series are the batch. A caller that draws a table of curves asks for all of them
 * at once rather than for one curve at a time, and the statement costs one plan
 * whatever the size of the set.
 */
class ORES_MARKETDATA_CORE_EXPORT published_set_read_repository final {
public:
    /**
     * @brief The newest set of each series that holds one, ordered by series id.
     *
     * A set nobody approved is not a published set, so a series that holds only
     * candidates does not appear: the caller reads its absence as the gap it is.
     */
    std::vector<published_set_coverage>
    latest_publishes(const ores::database::context& ctx,
                     const std::vector<boost::uuids::uuid>& series_ids);
};

}

#endif
