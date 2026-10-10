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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_MANUAL_POINT_GUARD_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_MANUAL_POINT_GUARD_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.core/export.hpp"
#include <vector>

namespace ores::marketdata::repository {

/**
 * @brief Keeps an automatic write from replacing a point an operator keyed by
 * hand.
 *
 * The precedence rule is this. A manual point owns its coordinate until it is
 * cleared. An automatic write at a later instant leaves the manual row where it
 * is, and the as-of read prefers the manual point to whatever later value the
 * feed wrote. An automatic write at the very same instant would replace the
 * manual row and leave the annex naming a value the row no longer holds, so the
 * shared insert drops that write instead.
 *
 * The feed, the import and the republish all write through the insert, so this
 * one rule covers all three. Only write_manual_point writes a manual point, and
 * it does not pass through here.
 *
 * The guard reads the annex and the insert is a separate statement. A manual
 * point committed between the two, by another transaction, can still be replaced
 * by an automatic write at its instant. The window is one round trip wide, and
 * closing it needs the check inside the insert statement.
 */
class ORES_MARKETDATA_CORE_EXPORT manual_point_guard final {
public:
    /**
     * @brief The observations of @p observations that do not land on a coordinate
     * an operator owns, in their original order.
     *
     * The result is the whole batch when no point of it is shadowed, which is the
     * case for every batch of a series nobody over-keyed. A point is matched to
     * the microsecond, which is the precision the store keeps. A dropped write is
     * logged once per batch at info, with the count.
     */
    static std::vector<domain::market_observation>
    unshadowed(ores::database::context ctx,
               const std::vector<domain::market_observation>& observations);
};

}

#endif
