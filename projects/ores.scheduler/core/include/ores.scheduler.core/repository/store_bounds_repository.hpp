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
#ifndef ORES_SCHEDULER_CORE_REPOSITORY_STORE_BOUNDS_REPOSITORY_HPP
#define ORES_SCHEDULER_CORE_REPOSITORY_STORE_BOUNDS_REPOSITORY_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.logging/logger.hpp"
#include "ores.scheduler.api/messaging/scheduling_operations_protocol.hpp"
#include "ores.scheduler.core/export.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::scheduler::repository {

/**
 * @brief One store's measurements over a window, and the window the read used.
 *
 * The window travels with the points because the plot draws both: the line says
 * how fast the store moved and the axis says over what. A read that stated only
 * the points would leave the screen to guess the horizon from its own clock, and
 * a screen is not the installation.
 */
struct store_sample_series {
    std::vector<messaging::store_sample> samples;
    /** The instant the window opens, in ISO-8601 UTC. */
    std::string from_at;
    /** The instant the read ran, in ISO-8601 UTC: where the window closes. */
    std::string to_at;
};

/**
 * @brief The housekeeping reads: what bounds each store, and one store's series.
 *
 * Both are views over state three tables hold, and the screen reads them as the
 * database states them rather than composing them from entities: whether a store
 * is bounded is a fact about the installation, and a screen that inferred it
 * would be a second implementation of the same rule.
 */
class ORES_SCHEDULER_CORE_EXPORT store_bounds_repository {
public:
    explicit store_bounds_repository(const ores::database::context& ctx);

    /**
     * @brief Every store, in catalogue order, with its bound and its size.
     */
    [[nodiscard]] std::vector<messaging::store_bound> read_bounds();

    /**
     * @brief The measurements of one store over a range, oldest first.
     *
     * The plot reads its points from here. A store nothing measures answers with
     * no points, which is the honest answer and not a flat line at zero, and it
     * still answers with the window the read covered.
     *
     * @param range =1h=, =24h= or =7d=; anything else answers the day.
     */
    [[nodiscard]] store_sample_series read_samples(const std::string& store_code,
                                                   const std::string& range);

private:
    const ores::database::context& ctx_;
};

/**
 * @brief How many hours a range covers.
 *
 * The screen offers three horizons and the read holds them as hours, so the
 * period is one decision in one place and a range nobody stated cannot widen the
 * read past the month.
 */
[[nodiscard]] ORES_SCHEDULER_CORE_EXPORT int range_hours(const std::string& range);

/**
 * @brief One row of the bounds view, as the columns the read selects them.
 *
 * The mapping is a free function so a test can state a row and read the answer:
 * the columns arrive as text because the query helper hands back text, and the
 * conversions that matter are the absent ones.
 */
[[nodiscard]] ORES_SCHEDULER_CORE_EXPORT messaging::store_bound
store_bound_from_row(const std::vector<std::optional<std::string>>& row);

}

#endif
