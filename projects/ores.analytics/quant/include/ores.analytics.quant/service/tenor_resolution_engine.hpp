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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_TENOR_RESOLUTION_ENGINE_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_TENOR_RESOLUTION_ENGINE_HPP

#include "ores.analytics.quant/domain/business_day_calendar_set.hpp"
#include "ores.analytics.quant/domain/tenor_resolution.hpp"
#include "ores.analytics.quant/export.hpp"
#include <span>
#include <vector>

namespace ores::analytics::quant::service {

/**
 * @brief Resolves tenors to dates, in batch: a flat span of questions in, a
 * flat vector of answers out, aligned by index, one status per row.
 *
 * Nothing here throws for bad input and nothing here reads a database, a
 * clock or a code string. A malformed rule is one row's @c status; the rest
 * of the batch is answered. That is the shape an operation needs, where a
 * refusal is a payload rather than a failed request, and it is the shape a
 * screen needs, where one bad row must not empty the list.
 *
 * What it does not do, deliberately:
 *
 * - It does not derive the spot date. Spot is the currency pair's settlement
 *   date; a caller that needs one resolves it with
 *   @c calendar_arithmetic::advance from the horizon and the pair's lag.
 * - It does not read schedules. A schedule's dates arrive as flat rows, the
 *   shape an ordered bulk read returns, and the closed-form ones this engine
 *   knows walk internally.
 * - It does not guess at an anchor whose meaning is unsettled. The @c status
 *   says so instead (see @c domain::resolution_status::unsupported_anchor).
 */
class ORES_ANALYTICS_QUANT_EXPORT tenor_resolution_engine final {
public:
    /**
     * @brief Answers each request, aligned 1:1 with @p requests by index.
     *
     * @param requests The questions. Each carries its own calendar index and
     * its own rule; the batch is the unit of work, not the unit of meaning.
     * @param calendars The holiday sets every request's
     * @c tenor_resolution_request::calendar_index indexes into. The set must
     * reach past the dates it will be asked about: a set ending in December
     * knows nothing about the following January, so a roll that crosses the
     * year would be answered as if that January were clear. The engine
     * reports a calendar it was not given at all
     * (@c resolution_status::calendar_index_out_of_range); it cannot tell
     * that one was given too narrowly.
     * @param schedule_dates The dates of every event-lookup schedule any
     * request walks, ordered by (schedule_index, date) -- which an
     * @c ORDER BY schedule, event_date returns for free. Unordered rows are
     * reported per row (@c schedule_dates_not_ordered) rather than quietly
     * answering with the wrong meeting. Closed-form schedules need none.
     */
    [[nodiscard]] static std::vector<domain::tenor_resolution_result>
    resolve_batch(std::span<const domain::tenor_resolution_request> requests,
                  const domain::business_day_calendar_set& calendars,
                  std::span<const domain::schedule_date> schedule_dates = {});
};

} // namespace ores::analytics::quant::service

#endif
