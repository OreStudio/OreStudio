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
#ifndef ORES_MARKETDATA_CORE_SERVICE_ORE_EXPORT_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_ORE_EXPORT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.core/export.hpp"
#include <chrono>
#include <optional>
#include <string>

namespace ores::marketdata::service {

/**
 * @brief The tenant's market data rendered back into ORE's own text format.
 *
 * Two bodies, because ORE reads two files: =market.txt= is one
 * DATE<TAB>KEY<TAB>VALUE line per observation, =fixings.txt= one
 * DATE<TAB>INDEX<TAB>VALUE line per fixing. Both are produced by
 * =ores.ore.core='s serializer, so the export and the import agree on the
 * format by construction rather than by two implementations being kept in
 * step.
 */
struct ore_export_result final {
    std::string market_data;
    std::string fixings;
    int series_count = 0;
    int observation_count = 0;
    int fixing_count = 0;
    /// The datums whose line at the valuation date carries a value from an earlier day.
    int carried_forward_count = 0;
    /// The datums left out of the valuation date because their latest value is too old.
    int stale_count = 0;
};

/**
 * @brief Writes the tenant's stored market data back out as ORE text.
 *
 * The inverse of import_service, and held to the same edge: what the import
 * read from a file, this writes back to one. It is what makes the component's
 * fidelity claim measurable -- a round trip through the database either
 * reproduces the input or it does not, and the test that drives both ends says
 * which.
 *
 * Two things decide what a key line says.
 *
 * Where the observation carries the producer's own key, that key is emitted
 * verbatim. The import deliberately rewrites a key it can name -- to the
 * canonical spelling, and to the corrected pair when refdata reports an FX/RATE
 * key reversed -- so the series' columns hold the importer's text and this
 * column holds the file's.
 *
 * Where it carries none, the key is rebuilt from the series and the
 * observation's point, and the point is dropped for a series type that has no
 * point dimension. Every point-free observation stores a point anyway -- =SPOT=
 * for an FX rate, the empty string for a recovery rate -- because a row must
 * name the point it was recorded at. Emitting it would turn
 * =FX/RATE/EUR/USD= into =FX/RATE/EUR/USD/SPOT=, a key no producer writes, so
 * the shape table decides rather than the stored value.
 */
class ORES_MARKETDATA_CORE_EXPORT ore_export_service final {
private:
    inline static std::string_view logger_name = "ores.marketdata.service.ore_export_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit ore_export_service(context ctx);

    /**
     * @brief Writes the market data and the fixings.
     *
     * Every stored observation of the observation table is one line, dated at its own time. The
     * ticks of a live feed are not among them, because they have no such row.
     *
     * With a date, each datum also has one line dated at @p valuation_date, with the value it
     * had at the close of that day, from the observation table or the tick table, whichever
     * holds the later value. That line replaces any observation line of the same date. The close
     * is the end of the day in UTC. The history of other dates stays, because an analytic can
     * read more than one date.
     *
     * A line at the date may carry a value forward from an earlier day, but not one older than
     * @p max_age_days before the date. A datum whose latest value is older has no line at the
     * date, so the run does not take a stale quote for a current one. The result counts both
     * kinds of datum. A datum with no value at or before the close has no line either.
     *
     * @param max_age_days The limit above, or the default of default_max_age_days.
     *
     * @throws std::invalid_argument if the date is not an ISO 8601 date, or the limit is
     * negative.
     */
    [[nodiscard]] ore_export_result
    write_all(const std::optional<std::string>& valuation_date = std::nullopt,
              const std::optional<int>& max_age_days = std::nullopt) const;

    /// The limit in days when a caller states none. It covers a weekend and a holiday.
    static constexpr int default_max_age_days = 5;

private:
    context ctx_;
};

}

#endif
