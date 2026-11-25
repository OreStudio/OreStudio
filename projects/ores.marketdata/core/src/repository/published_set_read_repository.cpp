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
#include "ores.marketdata.core/repository/published_set_read_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.logging/logger.hpp"
#include "ores.marketdata.core/repository/pg_array.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace ores::marketdata::repository {

using namespace ores::logging;

namespace {

inline std::string_view published_set_read_repository_logger_name =
    "ores.marketdata.repository.published_set_read_repository";

[[nodiscard]] auto& lg() {
    static auto instance = make_logger(published_set_read_repository_logger_name);
    return instance;
}

/**
 * @brief The newest published set of each series, in one statement.
 *
 * The state is computed from the record's own facts, because the field does not
 * store it: a set ends once, and storing the state beside the facts that end it
 * would be a second source of truth for one fact. A candidate holds no published
 * instant, so asking for one leaves every candidate out.
 *
 * The tie-break is the list screen's: a superseded set is older than the set that
 * superseded it, so it sorts as the older of a tie on the published instant.
 */
constexpr const char* latest_publishes_sql =
    "select distinct on (cp.output_series_id)"
    "  cp.output_series_id::text, cp.as_of::text,"
    "  case when cp.withdrawn_at is not null then 'withdrawn'"
    "       when cp.superseded_at is not null then 'superseded'"
    "       else 'published' end"
    " from ores_marketdata_curve_publishes_tbl cp"
    " where cp.tenant_id = $1::uuid"
    "   and cp.output_series_id = any($2::uuid[])"
    "   and cp.valid_to = ores_utility_infinity_timestamp_fn()"
    "   and cp.published_at is not null"
    " order by cp.output_series_id, cp.published_at desc,"
    "   (cp.superseded_at is null) desc, cp.as_of desc";

} // namespace

std::vector<published_set_coverage>
published_set_read_repository::latest_publishes(const ores::database::context& ctx,
                                                const std::vector<boost::uuids::uuid>& series_ids) {
    std::vector<published_set_coverage> out;
    if (series_ids.empty())
        return out;

    std::vector<std::string> ids;
    ids.reserve(series_ids.size());
    for (const auto& id : series_ids)
        ids.push_back(boost::uuids::to_string(id));

    const auto tenant = ctx.tenant_id().to_string();
    const auto rows = ores::database::query_builder::execute_parameterized_multi_column_query(
        ctx,
        latest_publishes_sql,
        {tenant, pg_array(ids)},
        lg(),
        "reading the newest published set of a set of series");

    boost::uuids::string_generator gen;
    out.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() < 3 || !row[0] || !row[1] || !row[2])
            throw std::runtime_error("A published set came back without its series or instant");
        published_set_coverage one;
        one.series_id = gen(*row[0]);
        one.as_of = *row[1];
        one.state_code = *row[2];
        out.push_back(std::move(one));
    }
    return out;
}

}
