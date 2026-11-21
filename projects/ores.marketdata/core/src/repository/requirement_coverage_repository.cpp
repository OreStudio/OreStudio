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
#include "ores.marketdata.core/repository/requirement_coverage_repository.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <stdexcept>
#include <string>

namespace ores::marketdata::repository {

namespace {

/*
 * One row per requirement, whether the market answers it or not. A party's own
 * series answers the address, and a fixing is also answered by the system
 * tenant's series of that index, because the export writes the world's fixing
 * when the party holds none of its own; that rule is stated here as the export
 * states it.
 */
constexpr auto coverage_query =
    "with requirements as ("
    "    select distinct jsonb_array_elements_text($3::jsonb)::text as uri),"
    " coverage as ("
    "    select c.oresmd_uri, bool_or(c.series_id is not null) as answered"
    "    from ores_marketdata_requirement_coverage_fn("
    "        $1::uuid, $2::uuid,"
    "        array(select uri from requirements),"
    "        coalesce($4::timestamptz, now())) c"
    "    group by c.oresmd_uri)"
    " select r.uri,"
    "        coalesce(c.answered, false)"
    "        or exists (select 1 from ores_marketdata_market_series_tbl s"
    "                   where s.tenant_id = ores_utility_system_tenant_id_fn()"
    "                     and s.oresmd_uri = r.uri"
    "                     and s.valid_to = ores_utility_infinity_timestamp_fn())"
    " from requirements r"
    " left join coverage c on c.oresmd_uri = r.uri"
    " order by r.uri";

}

std::vector<requirement_coverage>
requirement_coverage_repository::read(const context& ctx,
                                      const std::vector<std::string>& requirements,
                                      const std::optional<std::string>& as_of) {
    std::vector<requirement_coverage> out;
    if (requirements.empty())
        return out;

    const auto party = ctx.party_id();
    if (!party)
        throw std::invalid_argument(
            "requirement coverage: reading it needs the party the run acts for");

    const auto requirements_json = ores::utility::codec::json::write(requirements);
    std::vector<std::optional<std::string>> params{
        ctx.tenant_id().to_string(),
        boost::uuids::to_string(*party),
        requirements_json,
        // The close of the business date, so a point written about that day counts, as the
        // export's own line for the date does.
        as_of ? std::optional<std::string>(*as_of + "T23:59:59.999999Z") :
                std::optional<std::string>{}};

    for (const auto& row : ores::database::query_builder::execute_parameterized_multi_column_query(
             ctx, coverage_query, params, lg(), "Reading the market's coverage of requirements"))
        if (row.size() >= 2 && row[0])
            out.push_back(
                requirement_coverage{*row[0], row[1] && (*row[1] == "t" || *row[1] == "true")});
    return out;
}

}
