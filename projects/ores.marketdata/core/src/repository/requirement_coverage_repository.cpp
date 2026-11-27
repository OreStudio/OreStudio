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
 * One row per requirement, whether the market answers it or not, with the series
 * that answers it and whether that series holds a point at the date.
 *
 * A party's own series answers the address. A fixing is also answered by the
 * system tenant's series of the same address, because the export writes the
 * world's fixing when the party holds none of its own; a quote is not, because
 * the export takes from the system tenant only fixing series, so counting a
 * system-tenant quote here would pass a requirement the body never carries.
 *
 * The system tenant's series are installation reference data, which every party
 * reads, so its rows are counted without asking for a party of their own.
 *
 * The point is read from the series that answers the address, which is why the
 * share of the read that the coverage function performs is joined here rather
 * than asked again: the function answers for the party's series, and the fixing
 * fallback has to be read with its own tenant.
 */
constexpr auto coverage_query =
    "with requirements as ("
    "    select distinct jsonb_array_elements_text($3::jsonb)::text as uri),"
    " party as ("
    "    select c.oresmd_uri as uri,"
    "           (array_agg(c.series_id order by c.series_id)"
    "               filter (where c.series_id is not null))[1] as series_id,"
    "           max(c.latest_point) as latest_point"
    "    from ores_marketdata_requirement_coverage_fn("
    "        $1::uuid, $2::uuid,"
    "        array(select uri from requirements),"
    "        coalesce($4::timestamptz, now())) c"
    "    group by c.oresmd_uri),"
    " system_fixing as ("
    "    select distinct on (r.uri) r.uri, s.id as series_id,"
    "        greatest("
    "            (select max(f.fixing_date)::timestamp at time zone 'UTC'"
    "             from ores_marketdata_market_fixings_tbl f"
    "             where f.tenant_id = s.tenant_id and f.series_id = s.id"
    "               and f.fixing_date <="
    "                   (coalesce($4::timestamptz, now()) at time zone 'UTC')::date"
    "               and f.valid_to = ores_utility_infinity_timestamp_fn()),"
    "            (select max(o.observation_datetime)"
    "             from ores_marketdata_market_observations_tbl o"
    "             where o.tenant_id = s.tenant_id and o.series_id = s.id"
    "               and o.observation_datetime <= coalesce($4::timestamptz, now())"
    "               and o.valid_to = ores_utility_infinity_timestamp_fn())) as latest_point"
    "    from requirements r"
    "    join ores_marketdata_market_series_tbl s"
    "      on s.tenant_id = ores_utility_system_tenant_id_fn()"
    "     and s.oresmd_uri = r.uri"
    "     and s.valid_to = ores_utility_infinity_timestamp_fn()"
    "    where position('?type=fixing&' in r.uri) > 0"
    "      and not exists (select 1 from party p"
    "                      where p.uri = r.uri and p.series_id is not null)"
    "    order by r.uri, s.id)"
    " select r.uri,"
    "        p.series_id, sy.series_id,"
    "        (case when p.series_id is not null then p.latest_point"
    "              else sy.latest_point end) is not null"
    " from requirements r"
    " left join party p on p.uri = r.uri"
    " left join system_fixing sy on sy.uri = r.uri"
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
        // A bare business date is read at the close of its day, so a point written about that
        // day counts, as the export's own line for the date does. A value that already carries
        // a time is read as it stands.
        (as_of && as_of->size() == 10 && as_of->find('T') == std::string::npos) ?
            std::optional<std::string>(*as_of + "T23:59:59.999999Z") :
            as_of};

    for (const auto& row : ores::database::query_builder::execute_parameterized_multi_column_query(
             ctx, coverage_query, params, lg(), "Reading the market's coverage of requirements")) {
        if (row.size() < 4 || !row[0])
            continue;
        requirement_coverage one{.oresmd_uri = *row[0]};
        // The series the party holds answers first; the system's fixing answers when it holds
        // none, and the query picks one of the two so that the row names the series read.
        if (row[1])
            one.series_id = *row[1];
        else if (row[2])
            one.series_id = *row[2];

        const auto has_point = row[3] && (*row[3] == "t" || *row[3] == "true");
        if (!one.series_id)
            one.cause = std::string(gap_cause::no_series);
        else if (!has_point)
            one.cause = std::string(gap_cause::no_point);
        out.push_back(std::move(one));
    }
    return out;
}

}
