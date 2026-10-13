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
#include "ores.synthetic.api/feeds/feed_filter.hpp"
#include "ores.synthetic.api/feeds/feed_overview.hpp"
#include <algorithm>
#include <cctype>

namespace ores::synthetic::feed {

namespace {

bool is_code(const std::string& text) {
    return text.size() <= 64 && std::ranges::all_of(text, [](unsigned char c) {
               return std::islower(c) || std::isdigit(c) || c == '_' || c == '-';
           });
}

std::string id_list(const std::vector<std::string>& ids) {
    std::string out;
    for (const auto& id : ids)
        out += (out.empty() ? "" : ", ") + sql_literal(id) + "::uuid";
    return out;
}

std::string name_list(const std::vector<std::string>& names) {
    std::string out;
    for (const auto& name : names)
        out += (out.empty() ? "" : ", ") + sql_literal(name);
    return out;
}

/// A search word as a pattern that matches it literally.
std::string like_pattern(const std::string& word) {
    std::string escaped;
    for (const char c : word) {
        if (c == '\\' || c == '%' || c == '_')
            escaped.push_back('\\');
        escaped.push_back(c);
    }
    return "%" + escaped + "%";
}

}

std::string filter_problem(const feed_filter& filter) {
    if (!filter.folder_id.empty() && !is_folder_id(filter.folder_id))
        return "The folder id is not a UUID.";
    if (!is_code(filter.provider_code))
        return "The provider code is not a code.";
    if (!is_code(filter.asset_class))
        return "The asset class is not a code.";
    if (!filter.state.empty() && filter.state != "all" && filter.state != "started" &&
        filter.state != "stopped")
        return "The state is not all, started or stopped.";
    if (filter.feed_ids.size() > most_named_feeds || filter.excluded_ids.size() > most_named_feeds)
        return "A request may name at most 2000 feeds. Select by the filter instead.";
    for (const auto* ids : {&filter.feed_ids, &filter.excluded_ids})
        if (!std::ranges::all_of(*ids, [](const auto& id) { return is_folder_id(id); }))
            return "A feed id is not a UUID.";
    return {};
}

std::string sql_literal(const std::string& text) {
    std::string out = "E'";
    for (const char c : text) {
        if (c == '\'' || c == '\\')
            out.push_back(c);
        out.push_back(c);
    }
    out.push_back('\'');
    return out;
}

std::string feeds_query() {
    return R"sql(
        select f.id::text as id, 'fx_spot' as kind,
               f.base_currency_code || '/' || f.quote_currency_code as label,
               f.source_name, f.oresmd_uri, f.folder_id, f.enabled, f.auto_start,
               f.asset_class_code, f.ticks_per_hour, f.process_type as engine,
               f.base_currency_code || '/' || f.quote_currency_code as underlying,
               c.provider_code
        from ores_synthetic_fx_spot_generation_configs_tbl f
        join ores_synthetic_market_data_generation_configs_tbl c
          on c.id = f.config_id and c.valid_to = ores_utility_infinity_timestamp_fn()
        where f.valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select f.id::text, 'ir_curve',
               f.currency_code || ' ' || upper(f.index_family) ||
                   coalesce(' ' || nullif(f.tenor, ''), ''),
               f.source_name, '', f.folder_id, f.enabled, f.auto_start,
               f.asset_class_code, f.ticks_per_hour, f.process_type,
               f.currency_code, c.provider_code
        from ores_synthetic_ir_curve_generation_configs_tbl f
        join ores_synthetic_market_data_generation_configs_tbl c
          on c.id = f.config_id and c.valid_to = ores_utility_infinity_timestamp_fn()
        where f.valid_to = ores_utility_infinity_timestamp_fn())sql";
}

std::string where_clause(const feed_filter& filter, const std::vector<std::string>& running) {
    static const std::string stopped = "stopped";
    if (!filter.feed_ids.empty())
        return "id::uuid in (" + id_list(filter.feed_ids) + ")";

    std::string where = "true";
    if (!filter.folder_id.empty()) {
        where += " and folder_id in (with recursive below(id) as (select " +
                 sql_literal(filter.folder_id) +
                 "::uuid union all select f.id from ores_synthetic_folders_tbl f "
                 "join below b on f.parent_id = b.id "
                 "where f.valid_to = ores_utility_infinity_timestamp_fn()) select id from below)";
    }
    if (!filter.provider_code.empty())
        where += " and provider_code = " + sql_literal(filter.provider_code);
    if (!filter.asset_class.empty())
        where += " and asset_class_code = " + sql_literal(filter.asset_class);
    if (filter.should_run)
        where += " and enabled and auto_start";
    // A feed that should run and does not is a stopped one.
    const auto& state = filter.should_run ? stopped : filter.state;
    if (state == "started")
        where += running.empty() ? " and false" : " and source_name in (" + name_list(running) + ")";
    else if (state == "stopped" && !running.empty())
        where += " and source_name not in (" + name_list(running) + ")";
    for (const auto& word : search_terms(filter.search))
        where += " and (label || ' ' || source_name || ' ' || oresmd_uri) ilike " +
                 sql_literal(like_pattern(word));
    if (!filter.excluded_ids.empty())
        where += " and id::uuid not in (" + id_list(filter.excluded_ids) + ")";
    return where;
}

}
