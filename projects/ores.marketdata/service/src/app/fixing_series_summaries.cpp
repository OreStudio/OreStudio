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
#include "ores.marketdata.service/app/fixing_series_summaries.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.logging/logger.hpp"
#include <charconv>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::marketdata::service::app {

namespace {

auto& fixing_series_summaries_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.service.app.fixing_series_summaries");
    return instance;
}

using rows_t = std::vector<std::vector<std::optional<std::string>>>;

/// The columns the read states, so a row is read whole or not at all.
constexpr std::size_t k_series_id = 0;
constexpr std::size_t k_uri = 1;
constexpr std::size_t k_provider_code = 2;
constexpr std::size_t k_days = 3;
constexpr std::size_t k_latest_date = 4;
constexpr std::size_t k_latest_value = 5;
constexpr std::size_t k_source = 6;
constexpr std::size_t k_derivation_kind = 7;
constexpr std::size_t k_columns = 8;

std::string text_of(const std::optional<std::string>& value) {
    return value.value_or(std::string{});
}

/// The part of the address before its query: "oresmd://ir/USD".
std::string_view path_of(std::string_view uri) {
    const auto query = uri.find('?');
    return uri.substr(0, query == std::string_view::npos ? uri.size() : query);
}

/// The address's scheme host: fx, ir, or empty for an address this read does not list.
std::string asset_class_of(std::string_view uri) {
    const auto path = path_of(uri);
    const auto scheme = path.find("://");
    if (scheme == std::string_view::npos)
        return {};
    const auto host = path.substr(scheme + 3);
    const auto slash = host.find('/');
    return std::string(host.substr(0, slash));
}

/// A query parameter's value, or empty when the address states none.
std::string param_of(std::string_view uri, std::string_view name) {
    const auto query = uri.find('?');
    if (query == std::string_view::npos)
        return {};
    auto rest = uri.substr(query + 1);
    while (!rest.empty()) {
        const auto pair = rest.substr(0, rest.find('&'));
        const auto equals = pair.find('=');
        if (equals != std::string_view::npos && pair.substr(0, equals) == name)
            return std::string(pair.substr(equals + 1));
        rest = rest.size() > pair.size() ? rest.substr(pair.size() + 1) : std::string_view{};
    }
    return {};
}

/// The currency in the address's path, which an IR index is stated under.
std::string path_currency_of(std::string_view uri) {
    const auto path = path_of(uri);
    const auto last = path.rfind('/');
    return last == std::string_view::npos ? std::string{} : std::string(path.substr(last + 1));
}

/// Whether the address names @p currency: an FX series names two, an IR index one.
bool states_currency(std::string_view uri, std::string_view currency) {
    return path_currency_of(uri) == currency || param_of(uri, "ccy") == currency;
}

/**
 * @brief What the series fixes, in the words a screen shows.
 *
 * An IR index is stated as currency, name and tenor, such as "USD SOFR 1M"; an
 * FX fixing as its pair, such as "EUR/USD". An address that states neither is
 * shown as it is, because the screen states what the store holds rather than
 * hiding the row it cannot read.
 */
std::string label_of(std::string_view uri) {
    const auto asset_class = asset_class_of(uri);
    if (asset_class == "fx") {
        const auto base = path_currency_of(uri);
        const auto quote = param_of(uri, "ccy");
        if (!base.empty() && !quote.empty())
            return base + "/" + quote;
    }
    if (asset_class == "ir") {
        std::string label = path_currency_of(uri);
        for (const auto* part : {"name", "tenor"}) {
            const auto value = param_of(uri, part);
            if (!value.empty())
                label += " " + value;
        }
        if (!label.empty())
            return label;
    }
    return std::string(uri);
}

/// The day a "YYYY-MM-DD" text states, which is the form PostgreSQL writes.
std::optional<std::chrono::year_month_day> read_day(const std::string& text) {
    const auto read_int = [&text](std::size_t& at, int& value) {
        const auto [end, ec] = std::from_chars(text.data() + at, text.data() + text.size(), value);
        if (ec != std::errc{})
            return false;
        at = static_cast<std::size_t>(end - text.data());
        return true;
    };
    int year = 0;
    int month = 0;
    int day = 0;
    std::size_t at = 0;
    if (!read_int(at, year) || at >= text.size() || text[at++] != '-' || !read_int(at, month) ||
        at >= text.size() || text[at++] != '-' || !read_int(at, day))
        return std::nullopt;
    return std::chrono::year_month_day{std::chrono::year{year},
                                       std::chrono::month{static_cast<unsigned>(month)},
                                       std::chrono::day{static_cast<unsigned>(day)}};
}

rows_t read_series(const ores::database::context& ctx) {
    return ores::database::query_builder::execute_raw_multi_column_query(
        ctx,
        R"sql(
select s.id::text,
       s.oresmd_uri,
       s.provider_code,
       count(f.id) as days,
       coalesce(max(f.fixing_date)::text, '') as latest_date,
       coalesce((array_agg(f.value order by f.fixing_date desc))[1], '') as latest_value,
       coalesce((array_agg(f.source order by f.fixing_date desc))[1], '') as source,
       s.derivation_kind
from ores_marketdata_market_series_tbl s
left join ores_marketdata_market_fixings_tbl f
  on f.series_id = s.id
 and f.valid_to = ores_utility_infinity_timestamp_fn()
where s.valid_to = ores_utility_infinity_timestamp_fn()
  and (s.oresmd_uri like 'oresmd://fx/%type=fixing%'
       or s.oresmd_uri like 'oresmd://ir/%type=fixing%')
group by s.id, s.oresmd_uri, s.provider_code, s.derivation_kind
order by s.oresmd_uri)sql",
        fixing_series_summaries_lg(),
        "Reading the fixing series");
}

}

::ores::marketdata::messaging::list_fixing_series_summaries_response list_fixing_series_summaries(
    const ores::database::context& ctx,
    const ::ores::marketdata::messaging::list_fixing_series_summaries_request& request) {
    ::ores::marketdata::messaging::list_fixing_series_summaries_response resp;
    for (const auto& row : read_series(ctx)) {
        if (row.size() < k_columns)
            continue;
        const auto uri = text_of(row[k_uri]);
        const auto asset_class = asset_class_of(uri);
        if (asset_class.empty())
            continue;
        const auto source = text_of(row[k_source]);
        if (!request.asset_class.empty() && asset_class != request.asset_class)
            continue;
        if (!request.currency.empty() && !states_currency(uri, request.currency))
            continue;
        if (!request.source.empty() && source != request.source)
            continue;
        ::ores::marketdata::messaging::fixing_series_summary summary;
        summary.series_id = text_of(row[k_series_id]);
        summary.oresmd_uri = uri;
        summary.asset_class = asset_class;
        summary.index_label = label_of(uri);
        summary.source = source;
        summary.provider_code = text_of(row[k_provider_code]);
        summary.latest_value = text_of(row[k_latest_value]);
        summary.latest_date = read_day(text_of(row[k_latest_date]));
        summary.days = static_cast<std::uint32_t>(std::stoul(text_of(row[k_days])));
        summary.derivation_kind = text_of(row[k_derivation_kind]);
        resp.series.push_back(std::move(summary));
    }
    return resp;
}

}
