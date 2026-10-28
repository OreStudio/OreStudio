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
#include "ores.marketdata.official/domain/source_feed.hpp"
#include <algorithm>
#include <utility>

namespace ores::marketdata::official::domain {

source_feed::source_feed(hosted_source source, frankfurter_fetcher& fetcher, int backfill_days)
    : source_(std::move(source))
    , fetcher_(fetcher)
    , backfill_days_(backfill_days) {}

std::expected<void, std::string> source_feed::load_universe() {
    auto universe = fetcher_.universe(source_.frankfurter_key);
    if (!universe)
        return std::unexpected(universe.error());
    pivot_ = std::move(universe->pivot);
    universe_ = {universe->currencies.begin(), universe->currencies.end()};
    return {};
}

messaging::adapter_announce_request source_feed::announcement() const {
    messaging::adapter_announce_request request;
    request.adapter_id = source_.source_name;
    request.provider_code = source_.provider_code;
    for (const auto& currency : universe_)
        request.universe.push_back({std::string(id_scheme), currency, std::string(rate_field)});
    return request;
}

messaging::adapter_demand_response
source_feed::answer(const messaging::adapter_demand_request& request) const {
    messaging::adapter_demand_response response;
    for (const auto& id : request.ids) {
        if (id.id_scheme == id_scheme && id.field == rate_field &&
            universe_.contains(id.external_id))
            response.accepted.push_back(id);
        else
            response.rejected.push_back({id, "unknown"});
    }
    return response;
}

messaging::adapter_demand_response
source_feed::demand(const messaging::adapter_demand_request& request) {
    auto response = answer(request);
    demanded_.clear();
    for (const auto& id : response.accepted)
        demanded_.insert(id.external_id);
    return response;
}

std::expected<std::size_t, std::string> source_feed::refresh(std::chrono::year_month_day today,
                                                             const publish_fn& publish) {
    using std::chrono::days;
    using std::chrono::sys_days;
    if (demanded_.empty())
        return 0;
    const auto window_start = std::chrono::year_month_day{sys_days{today} - days{backfill_days_}};
    // A currency starts after the later of the last day it sent and the last day it was read
    // through, so one that publishes nothing costs a read of a day, not of the window.
    const auto after = [](std::chrono::year_month_day d) {
        return std::chrono::year_month_day{sys_days{d} + days{1}};
    };
    auto from = today;
    for (const auto& currency : demanded_) {
        auto start = window_start;
        if (const auto sent = last_sent_.find(currency); sent != last_sent_.end())
            start = std::max(start, after(sent->second));
        if (const auto read = read_through_.find(currency); read != read_through_.end())
            start = std::max(start, after(read->second));
        from = std::min(from, start);
    }
    if (from > today)
        return 0;

    auto rates = fetcher_.rates(source_.frankfurter_key, pivot_, from, today);
    if (!rates)
        return std::unexpected(rates.error());
    std::ranges::stable_sort(*rates, {}, &published_rate::date);
    std::size_t sent = 0;
    for (const auto& rate : *rates) {
        // A rate against another base would be filed under the wrong item, since an item names
        // the pivot; it is refused rather than sent.
        if (rate.base != pivot_) {
            ++misbased_;
            continue;
        }
        if (!demanded_.contains(rate.quote))
            continue;
        const auto last = last_sent_.find(rate.quote);
        if (last != last_sent_.end() && rate.date <= last->second)
            continue;
        publish(tick_of(rate, ++sequence_));
        last_sent_.insert_or_assign(rate.quote, rate.date);
        ++sent;
    }
    // Today's rate may not be published yet, so a read counts only through yesterday.
    const auto yesterday = std::chrono::year_month_day{sys_days{today} - days{1}};
    for (const auto& currency : demanded_)
        read_through_.insert_or_assign(currency, yesterday);
    return sent;
}

messaging::adapter_tick source_feed::tick_of(const published_rate& rate, std::uint64_t sequence) {
    messaging::adapter_tick tick;
    tick.id_scheme = std::string(id_scheme);
    tick.external_id = rate.quote;
    tick.field = std::string(rate_field);
    tick.value = rate.rate;
    tick.vendor_time = std::chrono::sys_days{rate.date} + std::chrono::hours(12);
    tick.sequence = sequence;
    return tick;
}

}
