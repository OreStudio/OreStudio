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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.synthetic.api/feeds/derived_fx_vol_feed.hpp"
#include "ores.logging/make_logger.hpp"
#include <algorithm>
#include <chrono>
#include <iterator>
#include <ranges>
#include <string>
#include <utility>

namespace ores::synthetic::feed {

namespace {

auto& derived_fx_feed_lg() {
    static auto instance = ores::logging::make_logger("ores.synthetic.api.derived_fx_vol_feed");
    return instance;
}

double seconds_since(std::chrono::steady_clock::time_point start) {
    return std::chrono::duration<double>(std::chrono::steady_clock::now() - start).count();
}

namespace qm = analytics::quant::math;
namespace mm = marketdata::messaging;

std::string key_of(const derived_fx_pair& pair) {
    return pair.base + pair.quote;
}

bool in_notation(const mm::vendor_id& id) {
    return id.id_scheme == derived_fx_id_scheme && id.field == derived_fx_field &&
           parse_derived_fx_external_id(id.external_id).has_value();
}

}

derived_fx_vol_feed::derived_fx_vol_feed(std::string source_name,
                                         std::string provider_code,
                                         derived_fx_readers readers,
                                         qm::fx_derivation_settings settings,
                                         std::optional<derived_fx_store> store)
    : source_name_(std::move(source_name))
    , provider_code_(std::move(provider_code))
    , readers_(std::move(readers))
    , settings_(settings)
    , store_(std::move(store)) {}

void derived_fx_vol_feed::set_universe(const std::vector<std::string>& external_ids) {
    universe_.clear();
    for (const auto& id : external_ids)
        if (parse_derived_fx_external_id(id))
            universe_.insert(id);
}

mm::adapter_announce_request derived_fx_vol_feed::announcement() const {
    mm::adapter_announce_request request;
    request.adapter_id = source_name_;
    request.provider_code = provider_code_;
    for (const auto& id : universe_)
        request.universe.push_back(
            {std::string(derived_fx_id_scheme), id, std::string(derived_fx_field)});
    return request;
}

mm::adapter_demand_response
derived_fx_vol_feed::answer(const mm::adapter_demand_request& request) const {
    mm::adapter_demand_response response;
    for (const auto& id : request.ids) {
        if (in_notation(id) && universe_.contains(id.external_id))
            response.accepted.push_back(id);
        else
            response.rejected.push_back({id, "unknown"});
    }
    return response;
}

mm::adapter_demand_request coalesce_demands(mm::adapter_demand_request earlier,
                                            mm::adapter_demand_request later) {
    later.image.insert(later.image.end(),
                       std::make_move_iterator(earlier.image.begin()),
                       std::make_move_iterator(earlier.image.end()));
    return later;
}

mm::adapter_demand_response derived_fx_vol_feed::demand(const mm::adapter_demand_request& request) {
    auto response = answer(request);
    demanded_.clear();
    for (const auto& id : response.accepted)
        demanded_.insert(id.external_id);
    // An id a party needs for the first time is sent again with the day it already went out,
    // as a vendor sends a new subscriber its last value.
    for (const auto& id : request.image)
        last_sent_.erase(id.external_id);
    return response;
}

std::expected<std::vector<mm::adapter_tick>, std::string>
derived_fx_vol_feed::ticks_of(const derived_fx_pair& pair,
                              const std::set<std::string>& ids,
                              std::chrono::year_month_day today,
                              std::chrono::year_month_day& day) {
    const auto legs = ecb_legs_of(pair);
    const auto read = [&](const std::optional<std::string>& item)
        -> std::expected<std::optional<dated_series>, std::string> {
        if (!item)
            return std::optional<dated_series>{};
        auto series = readers_.fixings(*item, today);
        if (!series)
            return std::unexpected("the fixings of " + *item + ": " + series.error());
        return std::optional<dated_series>{std::move(*series)};
    };
    const auto plus = read(legs.plus);
    if (!plus)
        return std::unexpected(plus.error());
    const auto minus = read(legs.minus);
    if (!minus)
        return std::unexpected(minus.error());
    const auto returns = pair_log_returns(*plus, *minus);
    if (!returns)
        return std::unexpected(returns.error());
    if (!returns->last)
        return std::unexpected("the legs share no day of fixings");
    day = *returns->last;

    std::set<std::string> owed;
    std::set<std::string> owed_expiries;
    for (const auto& id : ids) {
        const auto sent = last_sent_.find(id);
        if (sent != last_sent_.end() && sent->second >= day)
            continue;
        owed.insert(id);
        owed_expiries.insert(parse_derived_fx_external_id(id)->expiry);
    }
    if (owed.empty())
        return std::vector<mm::adapter_tick>{};

    const auto convention = readers_.convention(pair);
    if (!convention)
        return std::unexpected("the convention: " + convention.error());
    std::vector<qm::fx_expiry> expiries;
    for (const auto& e : derived_fx_expiries) {
        if (!owed_expiries.contains(std::string(e.label)))
            continue;
        const auto at = convention_at(*convention, e.label);
        if (!at)
            return std::unexpected("the convention at " + std::string(e.label) + ": " + at.error());
        expiries.push_back({.label = std::string(e.label), .years = e.years, .convention = *at});
    }
    const auto derivation = qm::derive_fx_quotes(returns->returns, expiries, settings_);
    if (!derivation)
        return std::unexpected(derivation.error());
    for (const auto& expiry : derivation->expiries)
        if (!expiry)
            failures_.push_back(key_of(pair) + " " + expiry.error());
    auto ticks = derived_fx_ticks(pair, *derivation, day, sequence_);
    std::erase_if(ticks, [&](const mm::adapter_tick& t) { return !owed.contains(t.external_id); });
    return ticks;
}

std::size_t derived_fx_vol_feed::persist(std::chrono::year_month_day today,
                                         std::size_t pair_limit) {
    if (!store_)
        return 0;
    failures_.clear();
    std::map<std::string, derived_fx_pair> pairs;
    for (const auto& id : universe_)
        if (const auto quote = parse_derived_fx_external_id(id))
            pairs.emplace(key_of(quote->pair), quote->pair);

    using namespace ores::logging;
    const auto pass_started = std::chrono::steady_clock::now();
    const auto fail = [&](const std::string& key, const std::string& why) {
        BOOST_LOG_SEV(derived_fx_feed_lg(), warn) << "Derived history of " << key << ": " << why;
        failures_.push_back(key + ": " + why);
    };
    BOOST_LOG_SEV(derived_fx_feed_lg(), info)
        << "Keeping the derived history of " << pairs.size() << " pair(s) up to " << today;

    std::size_t written = 0;
    std::size_t pairs_written = 0;
    for (const auto& [key, pair] : pairs) {
        if (pairs_written >= pair_limit)
            break;
        auto kept = kept_.find(key);
        if (kept == kept_.end()) {
            auto started = std::chrono::steady_clock::now();
            const auto series = store_->ensure_series(pair);
            if (!series) {
                fail(key, "the series: " + series.error());
                continue;
            }
            BOOST_LOG_SEV(derived_fx_feed_lg(), info)
                << "Derived history of " << key << ": series " << *series << " ready in "
                << seconds_since(started) << "s";
            started = std::chrono::steady_clock::now();
            auto held = store_->stored_days(*series, pair, today);
            if (!held) {
                fail(key, "the days held: " + held.error());
                continue;
            }
            BOOST_LOG_SEV(derived_fx_feed_lg(), info)
                << "Derived history of " << key << ": holds " << held->size()
                << " stored day(s), read in " << seconds_since(started) << "s";
            kept = kept_.emplace(key, std::pair{*series, std::move(*held)}).first;
        }
        auto& [series_id, held] = kept->second;

        const auto legs = ecb_legs_of(pair);
        const auto read = [&](const std::optional<std::string>& item)
            -> std::expected<std::optional<dated_series>, std::string> {
            if (!item)
                return std::optional<dated_series>{};
            auto series = readers_.fixings(*item, today);
            if (!series)
                return std::unexpected("the fixings of " + *item + ": " + series.error());
            return std::optional<dated_series>{std::move(*series)};
        };
        auto started = std::chrono::steady_clock::now();
        const auto plus = read(legs.plus);
        const auto minus = plus ? read(legs.minus) : plus;
        if (!plus || !minus) {
            fail(key, !plus ? plus.error() : minus.error());
            continue;
        }
        const auto returns = pair_log_returns(*plus, *minus);
        if (!returns) {
            fail(key, returns.error());
            continue;
        }
        BOOST_LOG_SEV(derived_fx_feed_lg(), debug)
            << "Derived history of " << key << ": " << returns->dates.size()
            << " daily return(s) from the fixings, read in " << seconds_since(started) << "s";
        // A history already held whole needs no derivation: only a new day of fixings does.
        const auto first = static_cast<std::size_t>(std::max(settings_.minimum_window, 1)) - 1;
        const bool whole = std::ranges::all_of(
            std::views::drop(returns->dates, static_cast<std::ptrdiff_t>(first)),
            [&](const std::chrono::year_month_day d) { return held.contains(d); });
        if (whole) {
            BOOST_LOG_SEV(derived_fx_feed_lg(), debug)
                << "Derived history of " << key << ": every day is held; nothing to derive";
            continue;
        }
        const auto convention = readers_.convention(pair);
        if (!convention) {
            fail(key, "the convention: " + convention.error());
            continue;
        }
        started = std::chrono::steady_clock::now();
        const auto missing = derive_missing_days(
            pair, *returns, *convention, held, settings_, settings_.minimum_window);
        if (!missing) {
            fail(key, missing.error());
            continue;
        }
        // A day that cannot be derived is named once and not tried again until a restart,
        // since its fixings will not change.
        if (!missing->underived.empty()) {
            fail(key, std::to_string(missing->underived.size()) + " day(s) could not be derived");
            held.insert(missing->underived.begin(), missing->underived.end());
        }
        if (missing->days.empty())
            continue;
        const auto derived_in = seconds_since(started);
        started = std::chrono::steady_clock::now();
        if (const auto stored = store_->write(series_id, missing->days); !stored) {
            fail(key, "the write: " + stored.error());
            continue;
        }
        BOOST_LOG_SEV(derived_fx_feed_lg(), info)
            << "Derived history of " << key << ": " << missing->days.size() << " day(s) derived in "
            << derived_in << "s and written in " << seconds_since(started) << "s";
        for (const auto& d : missing->days)
            held.insert(d.day);
        written += missing->days.size();
        ++pairs_written;
    }
    BOOST_LOG_SEV(derived_fx_feed_lg(), info)
        << "Kept the derived history of " << pairs.size() << " pair(s) in "
        << seconds_since(pass_started) << "s: " << written << " day(s) written, "
        << failures_.size() << " failure(s)";
    return written;
}

std::size_t derived_fx_vol_feed::refresh(std::chrono::year_month_day today,
                                         const publish_fn& publish) {
    failures_.clear();
    std::map<std::string, std::pair<derived_fx_pair, std::set<std::string>>> pairs;
    for (const auto& id : demanded_)
        if (const auto quote = parse_derived_fx_external_id(id)) {
            auto& entry = pairs[key_of(quote->pair)];
            entry.first = quote->pair;
            entry.second.insert(id);
        }

    std::size_t sent = 0;
    for (const auto& [key, entry] : pairs) {
        std::chrono::year_month_day day{};
        const auto ticks = ticks_of(entry.first, entry.second, today, day);
        if (!ticks) {
            failures_.push_back(key + ": " + ticks.error());
            continue;
        }
        for (const auto& tick : *ticks) {
            publish(tick);
            last_sent_.insert_or_assign(tick.external_id, day);
            ++sent;
            ++sequence_;
        }
    }
    return sent;
}

}
