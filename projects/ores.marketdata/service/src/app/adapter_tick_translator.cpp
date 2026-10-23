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
#include "ores.marketdata.service/app/adapter_tick_translator.hpp"
#include "ores.platform/numeric/floating_point.hpp"
#include <cmath>
#include <map>
#include <utility>

namespace ores::marketdata::service::app {

std::string_view to_string(adapter_tick_drop reason) {
    switch (reason) {
        case adapter_tick_drop::unmapped:
            return "unmapped";
        case adapter_tick_drop::not_a_number:
            return "not_a_number";
        case adapter_tick_drop::out_of_range:
            return "out_of_range";
        case adapter_tick_drop::stale:
            return "stale";
        case adapter_tick_drop::out_of_order:
            return "out_of_order";
    }
    return "unmapped";
}

adapter_tick_translator::adapter_tick_translator(
    std::vector<domain::market_data_source> sources,
    std::vector<domain::market_data_vendor_instrument> instruments,
    std::vector<domain::market_data_mapping> mappings) {
    for (auto& source : sources) {
        auto name = source.name;
        sources_.emplace(std::move(name), std::move(source));
    }

    std::map<boost::uuids::uuid, instrument_key> keys_by_id;
    for (const auto& instrument : instruments) {
        keys_by_id.emplace(instrument.id,
                           instrument_key{instrument.provider_code,
                                          instrument.id_scheme,
                                          instrument.external_id,
                                          instrument.field});
    }
    for (auto& mapping : mappings) {
        const auto found = keys_by_id.find(mapping.vendor_instrument_id);
        if (found != keys_by_id.end())
            mappings_[found->second].push_back(std::move(mapping));
    }
}

std::expected<translated_tick, adapter_tick_drop>
adapter_tick_translator::translate(std::string_view adapter_id,
                                   const messaging::adapter_tick& tick,
                                   std::chrono::system_clock::time_point now) {
    auto checked = check(adapter_id, tick, now);
    if (!checked)
        count(adapter_id, checked.error());
    return checked;
}

std::expected<translated_tick, adapter_tick_drop>
adapter_tick_translator::check(std::string_view adapter_id,
                               const messaging::adapter_tick& tick,
                               std::chrono::system_clock::time_point now) {
    const auto source = sources_.find(std::string(adapter_id));
    if (source == sources_.end())
        return std::unexpected(adapter_tick_drop::unmapped);

    const auto candidates = mappings_.find(
        instrument_key{source->second.provider_code, tick.id_scheme, tick.external_id, tick.field});
    if (candidates == mappings_.end())
        return std::unexpected(adapter_tick_drop::unmapped);

    const domain::market_data_mapping* held = nullptr;
    for (const auto& mapping : candidates->second) {
        if (mapping.effective_from <= now &&
            (!mapping.effective_to || now < *mapping.effective_to) &&
            (held == nullptr || mapping.effective_from > held->effective_from))
            held = &mapping;
    }
    if (held == nullptr)
        return std::unexpected(adapter_tick_drop::unmapped);

    const auto value = ores::platform::numeric::parse_double(tick.value);
    if (!value || !std::isfinite(*value))
        return std::unexpected(adapter_tick_drop::not_a_number);
    if (std::fabs(*value) > max_abs_value)
        return std::unexpected(adapter_tick_drop::out_of_range);

    if (now - tick.vendor_time > max_tick_age)
        return std::unexpected(adapter_tick_drop::stale);

    {
        std::lock_guard lock(mu_);
        auto& newest = newest_[{source->second.name, held->oresmd_uri}];
        if (newest != std::chrono::system_clock::time_point{} && tick.vendor_time < newest)
            return std::unexpected(adapter_tick_drop::out_of_order);
        newest = tick.vendor_time;
    }

    translated_tick out;
    out.tick.oresmd_uri = held->oresmd_uri;
    out.tick.value = tick.value;
    out.tick.observation_time = tick.vendor_time;
    out.tick.source = source->second.name;
    out.provider_code = source->second.provider_code;
    out.source_id = source->second.id;
    return out;
}

void adapter_tick_translator::count(std::string_view adapter_id, adapter_tick_drop reason) {
    std::lock_guard lock(mu_);
    ++drops_[{std::string(adapter_id), reason}];
}

std::uint64_t adapter_tick_translator::drops(std::string_view adapter_id,
                                             adapter_tick_drop reason) const {
    std::lock_guard lock(mu_);
    const auto found = drops_.find({std::string(adapter_id), reason});
    return found == drops_.end() ? 0 : found->second;
}

}
