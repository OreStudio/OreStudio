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
#ifndef ORES_MARKETDATA_SERVICE_APP_ADAPTER_TICK_TRANSLATOR_HPP
#define ORES_MARKETDATA_SERVICE_APP_ADAPTER_TICK_TRANSLATOR_HPP

#if defined(_MSC_VER)
#    pragma once
#endif

#include "ores.marketdata.api/domain/adapter_limits.hpp"
#include "ores.marketdata.api/domain/market_data_mapping.hpp"
#include "ores.marketdata.api/domain/market_data_source.hpp"
#include "ores.marketdata.api/domain/market_data_vendor_instrument.hpp"
#include "ores.marketdata.api/messaging/adapter_operation_protocol.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.service/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <expected>
#include <map>
#include <mutex>
#include <set>
#include <string>
#include <string_view>
#include <tuple>
#include <unordered_map>
#include <vector>

namespace ores::marketdata::service::app {

/**
 * @brief Why the service drops a tick of an adapter before it stores the tick.
 *
 * The reasons are the same for every adapter. None depends on the kind of the source.
 */
enum class adapter_tick_drop {
    /// The adapter is no source, or its tick has no vendor instrument, or no mapping holds now.
    unmapped,
    /// The value is not a decimal number.
    not_a_number,
    /// The value is outside the bounds.
    out_of_range,
    /// The vendor time is older than the age limit.
    stale,
    /// The vendor time is before the newest vendor time stored for the item.
    out_of_order
};

[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT std::string_view to_string(adapter_tick_drop reason);

/**
 * @brief A tick that passed the checks: the tick the pipeline stores, and the provider and
 * the source it belongs to.
 */
/**
 * @brief What a tick is, which decides the rules it meets and where it is stored.
 *
 * A quote is perishable: it ages out and is kept for days. A fixing is an official value of
 * an index on a date, a durable fact: it never ages out, and it is kept for good in the
 * fixings table.
 */
enum class tick_class { quote, fixing };

struct translated_tick {
    messaging::market_tick tick;
    tick_class kind = tick_class::quote;
    std::string provider_code;
    boost::uuids::uuid source_id;
};

/**
 * @brief Maps and checks the ticks of adapters.
 *
 * It holds the catalogue the checks need: the sources by name, the vendor instruments of each
 * provider and the mappings from them to oresmd items. An adapter id is the name of a source,
 * and the source says the provider. @c translate() finds the vendor instrument of the tick,
 * the mapping that holds at the time of the call, and then applies the checks in this order:
 * mapping, number, range, age and order. A fixing skips the age check. It counts each drop for each
 * adapter and reason.
 *
 * The translator reads the kind of a source nowhere. A live source and a simulated source of
 * one provider take the same path and give the same tick apart from the source id.
 *
 * The tables are fixed at construction. The service builds a new translator when the catalogue
 * changes. The order check and the counts belong to one translator, and a mutex guards them.
 */
class ORES_MARKETDATA_SERVICE_EXPORT adapter_tick_translator {
public:
    /// The age beyond which a tick is stale.
    static constexpr std::chrono::milliseconds max_tick_age = domain::adapter_limits::max_tick_age;

    /// The largest absolute value a tick may carry.
    static constexpr double max_abs_value = domain::adapter_limits::max_abs_value;

    adapter_tick_translator(std::vector<domain::market_data_source> sources,
                            std::vector<domain::market_data_vendor_instrument> instruments,
                            std::vector<domain::market_data_mapping> mappings);

    /// The tick of @p adapter_id as the pipeline stores it, or the reason it is dropped.
    [[nodiscard]] std::expected<translated_tick, adapter_tick_drop>
    translate(std::string_view adapter_id,
              const messaging::adapter_tick& tick,
              std::chrono::system_clock::time_point now);

    /// How many ticks of @p adapter_id were dropped for @p reason.
    [[nodiscard]] std::uint64_t drops(std::string_view adapter_id, adapter_tick_drop reason) const;

    /// The sources by name that the translator knows.
    [[nodiscard]] std::size_t source_count() const {
        return sources_.size();
    }

private:
    struct instrument_key {
        std::string provider;
        std::string id_scheme;
        std::string external_id;
        std::string field;

        bool operator<(const instrument_key& other) const {
            return std::tie(provider, id_scheme, external_id, field) <
                   std::tie(other.provider, other.id_scheme, other.external_id, other.field);
        }
    };

    std::expected<translated_tick, adapter_tick_drop>
    check(std::string_view adapter_id,
          const messaging::adapter_tick& tick,
          std::chrono::system_clock::time_point now);
    void count(std::string_view adapter_id, adapter_tick_drop reason);

    std::unordered_map<std::string, domain::market_data_source> sources_;
    std::map<instrument_key, std::vector<domain::market_data_mapping>> mappings_;
    /// The items of the mappings that are indices, whose ticks are fixings.
    std::set<std::string> fixing_items_;

    mutable std::mutex mu_;
    std::map<std::pair<std::string, adapter_tick_drop>, std::uint64_t> drops_;
    std::map<std::pair<std::string, std::string>, std::chrono::system_clock::time_point> newest_;
};

}

#endif
