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
#ifndef ORES_SYNTHETIC_API_FEEDS_DERIVED_FX_VOL_FEED_HPP
#define ORES_SYNTHETIC_API_FEEDS_DERIVED_FX_VOL_FEED_HPP

#include "ores.synthetic.api/export.hpp"
#include "ores.synthetic.api/feeds/derived_fx_vol.hpp"
#include <chrono>
#include <cstdint>
#include <expected>
#include <functional>
#include <map>
#include <set>
#include <string>
#include <vector>

namespace ores::synthetic::feed {

/**
 * @brief What the derived source reads, injected so the feed holds no connection.
 *
 * fixings returns the window of one fixing item that ends on a day; convention returns
 * the quoting convention of a pair.
 */
struct derived_fx_readers final {
    std::function<std::expected<dated_series, std::string>(const std::string& item,
                                                           std::chrono::year_month_day to)>
        fixings;
    std::function<std::expected<refdata::domain::fx_option_convention, std::string>(
        const derived_fx_pair& pair)>
        convention;
};

/**
 * @brief The derived source of FX volatility quotes, as an adapter serves it.
 *
 * Its universe is the vendor ids the catalogue holds for it. It answers demand for ids
 * of that universe, and on a refresh publishes, for each demanded pair whose legs hold a
 * day of fixings later than the last it published, the quotes of that day. It publishes
 * the current day only and never a past one, since a past day is computed again from the
 * fixings when it is needed.
 */
class ORES_SYNTHETIC_API_EXPORT derived_fx_vol_feed final {
public:
    using publish_fn = std::function<void(const marketdata::messaging::adapter_tick&)>;

    derived_fx_vol_feed(std::string source_name,
                        std::string provider_code,
                        derived_fx_readers readers,
                        analytics::quant::math::fx_derivation_settings settings = {});

    /// Takes the catalogue's vendor ids; an id outside the derived notation is left out.
    void set_universe(const std::vector<std::string>& external_ids);

    [[nodiscard]] bool has_universe() const {
        return !universe_.empty();
    }

    [[nodiscard]] const std::string& source_name() const {
        return source_name_;
    }

    [[nodiscard]] marketdata::messaging::adapter_announce_request announcement() const;

    [[nodiscard]] marketdata::messaging::adapter_demand_response
    answer(const marketdata::messaging::adapter_demand_request& request) const;

    marketdata::messaging::adapter_demand_response
    demand(const marketdata::messaging::adapter_demand_request& request);

    /**
     * @brief Publishes each demanded pair's quotes of its latest day not yet sent.
     *
     * A pair that cannot be derived is left out and named in failures(); the others are
     * still sent. Returns how many ticks went out.
     */
    std::size_t refresh(std::chrono::year_month_day today, const publish_fn& publish);

    /// Why each pair left out of the last refresh was left out.
    [[nodiscard]] const std::vector<std::string>& failures() const {
        return failures_;
    }

private:
    std::expected<std::vector<marketdata::messaging::adapter_tick>, std::string>
    ticks_of(const derived_fx_pair& pair,
             std::chrono::year_month_day today,
             std::chrono::year_month_day& day);

    std::string source_name_;
    std::string provider_code_;
    derived_fx_readers readers_;
    analytics::quant::math::fx_derivation_settings settings_;
    std::set<std::string> universe_;
    std::set<std::string> demanded_;
    std::map<std::string, std::chrono::year_month_day> last_sent_;
    std::vector<std::string> failures_;
    std::uint64_t sequence_ = 0;
};

}

#endif
