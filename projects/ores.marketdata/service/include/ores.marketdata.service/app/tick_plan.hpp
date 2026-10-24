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
#ifndef ORES_MARKETDATA_SERVICE_APP_TICK_PLAN_HPP
#define ORES_MARKETDATA_SERVICE_APP_TICK_PLAN_HPP

#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.api/domain/feed_binding.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.service/export.hpp"
#include <expected>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

namespace ores::marketdata::service::app {

/**
 * @brief Why the ingest loop drops a tick rather than storing it.
 */
enum class tick_drop {
    /// No enabled feed binding names the tick's source.
    unbound_source,
    /// The tick's URI names no datum the oresmd URI codec can read and write.
    unnameable_datum,
    /// The tick's value is not a number.
    not_a_number
};

/**
 * @brief What the URI of a tick names: the datum, its canonical spelling and the URI of its
 * series.
 *
 * Reading and writing a URI costs far more than storing the tick it came on, and a feed repeats
 * the same few URIs. The loop therefore reads each distinct URI once and shares this result.
 */
struct named_datum final {
    datum::market_datum datum;
    /// The canonical spelling of the datum, which the republish subject carries.
    std::string uri;
    /// The URI of the series the datum belongs to.
    std::string series_uri;
};

/**
 * @brief Reads @p uri into the datum it names.
 *
 * @return tick_drop::unnameable_datum when the codec cannot read the URI or write it back.
 */
ORES_MARKETDATA_SERVICE_EXPORT std::expected<std::shared_ptr<const named_datum>, tick_drop>
name_datum(std::string_view uri);

/**
 * @brief One consumer of a tick: the binding it is stored under, and the
 * subject it is republished on.
 */
struct tick_target final {
    domain::feed_binding binding;
    std::string subject;
    /// The tenant and the party of the binding as text, written once so the loop writes none
    /// for each tick.
    std::string tenant_text;
    std::string party_text;
    /// The source of the binding as text, for the same reason.
    std::string source_text;
};

/**
 * @brief The targets of a tick of @p named: one for each binding, each with the republish
 * subject for that binding's tenant and party.
 *
 * The targets depend on the bindings and the datum alone, so the loop builds them once for each
 * source and datum and shares them.
 */
ORES_MARKETDATA_SERVICE_EXPORT std::shared_ptr<const std::vector<tick_target>>
make_targets(const std::vector<domain::feed_binding>& bindings, const named_datum& named);

/**
 * @brief What the ingest loop does with a tick it accepts: the datum it names,
 * its value, and one target per enabled binding of its source.
 */
struct tick_plan final {
    std::shared_ptr<const named_datum> named;
    double value = 0;
    std::shared_ptr<const std::vector<tick_target>> targets;
};

/**
 * @brief Decides what happens to @p tick, given the enabled bindings of its
 * source.
 *
 * The checks run in the order the loop reports them: an empty @p bindings is an
 * unbound source, then the URI must name a datum, then the value must be a
 * number. A tick that passes gets one target per binding, each with the
 * republish subject for that binding's tenant and party.
 *
 * A caller that has read the URI already passes @p named, and the URI is not read again. A
 * caller that has built the targets for this source and datum passes @p targets.
 */
ORES_MARKETDATA_SERVICE_EXPORT std::expected<tick_plan, tick_drop>
plan_tick(const messaging::market_tick& tick,
          const std::vector<domain::feed_binding>& bindings,
          std::shared_ptr<const named_datum> named = nullptr,
          std::shared_ptr<const std::vector<tick_target>> targets = nullptr);

}

#endif
