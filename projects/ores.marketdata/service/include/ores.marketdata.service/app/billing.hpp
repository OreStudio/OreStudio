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
#ifndef ORES_MARKETDATA_SERVICE_APP_BILLING_HPP
#define ORES_MARKETDATA_SERVICE_APP_BILLING_HPP

#if defined(_MSC_VER)
#    pragma once
#endif

#include "ores.marketdata.api/domain/delivery_count.hpp"
#include "ores.marketdata.api/domain/demand_interval.hpp"
#include "ores.marketdata.api/domain/market_data_source.hpp"
#include "ores.marketdata.api/domain/rate_card_component.hpp"
#include "ores.marketdata.api/domain/subscription_interval.hpp"
#include "ores.marketdata.service/export.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <string>
#include <vector>

namespace ores::marketdata::service::app {

/// The usage events a bill is made from.
struct usage_events {
    std::vector<domain::subscription_interval> subscriptions;
    std::vector<domain::demand_interval> demands;
    std::vector<domain::delivery_count> deliveries;
};

/// A half open period: @c from is in it and @c to is not.
struct billing_period {
    std::chrono::system_clock::time_point from;
    std::chrono::system_clock::time_point to;
};

/// One line of the bill of a source: a component of the rate card, the quantity it counted and
/// what it costs. Every amount is a whole number of millionths of the currency.
struct bill_line {
    std::string driver;
    std::string licence_right;
    std::int64_t quantity = 0;
    std::int64_t charge_micros = 0;
    std::int64_t total_micros = 0;
};

/**
 * @brief What the platform owes the vendor of a source for a period.
 *
 * @c shadow is true when nobody pays the bill because the source is simulated. It labels the bill
 * and changes no line and no amount.
 */
struct source_bill {
    boost::uuids::uuid source_id;
    std::string source_name;
    std::string currency;
    bool shadow = false;
    billing_period period;
    std::vector<bill_line> lines;
    std::int64_t total_micros = 0;
};

/// What one party bears of one component of a source bill.
struct party_charge {
    ores::utility::uuid::tenant_id tenant_id = ores::utility::uuid::tenant_id::system();
    boost::uuids::uuid party_id;
    boost::uuids::uuid source_id;
    std::string driver;
    std::string licence_right;
    std::int64_t total_micros = 0;
};

/// The split of a source bill among parties.
struct allocation {
    std::vector<party_charge> charges;
    /// The part of the bill that no party bears because no party drives it, such as a fixed fee in
    /// a period with no subscriber. The charges and this add up to the total of the bill.
    std::int64_t unallocated_micros = 0;
};

/**
 * @brief Divides @p total into @p parts whole numbers that add up to @p total and differ by at
 * most one, the larger ones first.
 */
[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT std::vector<std::int64_t>
split_evenly(std::int64_t total, std::size_t parts);

/**
 * @brief Divides @p total in proportion to @p weights into whole numbers that add up to
 * @p total. The remainder goes to the largest fractions, the earlier weight first. A total weight
 * of zero gives zero shares.
 */
[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT std::vector<std::int64_t>
split_weighted(std::int64_t total, const std::vector<std::int64_t>& weights);

/**
 * @brief Applies the rate card of @p source to the usage events of @p period.
 *
 * Only the components of the source that apply in the period count, and only the events of the
 * source. The kind of the source is read to label the bill and nowhere else.
 */
[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT source_bill
rate_source(const domain::market_data_source& source,
            const std::vector<domain::rate_card_component>& card,
            const usage_events& events,
            const billing_period& period);

/**
 * @brief Splits @p bill among the parties by the driver of each component.
 *
 * - per_instrument: the charge of each instrument is shared among the parties that demanded it.
 * - per_party: each active subscriber bears the charge.
 * - per_licence: each party that holds the right bears the charge.
 * - per_thousand_ticks: the total is split by the deliveries counted for each party.
 * - fixed_per_period: the total is split equally among the active subscribers.
 */
[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT allocation allocate(const source_bill& bill,
                                                                 const usage_events& events);

/// The bills of a period: one for each source with a rate card, the charge of each party, and the
/// part that no party bears.
struct bill_report {
    std::vector<source_bill> bills;
    std::vector<party_charge> charges;
    std::int64_t unallocated_micros = 0;
};

/**
 * @brief Rates and allocates every source in @p sources for @p period.
 *
 * A source with no component of its rate card in the period has no bill. An empty
 * @p source_name selects every source, and a name selects the source of that name.
 */
[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT bill_report
make_report(const std::vector<domain::market_data_source>& sources,
            const std::vector<domain::rate_card_component>& card,
            const usage_events& events,
            const billing_period& period,
            const std::string& source_name = {});

}

#endif
