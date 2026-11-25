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
#ifndef ORES_HTTP_ROUTES_SYNTHETIC_IR_CURVE_COVERAGE_HPP
#define ORES_HTTP_ROUTES_SYNTHETIC_IR_CURVE_COVERAGE_HPP

#include "ores.refdata.api/domain/ir_curve_bootstrap_config.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config.hpp"
#include "ores.synthetic.api/messaging/feed_config_protocol.hpp"
#include "ores.utility/codec/shaped_wire_traits.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <chrono>
#include <map>
#include <string>
#include <type_traits>
#include <vector>

namespace ores::http::routes::synthetic {

/**
 * @brief One curve a currency's bootstrap configs build, with its newest publish.
 */
struct ir_curve_of_currency final {
    std::string bootstrap_config_id;
    std::string name;
    /** FUNDING for the curve a currency discounts off, PROJECTION for a term curve. */
    std::string role;
    std::string index_tenor;
    /** The funding curve a projection discounts off; empty for a funding curve. */
    std::string discount_curve_name;
    std::string published_as_of;
    std::string published_state;
    /** The first fact that keeps this curve from being live, or empty when it is. */
    std::string gap;
    std::string gap_reason;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<ir_curve_of_currency>) {
    using ores::utility::codec::field;
    using t = ir_curve_of_currency;
    return ::ores::utility::codec::field_list{field{"bootstrap_config_id", &t::bootstrap_config_id},
                                              field{"name", &t::name},
                                              field{"role", &t::role},
                                              field{"index_tenor", &t::index_tenor},
                                              field{"discount_curve_name", &t::discount_curve_name},
                                              field{"published_as_of", &t::published_as_of},
                                              field{"published_state", &t::published_state},
                                              field{"gap", &t::gap},
                                              field{"gap_reason", &t::gap_reason}};
}

/**
 * @brief One currency's interest-rate curve, and the first gap that stops it being covered.
 */
struct ir_curve_coverage_row final {
    std::string currency_code;
    std::string config_id;
    std::string source_name;
    std::string provider_code;
    std::string bootstrap_config_id;
    std::string bootstrap_config_name;
    std::string published_as_of;
    std::string published_state;
    /** The first fact the currency is missing, or empty when its curve is covered. */
    std::string gap;
    std::string gap_reason;
    /** Every curve of the currency: its funding curve first, then each projection by name. */
    std::vector<ir_curve_of_currency> curves;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<ir_curve_coverage_row>) {
    using ores::utility::codec::field;
    using t = ir_curve_coverage_row;
    return ::ores::utility::codec::field_list{
        field{"currency_code", &t::currency_code},
        field{"config_id", &t::config_id},
        field{"source_name", &t::source_name},
        field{"provider_code", &t::provider_code},
        field{"bootstrap_config_id", &t::bootstrap_config_id},
        field{"bootstrap_config_name", &t::bootstrap_config_name},
        field{"published_as_of", &t::published_as_of},
        field{"published_state", &t::published_state},
        field{"gap", &t::gap},
        field{"gap_reason", &t::gap_reason},
        field{"curves", &t::curves}};
}

/**
 * @brief What a series' newest published set states: when it was built and its state.
 *
 * Both are the store's own text, passed on as it was read rather than parsed and
 * written again: the reader formats the instant for its screen.
 */
struct published_curve_fact final {
    std::string as_of;
    std::string state_code;
};

/**
 * @brief Every read the interest-rate coverage joins, held in memory at once.
 *
 * The four reads are batches: the generation configs, the bootstrap configs, the
 * feeds the service runs, and the newest published set of every output series.
 * The join below is then a walk over those collections, and nothing in it costs a
 * round trip per currency.
 */
struct ir_curve_coverage_input final {
    std::vector<ores::synthetic::messaging::running_feed> runs;
    std::vector<ores::synthetic::domain::ir_curve_generation_config> generation_configs;
    std::vector<ores::refdata::domain::ir_curve_bootstrap_config> bootstrap_configs;
    /** Each output series' newest published set, by the series id as text. */
    std::map<std::string, published_curve_fact> published_by_series;
    /** The clock the staleness of a publish is measured against. */
    std::chrono::system_clock::time_point now{};
};

/**
 * @brief One row per supported currency: its source, its curve and its newest publish.
 *
 * The currency universe is the union of the feed configs and the bootstrap configs,
 * ordered by code. A row states the first gap that stops the currency being
 * covered, in the order a currency falls short of a published curve, and every
 * curve of the currency with the same verdict on each.
 */
[[nodiscard]] std::vector<ir_curve_coverage_row>
ir_curve_coverage(const ir_curve_coverage_input& input);

}

template <>
struct ores::utility::codec::wire_traits<ores::http::routes::synthetic::ir_curve_of_currency>
    : shaped_wire_traits<ores::http::routes::synthetic::ir_curve_of_currency> {};

template <>
struct ores::utility::codec::wire_traits<ores::http::routes::synthetic::ir_curve_coverage_row>
    : shaped_wire_traits<ores::http::routes::synthetic::ir_curve_coverage_row> {};

#endif
