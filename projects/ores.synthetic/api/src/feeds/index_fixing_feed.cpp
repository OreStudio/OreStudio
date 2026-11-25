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
#include "ores.synthetic.api/feeds/index_fixing_feed.hpp"
#include "ores.analytics.quant/math/rng_stream.hpp"
#include "ores.logging/logger.hpp"
#include "ores.marketdata.api/messaging/adapter_operation_protocol.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.platform/time/zone_offset.hpp"
#include "ores.synthetic.api/feeds/index_fixings.hpp"
#include <chrono>
#include <format>
#include <stdexcept>

namespace ores::synthetic::feed {

namespace {

auto& index_fixing_feed_lg() {
    static auto instance = ores::logging::make_logger("ores.synthetic.api.index_fixing_feed");
    return instance;
}

bool is_weekday(std::chrono::year_month_day date) {
    const std::chrono::weekday day{std::chrono::sys_days{date}};
    return day != std::chrono::Saturday && day != std::chrono::Sunday;
}

/// The business day before @p date, Monday to Friday.
std::chrono::year_month_day previous_business_day(std::chrono::year_month_day date) {
    auto day = std::chrono::sys_days{date} - std::chrono::days{1};
    while (!is_weekday(std::chrono::year_month_day{day}))
        day -= std::chrono::days{1};
    return std::chrono::year_month_day{day};
}

/// The offset @p zone keeps from UTC at @p instant.
///
/// The C++ library is not required to carry std::chrono's time zone database,
/// and libc++ does not, so a call to locate_zone does not compile on macOS even
/// with <chrono> included. Where the library has the database it is asked, and
/// where it has not the offset is read from the system's own.
std::chrono::seconds zone_offset(const std::string& zone,
                                 std::chrono::system_clock::time_point instant) {
#if defined(__cpp_lib_chrono) && __cpp_lib_chrono >= 201907L
    return std::chrono::locate_zone(zone)->get_info(instant).offset;
#else
    const auto offset = ores::platform::time::zone_offset_at(zone, instant);
    if (!offset)
        throw std::runtime_error("the fixing's zone '" + zone +
                                 "' is not one the time zone database describes.");
    return *offset;
#endif
}

}

std::optional<std::chrono::year_month_day>
fixing_due(const fixing_time& when,
           std::chrono::system_clock::time_point observed_at,
           std::optional<std::chrono::year_month_day> last_fixed) {
    using namespace std::chrono;
    year_month_day day;
    if (when.at) {
        const auto offset = zone_offset(when.zone, observed_at);
        const auto local = observed_at + offset;
        day = year_month_day{floor<days>(local)};
        /// The wall clock the cut is read on, held as though it were UTC, which
        /// is what makes the date and the time of day line up.
        const auto wall_clock = floor<days>(local) + when.at->since_midnight();
        /// The offset at the cut is not always the offset now: a saving change
        /// can fall between the two, so the instant is read with its own.
        const auto instant = wall_clock - zone_offset(when.zone, wall_clock - offset);
        if (observed_at < instant)
            return std::nullopt;
    } else {
        day = year_month_day{floor<days>(observed_at)};
    }
    if (!is_weekday(day) || (last_fixed && day <= *last_fixed))
        return std::nullopt;
    return day;
}

index_fixing_feed::index_fixing_feed(ores::nats::service::client& nats,
                                     index_fixing_feed_input input,
                                     ores::analytics::quant::service::run_market& market)
    : nats_(nats)
    , input_(std::move(input))
    , market_(market)
    , series_uris_{input_.item} {
    if (input_.level_codes.empty())
        throw std::invalid_argument("index_fixing_feed: '" + input_.item +
                                    "' names no board member to read its level from.");
    for (const auto& code : input_.level_codes)
        level_members_.push_back(market_.values().member_of(input_.level_kind, code));
}

void index_fixing_feed::start() {
    handle_ =
        market_.loop().add({.series = ores::analytics::quant::math::series_key(input_.source_name),
                            .arrival = {.mode = "periodic", .ticks_per_hour = 360.0},
                            .name = input_.source_name,
                            .group = "fixings"},
                           [this](const ores::analytics::quant::service::tick_context& context) {
                               return tick(context);
                           });
}

void index_fixing_feed::stop() {
    if (handle_) {
        market_.loop().remove(*handle_);
        handle_.reset();
    }
}

void index_fixing_feed::publish(std::chrono::year_month_day date, const std::string& value) {
    ores::marketdata::messaging::adapter_tick tick;
    tick.id_scheme = input_.target.id.id_scheme;
    tick.external_id = input_.target.id.external_id;
    tick.field = input_.target.id.field;
    tick.value = value;
    tick.vendor_time = std::chrono::sys_days{date} + std::chrono::hours{12};
    tick.sequence = published_.fetch_add(1, std::memory_order_relaxed);
    nats_.publish(input_.target.subject, ores::nats::default_wire_codec().encode(tick));
}

std::string index_fixing_feed::tick(const ores::analytics::quant::service::tick_context& context) {
    std::optional<ores::analytics::quant::domain::market_member_view> published;
    for (const auto member : level_members_) {
        const auto view = market_.values().view(member);
        if (view.published) {
            published = view;
            break;
        }
    }
    if (!published)
        return "0";
    const auto& level = *published;

    std::optional<std::chrono::year_month_day> due;
    try {
        due = fixing_due(input_.when, context.observed_at, last_fixed_);
    } catch (const std::exception& e) {
        ORES_LOG_SEV(index_fixing_feed_lg(), warn)
            << "The fixing of '" << input_.item << "' has no instant: " << e.what();
        return "0";
    }

    if (!history_stated_) {
        // The history ends on the day that is due now, or on the business day before today
        // while today's fixing is not yet due.
        const auto to = due.value_or(previous_business_day(std::chrono::year_month_day{
            std::chrono::floor<std::chrono::days>(context.observed_at)}));
        std::set<std::chrono::year_month_day> held;
        if (input_.held) {
            // The read is a request to the market data service on the loop's thread, so a
            // service that does not answer is asked again at most once a minute of the run.
            if (next_held_read_ && context.observed_at < *next_held_read_)
                return "0";
            const auto read = input_.held(input_.item, to);
            if (!read) {
                next_held_read_ = context.observed_at + std::chrono::minutes{1};
                const auto severity =
                    held_failure_logged_ ? ores::logging::debug : ores::logging::info;
                ORES_LOG_SEV(index_fixing_feed_lg(), severity)
                    << "The days the market holds of '" << input_.item
                    << "' are not read yet: " << read.error();
                held_failure_logged_ = true;
                return "0";
            }
            held = *read;
        }
        const auto history = derive_fixing_history(
            level.spot, to, input_.history_days, input_.run_seed, input_.item, held);
        if (!history) {
            ORES_LOG_SEV(index_fixing_feed_lg(), warn)
                << "The history of '" << input_.item << "' is not stated: " << history.error();
        } else {
            for (const auto& fixing : *history)
                publish(fixing.date, fixing.value);
            ORES_LOG_SEV(index_fixing_feed_lg(), info)
                << "Stated " << history->size() << " fixing(s) of '" << input_.item << "' to "
                << to;
        }
        history_stated_ = true;
        last_fixed_ = to;
        return std::to_string(history ? history->size() : 0);
    }

    if (!due)
        return "0";
    publish(*due, std::format("{:.6f}", level.spot));
    last_fixed_ = due;
    return "1";
}

}
