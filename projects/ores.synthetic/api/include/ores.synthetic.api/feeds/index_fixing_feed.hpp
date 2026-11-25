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
#ifndef ORES_SYNTHETIC_API_FEEDS_INDEX_FIXING_FEED_HPP
#define ORES_SYNTHETIC_API_FEEDS_INDEX_FIXING_FEED_HPP

#include "ores.analytics.quant/domain/market_board.hpp"
#include "ores.analytics.quant/service/run_market.hpp"
#include "ores.analytics.quant/service/term_rate_calculator.hpp"
#include "ores.analytics.quant/service/tick_scheduler.hpp"
#include "ores.marketdata.api/domain/i_feed.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.platform/time/time_of_day.hpp"
#include "ores.synthetic.api/export.hpp"
#include "ores.synthetic.api/feeds/adapter_target.hpp"
#include <atomic>
#include <chrono>
#include <cstdint>
#include <expected>
#include <functional>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <vector>

namespace ores::synthetic::feed {

inline constexpr std::string_view index_fixing_feed_kind = "index_fixing";

/**
 * @brief When a fixing is taken each day.
 *
 * An FX fixing is taken at its cut, a time of day on the clock of the cut's zone, so the value
 * is the market at that instant. An index the run states no time for is taken at the first tick
 * of each day of the run.
 */
struct fixing_time final {
    std::optional<ores::platform::time::time_of_day> at;
    /// The IANA zone @c at is read in. Unused when @c at is unset.
    std::string zone;
};

/**
 * @brief The day a fixing falls due at @p observed_at, or nothing.
 *
 * A fixing is due on a weekday after @p last_fixed once the run's clock has passed that day's
 * fixing instant. The day is the date on the zone's clock for a fixing with a time, and the UTC
 * date otherwise.
 *
 * @throws std::runtime_error if the zone is not one the time zone database knows.
 */
[[nodiscard]] ORES_SYNTHETIC_API_EXPORT std::optional<std::chrono::year_month_day>
fixing_due(const fixing_time& when,
           std::chrono::system_clock::time_point observed_at,
           std::optional<std::chrono::year_month_day> last_fixed);

/**
 * @brief How a fixing is stated from its currency's overnight curve, when no
 * feed quotes the index itself.
 *
 * A term index such as SOFR 1M has no publisher in the run and no curve of its
 * own: its rate is the overnight curve's, compounded over the term, or that
 * curve's overnight rate plus a stated spread. The rule is the curve config's
 * own data, carried here so the generator reads one curve instead of a level.
 */
struct derived_fixing final {
    /// The board member of the currency's overnight curve.
    std::string curve_code;
    /// Which rule states the rate.
    ores::analytics::quant::service::term_rate_rule rule =
        ores::analytics::quant::service::term_rate_rule::compounded_overnight;
    /// The term's length in months, from the index's own tenor.
    int term_months = 0;
    /// The overnight index's day-count convention, which annualises the rate.
    ores::analytics::quant::service::day_count_convention_code convention =
        ores::analytics::quant::service::day_count_convention_code::A360;
    /// The spread the spread rule adds, in basis points.
    double spread_basis_points = 0.0;
};

/**
 * @brief What a fixing generator reads, and where it publishes.
 */
struct index_fixing_feed_input final {
    std::string source_name;
    adapter_target target;
    /// The fixing's oresmd address.
    std::string item;
    /// The board member the fixing is taken from: a spot's level, or an index's rate.
    ores::analytics::quant::domain::market_member_kind level_kind =
        ores::analytics::quant::domain::market_member_kind::index_rate;
    /// The board members the level is read from, in order; the first one published is read.
    /// A pair may be quoted by several spot feeds, of which only some run.
    std::vector<std::string> level_codes;
    /// The rule that states the level from the currency's overnight curve, when the
    /// index has no feed of its own. Set, it replaces @c level_codes.
    std::optional<derived_fixing> derived;
    fixing_time when;
    /// How many business days of history the generator states on its first level.
    int history_days = 520;
    std::uint64_t run_seed = 0;
    /// The days the market already holds of the item up to a date, so the history leaves them.
    std::function<std::expected<std::set<std::chrono::year_month_day>, std::string>(
        const std::string& item, std::chrono::year_month_day to)>
        held;
};

/**
 * @brief Publishes the fixings of one item through the simulated fixing publisher's adapter.
 *
 * On the first tick the board holds a level, the generator states the item's history up to that
 * day, leaving the days the market already holds, from the same walk the run's curves state their
 * history with. From then on it fixes the item once a day from the board: an FX fixing from its
 * pair's spot at the cut, an index from the rate its curve states. Each fixing is an adapter tick
 * stamped at midday UTC of its date, which the market data ingest stores as a durable fixing.
 */
class ORES_SYNTHETIC_API_EXPORT index_fixing_feed final : public ores::marketdata::domain::i_feed {
public:
    index_fixing_feed(ores::nats::service::client& nats,
                      index_fixing_feed_input input,
                      ores::analytics::quant::service::run_market& market);

    const std::string& source_name() const override {
        return input_.source_name;
    }
    const std::vector<std::string>& series_uris() const override {
        return series_uris_;
    }
    const std::string& role() const override {
        static const std::string empty;
        return empty;
    }
    const std::string& nats_subject() const override {
        return input_.target.subject;
    }
    std::string_view kind() const override {
        return index_fixing_feed_kind;
    }
    void start() override;
    void stop() override;
    std::uint64_t publish_count() const override {
        return published_.load(std::memory_order_relaxed);
    }

    /// One tick, as the scheduler drives it.
    std::string tick(const ores::analytics::quant::service::tick_context& context);

private:
    void publish(std::chrono::year_month_day date, const std::string& value);

    ores::nats::service::client& nats_;
    index_fixing_feed_input input_;
    ores::analytics::quant::service::run_market& market_;
    std::vector<ores::analytics::quant::domain::market_board::member_id> level_members_;
    /// The overnight curve a derived fixing reads its level from, when it has no members.
    std::optional<ores::analytics::quant::domain::market_board::member_id> curve_member_;

    /// The level to fix: the first published member's, or the rate the rule states
    /// from the overnight curve.
    [[nodiscard]] std::optional<double> level_rate() const;
    std::vector<std::string> series_uris_;
    std::optional<std::chrono::year_month_day> last_fixed_;
    bool history_stated_ = false;
    /// When the read of the held days may be tried again after it failed, on the run's clock.
    std::optional<std::chrono::system_clock::time_point> next_held_read_;
    /// Whether a failed read of the held days has been logged at info already.
    bool held_failure_logged_ = false;
    std::atomic<std::uint64_t> published_{0};
    std::optional<std::size_t> handle_;
};

}

#endif
