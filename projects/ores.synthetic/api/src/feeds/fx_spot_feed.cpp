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
#include "ores.synthetic.api/feeds/fx_spot_feed.hpp"
#include "ores.analytics.quant/domain/process_parameter_validation.hpp"
#include "ores.analytics.quant/service/process_factory.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/tick_subjects.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.client/market_data_client.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.synthetic.api/feeds/feed_factory.hpp"
#include "ores.synthetic.api/feeds/ir_curve_feed.hpp"
#include "ores.synthetic.api/feeds/producer_subject.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <charconv>
#include <chrono>
#include <cmath>
#include <expected>
#include <format>
#include <stdexcept>

namespace ores::synthetic::feed {

using namespace ores::logging;

namespace {

auto& lg() {
    static auto instance = ores::logging::make_logger("ores.synthetic.api.fx_spot_feed");
    return instance;
}

std::optional<double> number_of(const std::string& text) {
    double out = 0.0;
    const auto [end, ec] = std::from_chars(text.data(), text.data() + text.size(), out);
    if (ec != std::errc{} || end != text.data() + text.size() || !std::isfinite(out) || out <= 0.0)
        return std::nullopt;
    return out;
}

/// The date a fixing item's policy reads on or before, or why the policy cannot be read.
std::expected<std::chrono::year_month_day, std::string>
policy_date(const std::string& policy, const ores::marketdata::domain::market_clock* clock) {
    using namespace std::chrono;
    if (policy == "latest_on_or_before_clock")
        return year_month_day(clock ? clock->today() :
                                      ores::marketdata::domain::system_market_clock{}.today());
    constexpr std::string_view exact = "exact:";
    if (policy.starts_with(exact)) {
        const auto text = policy.substr(exact.size());
        int y = 0;
        unsigned m = 0, d = 0;
        if (text.size() == 10 &&
            std::from_chars(text.data(), text.data() + 4, y).ec == std::errc{} &&
            std::from_chars(text.data() + 5, text.data() + 7, m).ec == std::errc{} &&
            std::from_chars(text.data() + 8, text.data() + 10, d).ec == std::errc{}) {
            const year_month_day date{year{y}, month{m}, day{d}};
            if (date.ok())
                return date;
        }
    }
    return std::unexpected("policy '" + policy +
                           "' is not latest_on_or_before_clock or exact:<date>");
}

/// The level and the measured spread a fixing feed starts from, and the mixture's deviations
/// scaled to it; or, when no item has a fixing, the record that the feed kept its fixed level.
struct resolved_start final {
    ores::marketdata::domain::feed_start source;
    std::optional<std::vector<double>> stdevs;
};

resolved_start
resolve_fixing_start(ores::nats::service::nats_client& auth_nats,
                     const ores::synthetic::domain::fx_spot_generation_config& cfg,
                     const std::vector<ores::synthetic::domain::gmm_component>& components,
                     std::vector<ores::synthetic::domain::fx_spot_fixing_item> items,
                     const ores::marketdata::domain::market_clock* clock) {
    resolved_start out;
    out.source.level = cfg.gmm_initial_price;
    std::ranges::sort(items, {}, &ores::synthetic::domain::fx_spot_fixing_item::rank);

    auto delegated = auth_nats.with_delegation({});
    ores::marketdata::client::market_data_client md(delegated);
    std::vector<std::string> skipped;
    for (const auto& item : items) {
        const auto orientation =
            orientation_of(item.item, cfg.base_currency_code, cfg.quote_currency_code);
        if (!orientation) {
            skipped.push_back(orientation.error());
            continue;
        }
        const auto date = policy_date(item.policy, clock);
        if (!date) {
            skipped.push_back(item.item + ": " + date.error());
            continue;
        }
        // The feed reads fixings for the party that owns its config: a fixing is entitled by
        // party, and the service that starts the feed holds no party of its own.
        const auto party_id = boost::uuids::to_string(cfg.party_id);
        const auto latest = md.get_fixing({.item = item.item,
                                           .provider_code = item.provider_code,
                                           .party_id = party_id,
                                           .on_or_before = *date});
        if (!latest) {
            skipped.push_back(item.item + ": " + latest.error());
            continue;
        }
        // An exact policy names one date; the latest fixing on or before it is that date's or
        // none.
        const bool exact = item.policy != "latest_on_or_before_clock";
        if (!latest->fixing_date || (exact && *latest->fixing_date != *date)) {
            skipped.push_back(item.item + ": no fixing on or before " +
                              std::format("{:%F}", std::chrono::sys_days(*date)));
            continue;
        }
        const auto value = number_of(latest->value);
        if (!value) {
            skipped.push_back(item.item + ": the fixing '" + latest->value + "' is not a price");
            continue;
        }

        auto& s = out.source;
        s.from_fixing = true;
        s.item = item.item;
        s.provider_code = latest->provider_code;
        s.vendor_id = latest->vendor_id;
        s.inverted = *orientation == item_orientation::inverted;
        s.fixing_date = latest->fixing_date;
        s.received_at = latest->received_at;
        s.level = s.inverted ? 1.0 / *value : *value;
        s.window_days = item.window_days;

        const auto window = md.get_fixing_window({.item = item.item,
                                                  .provider_code = item.provider_code,
                                                  .party_id = party_id,
                                                  .to = *latest->fixing_date,
                                                  .days = item.window_days});
        if (!window) {
            s.note = "The volatility was not measured: " + window.error();
            return out;
        }
        std::vector<double> values;
        for (const auto& v : window->values)
            if (const auto n = number_of(v))
                values.push_back(*n);
        s.used = window->used;
        const auto spread = business_day_log_return_spread(values);
        if (!spread) {
            s.note = "The window holds too few fixings to measure a volatility.";
            return out;
        }
        s.hourly_spread = hourly_spread_of(*spread);
        std::vector<mixture_component> mixture;
        for (const auto& c : components)
            mixture.push_back({.mean = c.mean, .stdev = c.stdev, .weight = c.weight});
        const auto scaled = scaled_stdevs(mixture, *s.hourly_spread, cfg.ticks_per_hour);
        if (!scaled) {
            s.note = "The configured mixture was kept: " + scaled.error();
            return out;
        }
        out.stdevs = *scaled;
        return out;
    }
    out.source.note = items.empty() ? "The config names no fixing item." : "No item has a fixing";
    for (const auto& reason : skipped)
        out.source.note += "; " + reason;
    return out;
}

} // namespace

ORES_SYNTHETIC_API_EXPORT std::shared_ptr<fx_spot_feed>
make_fx_spot_feed(ores::nats::service::client& nats,
                  ores::nats::service::nats_client& auth_nats,
                  const ores::synthetic::domain::fx_spot_generation_config& cfg,
                  const std::vector<ores::synthetic::domain::gmm_component>& components,
                  const std::vector<ores::synthetic::domain::fx_spot_fixing_item>& fixing_items,
                  ores::analytics::quant::service::tick_scheduler& scheduler,
                  std::optional<adapter_target> adapter,
                  const ores::marketdata::domain::market_clock* clock) {
    // The config states the pair twice, as two currencies and as a URI. They must agree.
    const auto expected =
        "oresmd://fx/" + cfg.base_currency_code +
        "?type=quote&instrument=fx_spot&quote=rate&ccy=" + cfg.quote_currency_code;
    if (cfg.oresmd_uri != expected)
        throw std::invalid_argument("make_fx_spot_feed: config '" + cfg.oresmd_uri +
                                    "' is not the quote of the pair " + cfg.base_currency_code +
                                    "/" + cfg.quote_currency_code + ": expected '" + expected +
                                    "'.");
    if (components.empty())
        throw std::invalid_argument("make_fx_spot_feed: config '" + cfg.oresmd_uri +
                                    "' has no GMM components.");

    std::vector<double> means, stdevs, weights;
    means.reserve(components.size());
    stdevs.reserve(components.size());
    weights.reserve(components.size());
    for (const auto& c : components) {
        means.push_back(c.mean);
        stdevs.push_back(c.stdev);
        weights.push_back(c.weight);
    }

    // "fixing" starts from the first ranked item with a fixing and sizes the mixture to its
    // spread; "fixed" uses the stored value as-is. The start records the level either way, so a
    // fixed feed reads as one that kept its level rather than as one that started nowhere.
    ores::marketdata::domain::feed_start start;
    start.level = cfg.gmm_initial_price;
    double initial_price = cfg.gmm_initial_price;
    if (cfg.price_source == "fixing") {
        auto resolved = resolve_fixing_start(auth_nats, cfg, components, fixing_items, clock);
        start = std::move(resolved.source);
        initial_price = start.level;
        if (resolved.stdevs)
            stdevs = std::move(*resolved.stdevs);
        BOOST_LOG_SEV(lg(), info) << "Feed " << cfg.source_name << " starts at " << initial_price
                                  << (start.from_fixing ?
                                          " from " + start.item + " (" + start.provider_code + ")" +
                                              (start.inverted ? ", inverted" : "") :
                                          std::string(", its fixed level"))
                                  << (start.note.empty() ? "" : ". " + start.note);
    }

    auto process = ores::analytics::quant::service::process_factory::make_process(
        cfg.process_type,
        std::move(means),
        std::move(stdevs),
        std::move(weights),
        initial_price,
        price_stream_for(scheduler.run_seed(), cfg.source_name));

    const auto arrival = to_arrival_spec(cfg);
    const auto compatibility = ores::analytics::quant::domain::validate_step_compatibility(
        arrival.mode, process->supports_variable_step());
    if (!compatibility.valid)
        throw std::invalid_argument("make_fx_spot_feed: " + compatibility.message);

    auto feed = std::make_shared<fx_spot_feed>(nats,
                                               cfg.oresmd_uri,
                                               cfg.source_name,
                                               producer_subject(cfg.source_name),
                                               std::move(process),
                                               arrival,
                                               scheduler,
                                               cfg.sensitivity,
                                               std::move(adapter));
    feed->record_start_source(std::move(start));
    return feed;
}

fx_spot_feed::fx_spot_feed(
    ores::nats::service::client& nats,
    std::string quote_uri,
    std::string source_name,
    std::string nats_subject,
    std::unique_ptr<ores::analytics::quant::domain::i_stochastic_process> process,
    const ores::analytics::quant::domain::arrival_spec& arrival,
    ores::analytics::quant::service::tick_scheduler& scheduler,
    double sensitivity,
    std::optional<adapter_target> adapter)
    : nats_(nats)
    , quote_uri_(std::move(quote_uri))
    , source_name_(std::move(source_name))
    , process_(std::move(process))
    , arrival_(arrival)
    , scheduler_(scheduler)
    , sensitivity_(sensitivity)
    , ticks_per_second_(arrival.ticks_per_hour / 3600.0)
    , nats_subject_(std::move(nats_subject))
    , display_subject_(ores::marketdata::domain::feed_display_subject_for(source_name_))
    , adapter_(std::move(adapter)) {

    if (!std::isfinite(sensitivity_) || sensitivity_ <= 0.0)
        throw std::invalid_argument(
            "fx_spot_feed: sensitivity must be finite and strictly positive");
    if (!process_)
        throw std::invalid_argument("fx_spot_feed: process must not be null");
    // The scheduler builds the law when the feed starts, so a spec it would refuse is refused
    // here, when the feed is built.
    const auto arrival_check = ores::analytics::quant::domain::validate_arrival_process_parameters(
        arrival_.mode, arrival_.ticks_per_hour, arrival_.branching_ratio, arrival_.decay);
    if (!arrival_check.valid)
        throw std::invalid_argument("fx_spot_feed: " + arrival_check.message);
    namespace datum = ores::marketdata::datum;
    const auto read = datum::oresmd_uri_codec::read(quote_uri_);
    if (!read)
        throw std::invalid_argument("fx_spot_feed: '" + quote_uri_ +
                                    "' names no datum: " + read.error());
    if (read->type() != datum::instrument_type::fx_spot || read->is_series())
        throw std::invalid_argument("fx_spot_feed: '" + quote_uri_ + "' is not an fx_spot quote.");
    quote_uri_ = datum::oresmd_uri_codec::write(*read).value();
    series_uris_ = {datum::oresmd_uri_codec::write(datum::series_of(*read)).value()};
}

const std::string& fx_spot_feed::source_name() const {
    return source_name_;
}

const std::vector<std::string>& fx_spot_feed::series_uris() const {
    return series_uris_;
}

const std::string& fx_spot_feed::role() const {
    static const std::string empty;
    return empty;
}

void fx_spot_feed::start() {
    handle_ = scheduler_.add({.series = ores::analytics::quant::math::series_key(source_name_),
                              .arrival = arrival_,
                              .sensitivity = sensitivity_,
                              .name = source_name_,
                              .group = "fx"},
                             [this](const ores::analytics::quant::service::tick_context& context) {
                                 return tick(context);
                             });
}

std::string fx_spot_feed::tick(const ores::analytics::quant::service::tick_context& context) {
    const auto n = published_.fetch_add(1, std::memory_order_relaxed) + 1;

    // Simulated time, not wall time: the run's market speed says how many simulated seconds a
    // unit of market time spans, so the feed's rate decides how often it looks and not how fast
    // its world runs, and a busy market brings ticks sooner without moving the price further. An
    // unusable gap still publishes a tick, so it advances the process by one nominal tick.
    const auto elapsed_ticks =
        context.simulated_seconds > 0.0 ? context.simulated_seconds * ticks_per_second_ : 1.0;
    process_->set_tail_tilt(context.tail_tilt);
    const auto value = std::format("{}", process_->next(elapsed_ticks));
    if (n == 1 || n % 100 == 0) {
        BOOST_LOG_SEV(lg(), ores::logging::info)
            << "SYNTHETIC PUBLISH: subject='" << published_subject() << "' oresmd_uri='"
            << quote_uri_ << "' count=" << n << " value=" << value;
    }
    const auto now = context.observed_at;
    const auto& codec = ores::nats::default_wire_codec();
    ores::marketdata::messaging::market_tick shown;
    shown.oresmd_uri = quote_uri_;
    shown.value = value;
    shown.observation_time = now;
    shown.source = source_name_;
    nats_.publish(display_subject_, codec.encode(shown));
    // A quote is perishable, so it goes out on core NATS without a wait for an
    // acknowledgement. A producer that waits limits its own rate to the round
    // trip of the broker.
    if (adapter_) {
        ores::marketdata::messaging::adapter_tick tick;
        tick.id_scheme = adapter_->id.id_scheme;
        tick.external_id = adapter_->id.external_id;
        tick.field = adapter_->id.field;
        tick.value = value;
        tick.vendor_time = now;
        tick.sequence = published_.load(std::memory_order_relaxed);
        const auto plan = adapter_->faults ? adapter_->faults->get() : fault_plan{};
        for (const auto& out : faulted_ticks(plan, tick, previous_tick_))
            nats_.publish(adapter_->subject, codec.encode(out));
        return value;
    }
    nats_.publish(nats_subject_, codec.encode(shown));
    return value;
}

void fx_spot_feed::stop() {
    if (handle_) {
        scheduler_.remove(*handle_);
        handle_.reset();
    }
}

}
