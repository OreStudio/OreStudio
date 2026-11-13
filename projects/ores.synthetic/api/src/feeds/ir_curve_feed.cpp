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
#include "ores.synthetic.api/feeds/ir_curve_feed.hpp"
#include "ores.analytics.quant/domain/process_parameter_validation.hpp"
#include "ores.analytics.quant/service/curve_instrument_pricer.hpp"
#include "ores.analytics.quant/service/process_factory.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.api/domain/tick_subjects.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.client/market_data_client.hpp"
#include "ores.marketdata.core/oresmd/pillar_quote.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_parameter_mapping.hpp"
#include "ores.synthetic.api/feeds/feed_factory.hpp"
#include "ores.synthetic.api/feeds/producer_subject.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cctype>
#include <charconv>
#include <chrono>
#include <cmath>
#include <format>
#include <optional>
#include <stdexcept>

namespace ores::synthetic::feed {

using namespace ores::logging;

namespace {

auto& ir_curve_feed_lg() {
    static auto instance = ores::logging::make_logger("ores.synthetic.api.ir_curve_feed");
    return instance;
}

} // namespace

ir_curve_feed::ir_curve_feed(
    ores::nats::service::client& nats,
    std::string source_name,
    std::string nats_subject,
    std::string currency_code,
    std::string index_tenor,
    std::string role,
    std::unique_ptr<ores::analytics::quant::domain::i_yield_curve_process> process,
    const ores::analytics::quant::domain::arrival_spec& arrival,
    std::vector<ir_curve_resolved_entry> entries,
    ores::analytics::quant::service::run_market& market,
    double step_speed,
    double sensitivity,
    std::vector<ir_pillar_quote_target> adapter_targets,
    std::chrono::year_month_day curve_spot_date)
    : nats_(nats)
    , source_name_(std::move(source_name))
    , nats_subject_(std::move(nats_subject))
    , currency_code_(std::move(currency_code))
    , role_(std::move(role))
    , process_(std::move(process))
    , step_seconds_(step_speed / (arrival.ticks_per_hour / 3600.0))
    , arrival_(arrival)
    , market_(market)
    , sensitivity_(sensitivity)
    , entries_(std::move(entries)) {

    if (!std::isfinite(sensitivity_) || sensitivity_ <= 0.0)
        throw std::invalid_argument(
            "ir_curve_feed: sensitivity must be finite and strictly positive");
    if (!process_)
        throw std::invalid_argument("ir_curve_feed: process must not be null");
    // The scheduler builds the law when the feed starts, so a spec it would refuse is refused
    // here, when the feed is built.
    const auto arrival_check = ores::analytics::quant::domain::validate_arrival_process_parameters(
        arrival_.mode, arrival_.ticks_per_hour, arrival_.branching_ratio, arrival_.decay);
    if (!arrival_check.valid)
        throw std::invalid_argument("ir_curve_feed: " + arrival_check.message);
    if (arrival.ticks_per_hour <= 0.0)
        throw std::invalid_argument("ir_curve_feed: ticks_per_hour must be positive");
    if (entries_.empty())
        throw std::invalid_argument("ir_curve_feed: entries must not be empty");

    // The run's board holds this curve, so a surface on a pair that names this currency
    // solves its strikes against the same discounts this feed prices its pillars off. The
    // member is taken once, here, so a tick writes by index.
    {
        auto& shared = market_.values();
        board_member_ = shared.member_of(ores::analytics::quant::domain::market_member_kind::curve,
                                         currency_code_);
        board_spot_date_ = curve_spot_date;
    }

    display_subject_ = ores::marketdata::domain::feed_display_subject_for(source_name_);

    // One curve quotes every pillar, so the targets are matched to the entries here and an entry
    // with no id fails the build: a pillar the adapter cannot name has nowhere to publish.
    for (const auto& e : entries_) {
        const auto found = std::ranges::find_if(adapter_targets, [&e](const auto& t) {
            return t.start_tenor_code == e.start_tenor_code && t.end_tenor_code == e.point_id;
        });
        if (found == adapter_targets.end()) {
            if (!adapter_targets.empty())
                throw std::invalid_argument("ir_curve_feed: the pillar " + e.start_tenor_code +
                                            " to " + e.point_id + " has no adapter id");
            adapter_targets_.emplace_back(std::nullopt);
            continue;
        }
        adapter_targets_.emplace_back(*found);
    }

    // Each pillar is a quote, and the datum is built from the market data model rather than
    // from a key: the identity of a market datum is its oresmd URI, and a key is a boundary
    // format. The datums are built once, here, so a pillar the codec refuses fails when the
    // feed is built and not on the first tick.
    for (const auto& e : entries_) {
        const ores::marketdata::core::pillar_quote quote{
            currency_code_, index_tenor, e.start_tenor_code, e.point_id, e.start_date, e.end_date};
        const auto made = ores::marketdata::core::pillar_quote_datum(quote);
        if (!made)
            throw std::invalid_argument("ir_curve_feed: the pillar " + e.start_tenor_code + " to " +
                                        e.point_id + " names no datum: " + made.error());
        const auto uri = ores::marketdata::datum::oresmd_uri_codec::write(*made);
        if (!uri)
            throw std::invalid_argument("ir_curve_feed: the pillar " + e.start_tenor_code + " to " +
                                        e.point_id + " has no item: " + uri.error());
        datum_uris_.push_back(*uri);
        auto series = ores::marketdata::datum::oresmd_uri_codec::write(
            ores::marketdata::datum::series_of(*made));
        if (!series)
            throw std::invalid_argument("ir_curve_feed: the pillar " + e.start_tenor_code + " to " +
                                        e.point_id + " has no series: " + series.error());
        if (std::ranges::find(series_uris_, *series) == series_uris_.end())
            series_uris_.push_back(std::move(*series));
    }
}

void ir_curve_feed::start() {
    handle_ =
        market_.loop().add({.series = ores::analytics::quant::math::series_key(source_name_),
                            .arrival = arrival_,
                            .sensitivity = sensitivity_,
                            .name = source_name_,
                            .group = "interest_rates"},
                           [this](const ores::analytics::quant::service::tick_context& context) {
                               return tick(context);
                           });
}

std::string ir_curve_feed::tick(const ores::analytics::quant::service::tick_context& context) {
    const auto n = published_.fetch_add(1, std::memory_order_relaxed) + 1;
    const double elapsed_ticks =
        context.simulated_seconds > 0.0 ? context.simulated_seconds / step_seconds_ : 1.0;

    // The scheduler decides when a curve publishes, and the model steps one tick of its own time.
    // A process whose law is written for an arbitrary step takes the simulated time instead, so
    // a curve that ticks later than its rate still advances the time it skipped.
    if (process_->supports_variable_step())
        process_->next(elapsed_ticks);
    else
        process_->next();
    const auto now = context.observed_at;

    // One publish of the whole curve, from the same state the pillars were priced off: the
    // board's reader interpolates it rather than rebuilding it from the quotes.
    if (board_member_) {
        append_curve_points(board_spot_date_, entries_, *process_, board_points_);
        market_.values().publish_curve(*board_member_, board_spot_date_, board_points_, now);
    }

    for (std::size_t i = 0; i < entries_.size(); ++i) {
        const auto& e = entries_[i];
        const auto value = std::format("{}", price_ir_curve_entry(*process_, e));

        // Each market tick names one pillar: the datum built at construction from the entry's own
        // dates and the index tenor, so the series it lands in carries the instrument's identity
        // rather than the pipeline's vocabulary. It goes out before the adapter branch, as a spot
        // feed's does, because a screen reads this copy: a pillar served by an adapter is still a
        // pillar the feed published, and dropping its market tick left the feed silent on every
        // screen while its ticks were stored.
        {
            ores::marketdata::messaging::market_tick shown;
            shown.oresmd_uri = datum_uris_[i];
            shown.value = value;
            shown.observation_time = now;
            shown.source = source_name_;

            const auto shown_encoded = ores::nats::default_wire_codec().encode(shown);
            nats_.publish(display_subject_, shown_encoded);
        }

        // A pillar that serves an adapter publishes the adapter's own tick under the pillar's
        // vendor id, as an adapter of a live provider does. The service maps the id to its datum
        // and finds the parties, so the tick names neither.
        if (const auto& target = adapter_targets_[i]) {
            ores::marketdata::messaging::adapter_tick tick;
            tick.id_scheme = target->target.id.id_scheme;
            tick.external_id = target->target.id.external_id;
            tick.field = target->target.id.field;
            tick.value = value;
            tick.vendor_time = now;
            tick.sequence = published_.load(std::memory_order_relaxed);
            const auto plan = target->target.faults ? target->target.faults->get() : fault_plan{};
            std::optional<ores::marketdata::messaging::adapter_tick> previous;
            if (const auto found = previous_ticks_.find(tick.external_id);
                found != previous_ticks_.end())
                previous = found->second;
            for (const auto& out : faulted_ticks(plan, tick, previous))
                nats_.publish(target->target.subject, ores::nats::default_wire_codec().encode(out));
            previous_ticks_.insert_or_assign(tick.external_id, tick);
            continue;
        }

        // A pillar no adapter serves still goes to the market as a tick of its own.
        ores::marketdata::messaging::market_tick tick;
        tick.oresmd_uri = datum_uris_[i];
        tick.value = value;
        tick.observation_time = now;
        tick.source = source_name_;

        nats_.publish(nats_subject_, ores::nats::default_wire_codec().encode(tick));
    }
    if (n == 1 || n % 100 == 0) {
        // The subject a pillar with an adapter target went out on, taken from the pillar's own
        // target rather than from the producer subject the feed does not use for it. The log is
        // what a person reads to find out where the feed's ticks are, so it names the family the
        // ticks actually travelled on.
        BOOST_LOG_SEV(ir_curve_feed_lg(), info)
            << "SYNTHETIC CURVE PUBLISH: source='" << source_name_ << "' batch=" << n
            << " points=" << entries_.size() << " subject='" << published_subject() << "'";
    }
    return std::to_string(entries_.size());
}

const std::string& ir_curve_feed::published_subject() const {
    for (const auto& target : adapter_targets_)
        if (target)
            return target->target.subject;
    return nats_subject_;
}

void ir_curve_feed::stop() {
    if (handle_) {
        market_.loop().remove(*handle_);
        handle_.reset();
    }
}

namespace {

/// A rate as the process reads it, which may be negative, or nothing when the text is not one.
std::optional<double> rate_of(const std::string& text) {
    double out = 0.0;
    const auto [end, ec] = std::from_chars(text.data(), text.data() + text.size(), out);
    if (ec != std::errc{} || end != text.data() + text.size() || !std::isfinite(out))
        return std::nullopt;
    return out;
}

}

ir_curve_start resolve_ir_start(ores::nats::service::nats_client& auth_nats,
                                const ores::synthetic::domain::ir_curve_generation_config& cfg,
                                const calibration_policy& policy,
                                const ores::marketdata::domain::market_clock* clock) {
    ir_curve_start out;
    const auto refuse_if_policy_says = [&] {
        out.refused = policy.fallback_code == "refuse";
        if (out.refused)
            out.source.note += "; the class policy refuses to start without a fixing.";
    };

    auto delegated = auth_nats.with_delegation({});
    ores::marketdata::client::market_data_client md(delegated);
    const auto series = md.list_series();
    if (!series) {
        out.source.note = "The series of the overnight rates could not be read: " + series.error();
        refuse_if_policy_says();
        return out;
    }
    std::vector<calibration_candidate> candidates;
    for (const auto& s : *series)
        if (const auto currency = overnight_currency_of(s.oresmd_uri))
            candidates.push_back({.item = s.oresmd_uri,
                                  .provider_code = s.provider_code,
                                  .base_currency = *currency,
                                  .quote_currency = {}});
    const auto items = derive_overnight_items(policy, cfg.currency_code, candidates);

    const std::chrono::year_month_day today(
        clock ? clock->today() : ores::marketdata::domain::system_market_clock{}.today());
    // A fixing is entitled by party, and the service that starts the curve holds no party of its
    // own, so it reads as the party that owns the config.
    const auto party_id = boost::uuids::to_string(cfg.party_id);
    std::vector<std::string> skipped;
    for (const auto& item : items) {
        const auto latest = md.get_fixing({.item = item.item,
                                           .provider_code = item.provider_code,
                                           .party_id = party_id,
                                           .on_or_before = today});
        if (!latest || !latest->fixing_date) {
            skipped.push_back(
                item.item + ": " +
                (latest ? std::string("no fixing on or before today") : latest.error()));
            continue;
        }
        const auto age = static_cast<int>(
            (std::chrono::sys_days(today) - std::chrono::sys_days(*latest->fixing_date)).count());
        if (policy.window_days > 0 && age > policy.window_days) {
            skipped.push_back(item.item + ": its newest fixing is " + std::to_string(age) +
                              " days old, beyond the policy's " +
                              std::to_string(policy.window_days) + "-day window");
            continue;
        }
        const auto rate = rate_of(latest->value);
        const auto published = ores::utility::decimal::decimal::from_string(latest->value);
        if (!rate || !published) {
            skipped.push_back(item.item + ": the fixing '" + latest->value + "' is not a rate");
            continue;
        }
        auto& s = out.source;
        s.from_fixing = true;
        s.item = item.item;
        s.provider_code = latest->provider_code;
        s.vendor_id = latest->vendor_id;
        s.value = *published;
        s.fixing_date = latest->fixing_date;
        s.received_at = latest->received_at;
        s.age_days = age;
        // Every adapter stores the decimal of a published percent; a rate above one in size is
        // a percent stored by mistake, which would start the curve a hundred times too high.
        if (std::abs(*rate) > 1.0)
            BOOST_LOG_SEV(ir_curve_feed_lg(), ores::logging::warn)
                << "Curve " << cfg.source_name << " starts from " << latest->value << " of "
                << item.item << ", which is above one in size: is it a percent?";
        out.rate = *rate;
        return out;
    }

    out.source.note = items.empty() ?
                          "No series of the class policy publishes an overnight rate in " +
                              cfg.currency_code + ", so the curve keeps its fixed initial rate" :
                          "No overnight rate in " + cfg.currency_code +
                              " has a fixing, so the curve keeps its fixed initial rate";
    for (const auto& reason : skipped)
        out.source.note += "; " + reason;
    refuse_if_policy_says();
    return out;
}

std::shared_ptr<ir_curve_feed> make_ir_curve_feed(
    ores::nats::service::client& nats,
    const ores::synthetic::domain::ir_curve_generation_config& cfg,
    const std::vector<ores::synthetic::domain::ir_curve_template_entry>& entries,
    const std::vector<ores::synthetic::domain::ir_curve_generation_config_process_parameter_value>&
        values,
    const std::vector<ores::synthetic::domain::yield_curve_process_parameter_definition>&
        definitions,
    const ir_curve_refdata_context& refctx,
    ores::analytics::quant::service::run_market& market,
    std::vector<ir_pillar_quote_target> adapter_targets,
    std::optional<ir_curve_start> start) {
    auto resolved = resolve(entries, refctx, cfg.fixed_leg_payment_frequency_code);

    // A curve that took a fixing starts its short rate there: the initial_rate value row is
    // replaced for this run, and the config keeps the rate it states.
    if (start && start->refused)
        throw std::runtime_error("Curve " + cfg.source_name +
                                 " did not start: " + start->source.note);
    auto run_values = values;
    if (start && start->rate) {
        for (const auto& d : definitions) {
            if (d.parameter_name != "initial_rate")
                continue;
            for (auto& v : run_values)
                if (v.parameter_definition_id == d.id)
                    v.parameter_value = *start->rate;
        }
    }

    // Read once, so the step the process is built with and the step the feed counts in agree
    // even when the speed changes while the feed is built.
    const double step_speed = market.loop().market_speed();
    const double step_years = curve_step_years(step_speed, static_cast<double>(cfg.ticks_per_hour));

    auto process = ores::synthetic::domain::map_parameters_to_yield_curve_process(
        cfg.process_type,
        definitions,
        run_values,
        price_stream_for(market.loop().run_seed(), cfg.source_name),
        step_years);

    // A scheme that only approximates its law states the largest step it can carry. A rate so
    // low that the step would exceed it is refused here, by name, rather than producing a path
    // the scheme was never calibrated for.
    if (step_years > process->max_step()) {
        throw std::invalid_argument(
            "make_ir_curve_feed: " + cfg.source_name + " at " + std::to_string(cfg.ticks_per_hour) +
            " ticks an hour asks the " + cfg.process_type + " scheme for a step of " +
            std::to_string(step_years * 365.0) + " days, and the largest it can take is " +
            std::to_string(process->max_step() * 365.0) +
            " days: raise the rate or lower the market speed");
    }

    // source_name is a persisted, editable column (see the field's own doc comment) -- the same
    // shape fx_spot_generation_config.source_name already uses, set at publish/save time rather
    // than computed here.
    const auto arrival = to_arrival_spec(cfg);
    const auto compatibility = ores::analytics::quant::domain::validate_step_compatibility(
        arrival.mode, process->supports_variable_step());
    if (!compatibility.valid)
        throw std::invalid_argument("make_ir_curve_feed: " + compatibility.message);

    // A run that took no fixing records the rate it kept, so its start states where the run
    // began even after the config's rate is edited.
    ores::marketdata::domain::feed_start record;
    if (start)
        record = std::move(start->source);
    if (!record.from_fixing) {
        for (const auto& d : definitions)
            if (d.parameter_name == "initial_rate")
                for (const auto& v : run_values)
                    if (v.parameter_definition_id == d.id)
                        if (const auto kept =
                                ores::utility::decimal::decimal::from_double(v.parameter_value))
                            record.value = *kept;
    }
    BOOST_LOG_SEV(ir_curve_feed_lg(), info)
        << "Curve " << cfg.source_name << " starts at "
        << (record.from_fixing ? record.value.to_string() + " from " + record.item + " (" +
                                     record.provider_code + ")" :
                                 record.value.to_string() + ", its fixed initial rate")
        << (record.note.empty() ? "" : ". " + record.note);

    auto feed = std::make_shared<ir_curve_feed>(nats,
                                                cfg.source_name,
                                                producer_subject(cfg.source_name),
                                                cfg.currency_code,
                                                ir_curve_index_tenor(cfg),
                                                cfg.role,
                                                std::move(process),
                                                arrival,
                                                std::move(resolved),
                                                market,
                                                step_speed,
                                                cfg.sensitivity,
                                                std::move(adapter_targets),
                                                refctx.spot);
    feed->record_start_source(std::move(record));
    return feed;
}

}
