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
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.synthetic.api/feeds/feed_factory.hpp"
#include "ores.synthetic.api/feeds/ir_curve_feed.hpp"
#include "ores.synthetic.api/feeds/producer_subject.hpp"
#include "ores.synthetic.api/feeds/vintage_lookup.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <format>
#include <stdexcept>

namespace ores::synthetic::feed {

using namespace ores::logging;

namespace {

auto& lg() {
    static auto instance = ores::logging::make_logger("ores.synthetic.api.fx_spot_feed");
    return instance;
}

// Resolves the initial price from a real market_observation when cfg.price_source is "vintage":
// the config's own series (from its oresmd_uri), keyed on (source=vintage_source, its own datum
// URI, date=vintage_date).
//
// @throws vintage_data_missing_error if no matching observation is found.
double resolve_vintage_initial_price(ores::nats::service::nats_client& auth_nats,
                                     const ores::synthetic::domain::fx_spot_generation_config& cfg,
                                     const std::string& start_date) {
    const auto missing_message = "No vintage data found for source=" + cfg.vintage_source +
                                 ", date=" + start_date + ", datum=" + cfg.oresmd_uri + ".";

    // The series and the row are found by the URIs the config's datum writes as: the
    // same codec the marketdata service's ingest loop applies to a tick's URI, so
    // the vintage read and the tick that follows it name one series.
    const auto datum = ores::marketdata::datum::oresmd_uri_codec::read(cfg.oresmd_uri);
    if (!datum)
        throw vintage_data_missing_error("oresmd URI '" + cfg.oresmd_uri + "': " + datum.error());
    const auto datum_uri = ores::marketdata::datum::oresmd_uri_codec::write(*datum).value();
    const auto series_uri =
        ores::marketdata::datum::oresmd_uri_codec::write(ores::marketdata::datum::series_of(*datum))
            .value();

    const auto found = find_vintage_observation(auth_nats,
                                                {},
                                                series_uri,
                                                datum_uri,
                                                cfg.vintage_source,
                                                start_date,
                                                missing_message,
                                                cfg.oresmd_uri,
                                                boost::uuids::to_string(cfg.party_id));
    if (!found)
        throw vintage_data_missing_error(found.error());
    return *found;
}

} // namespace

ORES_SYNTHETIC_API_EXPORT std::shared_ptr<fx_spot_feed>
make_fx_spot_feed(ores::nats::service::client& nats,
                  ores::nats::service::nats_client& auth_nats,
                  const ores::synthetic::domain::fx_spot_generation_config& cfg,
                  const std::vector<ores::synthetic::domain::gmm_component>& components,
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

    // "vintage" resolves the initial price from a real market_observation, overriding the stored
    // gmm_initial_price (0 for vintage configs, per the check constraint); "fixed" uses the
    // stored value as-is. See make_fx_spot_feed's doc comment for the vintage semantics.
    const double initial_price =
        cfg.price_source == "vintage" ?
            resolve_vintage_initial_price(
                auth_nats, cfg, ores::marketdata::domain::start_date(cfg.vintage_date, clock)) :
            cfg.gmm_initial_price;

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

    return std::make_shared<fx_spot_feed>(nats,
                                          cfg.oresmd_uri,
                                          cfg.source_name,
                                          producer_subject(cfg.source_name),
                                          std::move(process),
                                          arrival,
                                          scheduler,
                                          cfg.sensitivity,
                                          std::move(adapter));
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
                              .name = source_name_},
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
